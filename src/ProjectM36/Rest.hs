module ProjectM36.Rest (run) where

import Control.Exception (bracket)
import Control.Monad.IO.Class
import Data.Aeson hiding (Options)
import Data.Aeson qualified as A
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types qualified as A
import Data.ByteString.Base64 (decodeBase64)
import Data.Function
import Data.Functor
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Traversable
import Data.Vector qualified as V
import Debug.Trace
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp qualified as Warp
import Options.Applicative
import ProjectM36.Base
import ProjectM36.Client
import Servant
import Servant.Server.Generic

data Options = Options
  { dir :: FilePath,
    port :: Warp.Port
  }

options :: Parser Options
options =
  Options
    <$> strOption (short 'd' <> long "dir" <> help "Path to where project-m36 should store data." <> value "m36")
    <*> option auto (short 'p' <> long "port" <> help "Port on which the HTTP server should run" <> value 6543)

data Query
  = DefineQuery RelVarName [AttributeExpr]
  | InsertQuery RelVarName RelationalExpr

instance FromJSON Query where
  parseJSON = withObject "Query" $ \o -> case KeyMap.toList o of
    [("define", def)] -> flip (withObject "Definition") def $ \d -> do
      name <- d .: "name"
      attributes <- (d .: "attrs") >>= decodeRelationType
      pure $ DefineQuery name (NakedAttributeExpr <$> V.toList attributes)
    [("insert", ins)] -> flip (withObject "Insert") ins $ \i -> do
      name <- i .: "name"
      rel <- i .: "rel" >>= decodeExpr
      pure $ InsertQuery name rel
    _ -> fail "Poorly formed query key"

decodeRelationType :: A.Array -> A.Parser (V.Vector Attribute)
decodeRelationType o =
  traverse parseJSON o
    >>= traverse
      ( \(key, v) ->
          Attribute (Key.toText key) <$> case v of
            A.String "int" -> pure IntAtomType
            A.String "integer" -> pure IntegerAtomType
            A.String "scientific" -> pure ScientificAtomType
            A.String "double" -> pure DoubleAtomType
            A.String "text" -> pure TextAtomType
            A.String "day" -> pure DayAtomType
            A.String "datetime" -> pure DateTimeAtomType
            A.String "bytestring" -> pure ByteStringAtomType
            A.String "bool" -> pure BoolAtomType
            A.String "uuid" -> pure UUIDAtomType
            A.Array x -> RelationAtomType . Attributes <$> decodeRelationType x
            _ -> fail $ "unknown type: " <> show v
      )

decodeExpr :: A.Object -> A.Parser RelationalExpr
decodeExpr o = case KeyMap.toList (traceShowId o) of
  [("lit", A.Object lit)] -> do
    (traceShowId -> !attrs) <- lit .: "attrs" >>= decodeRelationType
    (traceShowId -> !vals) :: [V.Vector Value] <- lit .: "vals"
    tuples <- for vals $ \rel ->
      traverse
        (uncurry decodeAtom)
        (V.zip ((\(Attribute _ atomType) -> atomType) <$> attrs) rel)
    pure $
      MakeStaticRelation
        (Attributes attrs)
        (RelationTupleSet $ RelationTuple (Attributes attrs) <$> tuples)
  _ -> fail $ "unknown expression: " <> show o

decodeAtom :: AtomType -> A.Value -> A.Parser Atom
decodeAtom (traceShowId -> t) (traceShowId -> v) = case t of
  IntAtomType -> IntAtom <$> parseJSON v
  IntegerAtomType -> IntegerAtom <$> parseJSON v
  ScientificAtomType -> ScientificAtom <$> parseJSON v
  DoubleAtomType -> DoubleAtom <$> parseJSON v
  TextAtomType -> TextAtom <$> parseJSON v
  DayAtomType -> DayAtom <$> parseJSON v
  DateTimeAtomType -> DateTimeAtom <$> parseJSON v
  ByteStringAtomType ->
    ByteStringAtom <$> do
      b64 <- T.encodeUtf8 <$> parseJSON @Text v
      case decodeBase64 b64 of
        Right x -> pure x
        Left err -> fail $ T.unpack err
      pure undefined
  BoolAtomType -> BoolAtom <$> parseJSON v
  UUIDAtomType -> UUIDAtom <$> parseJSON v
  RelationAtomType (Attributes attrs) -> do
    rels <- parseJSON @[(V.Vector Value)] v
    decodedRels <- for rels $ \rel ->
      for (V.zip attrs rel) $ \(Attribute _ t', a) ->
        decodeAtom t' a
    pure $
      RelationAtom $
        Relation (Attributes attrs) $
          RelationTupleSet $
            decodedRels <&> \rel -> RelationTuple (Attributes attrs) rel
  _ -> fail $ "Unsupported atom type: " <> show t

data API r = API
  { startTransaction :: r :- "transaction" :> Post '[JSON] SessionId,
    commit :: r :- "transaction" :> Capture "SessionId" SessionId :> PutNoContent,
    abort :: r :- "transaction" :> Capture "SessionId" SessionId :> DeleteNoContent,
    query :: r :- "transaction" :> Capture "SessionId" SessionId :> ReqBody '[JSON] Query :> Post '[JSON] [A.Value]
  }
  deriving stock (Generic)

server :: Connection -> API AsServer
server conn =
  API
    { startTransaction =
        liftIO (createSessionAtHead conn defaultHeadName) >>= rethrow,
      commit = \sess -> (liftIO (autoMergeToHead sess conn UnionMergeStrategy defaultHeadName) >>= rethrow) $> NoContent,
      abort = \sess -> liftIO (closeSession sess conn) $> NoContent,
      query = \sess -> \case
        DefineQuery varName attrs -> do
          liftIO (executeDatabaseContextExpr sess conn (Define varName attrs)) >>= rethrow
          pure []
        InsertQuery varName rel -> do
          liftIO (executeDatabaseContextExpr sess conn (Insert varName rel)) >>= rethrow
          pure []
    }

rethrow :: (Show err) => Either err x -> Handler x
rethrow = \case
  Left err -> throwError err400 {errReasonPhrase = show err}
  Right x -> pure x

run :: IO ()
run = do
  opts <- execParser $ info (helper <*> options) fullDesc
  bracket
    ( connectProjectM36
        (InProcessConnectionInfo (CrashSafePersistence opts.dir) emptyNotificationCallback [] basicDatabaseContext)
        >>= \case
          Left err -> error $ show err
          Right x -> pure x
    )
    close
    ( \conn ->
        Warp.runSettings
          (Warp.defaultSettings & Warp.setServerName "project-m36-rest" & Warp.setPort opts.port)
          (genericServe (server conn))
    )
