module ProjectM36.Rest (run) where

import Control.Exception (bracket)
import Control.Monad.IO.Class
import Data.Aeson hiding (Options)
import Data.Aeson qualified as A
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types qualified as A
import Data.Bifunctor (Bifunctor (..))
import Data.ByteString.Base64 (decodeBase64, encodeBase64)
import Data.Function
import Data.Functor
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Traversable
import Data.Vector qualified as V
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Logger (withStdoutLogger)
import Options.Applicative
import ProjectM36.Base
import ProjectM36.Client
import ProjectM36.Relation (relationTrue)
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
  | QueryQuery RelationalExpr

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
    [("query", qry)] -> QueryQuery <$> decodeExpr qry
    _ -> fail "Poorly formed query key"

encodeRelationType :: V.Vector Attribute -> A.Value
encodeRelationType =
  A.Array
    . fmap
      ( \(Attribute key t) ->
          toJSON . (key,) $ case t of
            IntAtomType -> A.String "int"
            IntegerAtomType -> A.String "integer"
            ScientificAtomType -> A.String "scientific"
            DoubleAtomType -> A.String "double"
            TextAtomType -> A.String "text"
            DayAtomType -> A.String "day"
            DateTimeAtomType -> A.String "datetime"
            ByteStringAtomType -> A.String "bytestring"
            BoolAtomType -> A.String "bool"
            UUIDAtomType -> A.String "uuid"
            RelationAtomType (Attributes attrs) -> encodeRelationType attrs
            _ -> error $ "unsupported atom type: " <> show t
      )

encodeAtom :: Atom -> A.Value
encodeAtom = \case
  IntegerAtom a -> toJSON a
  IntAtom a -> toJSON a
  ScientificAtom a -> toJSON a
  DoubleAtom a -> toJSON a
  TextAtom a -> toJSON a
  DayAtom a -> toJSON a
  DateTimeAtom a -> toJSON a
  ByteStringAtom a -> toJSON $ encodeBase64 a
  BoolAtom a -> toJSON a
  UUIDAtom a -> toJSON a
  RelationAtom a -> encodeRelation a
  a -> error $ "Unsupported atom type: " <> show a

encodeRelation :: Relation -> A.Value
encodeRelation (Relation (Attributes attrs) (RelationTupleSet tuples)) =
  A.object
    [ "attrs" A..= fmap (\(Attribute key _) -> key) attrs,
      "vals" A..= (tuples <&> (\(RelationTuple _ atoms) -> encodeAtom <$> atoms))
    ]

decodeRelationType :: V.Vector (Text, A.Value) -> A.Parser (V.Vector Attribute)
decodeRelationType =
  traverse
    ( \(key, v) ->
        Attribute key <$> case v of
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
          x -> parseJSON x >>= fmap (RelationAtomType . Attributes) . decodeRelationType
    )

decodeExpr :: A.Value -> A.Parser RelationalExpr
decodeExpr = withObject "Expr" $ \o -> case KeyMap.toList o of
  [("lit", A.Object lit)] -> do
    attrs <- lit .: "attrs" >>= decodeRelationType
    vals :: [V.Vector Value] <- lit .: "vals"
    tuples <- for vals $ \rel ->
      traverse
        (uncurry decodeAtom)
        (V.zip ((\(Attribute _ atomType) -> atomType) <$> attrs) rel)
    pure $
      MakeStaticRelation
        (Attributes attrs)
        (RelationTupleSet $ RelationTuple (Attributes attrs) <$> tuples)
  [("join", A.Array (V.toList -> [x, y]))] -> Join <$> decodeExpr x <*> decodeExpr y
  [("union", A.Array (V.toList -> [x, y]))] -> Union <$> decodeExpr x <*> decodeExpr y
  [("proj", A.Object p)] -> do
    proj <- AttributeNames <$> p .: "proj"
    expr <- p .: "expr" >>= decodeExpr
    pure $ Project proj expr
  [("rename", A.Object r)] -> do
    mapping <- r .: "mapping" <&> Set.fromList . fmap (first Key.toText) . KeyMap.toList
    expr <- r .: "expr" >>= decodeExpr
    pure $ Rename mapping expr
  [("diff", A.Array (V.toList -> [x, y]))] -> Difference <$> decodeExpr x <*> decodeExpr y
  [("group", A.Object g)] -> do
    attrs <- AttributeNames <$> g .: "attrs"
    attr <- g .: "attr"
    expr <- g .: "expr" >>= decodeExpr
    pure $ Group attrs attr expr
  [("ungroup", A.Object u)] -> do
    attr <- u .: "attr"
    expr <- u .: "expr" >>= decodeExpr
    pure $ Ungroup attr expr
  [("var", A.String v)] -> pure $ RelationVariable v ()
  _ -> fail $ "unknown expression: " <> show o

decodeAtom :: AtomType -> A.Value -> A.Parser Atom
decodeAtom t v = case t of
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
    query :: r :- "transaction" :> Capture "SessionId" SessionId :> ReqBody '[JSON] Query :> Post '[JSON] A.Value
  }
  deriving stock (Generic)

server :: Connection -> API AsServer
server conn =
  API
    { startTransaction =
        liftIO (createSessionAtHead conn defaultHeadName) >>= rethrow,
      commit = \sess -> (liftIO (autoMergeToHead sess conn UnionMergeStrategy defaultHeadName) >>= rethrow) $> NoContent,
      abort = \sess -> liftIO (closeSession sess conn) $> NoContent,
      query = \sess q ->
        encodeRelation <$> case q of
          DefineQuery varName attrs -> do
            liftIO (executeDatabaseContextExpr sess conn (Define varName attrs)) >>= rethrow
            pure relationTrue
          InsertQuery varName rel -> do
            liftIO (executeDatabaseContextExpr sess conn (Insert varName rel)) >>= rethrow
            pure relationTrue
          QueryQuery rel -> liftIO (executeRelationalExpr sess conn rel) >>= rethrow
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
    ( \conn -> withStdoutLogger $ \logger ->
        Warp.runSettings
          ( Warp.defaultSettings
              & Warp.setServerName "project-m36-rest"
              & Warp.setPort opts.port
              & Warp.setLogger logger
          )
          (genericServe (server conn))
    )
