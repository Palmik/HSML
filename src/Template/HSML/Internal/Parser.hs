{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}

module Template.HSML.Internal.Parser
#ifdef TESTING
where
#else
(  hsmlParser
, shsmlParser

,  hsmlTemplate
, shsmlTemplate
) where
#endif

------------------------------------------------------------------------------
import qualified Language.Haskell.Exts.Parser    as HE
import qualified Language.Haskell.Exts.Extension as HE
------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad
import           Control.Arrow
------------------------------------------------------------------------------
import           Data.Monoid ((<>))
------------------------------------------------------------------------------
import qualified Text.Parsec.String as P
import qualified Text.Parsec.Prim   as P
------------------------------------------------------------------------------
import qualified Template.HSML.Internal.Types         as I
import qualified Template.HSML.Internal.Types.Syntax  as IS
import qualified Template.HSML.Internal.Parser.Syntax as IS
------------------------------------------------------------------------------

hsmlTemplate :: String -> Either String I.Template
hsmlTemplate str = 
    case P.parse hsmlParser ".HSML" str of
        Right tpl -> Right tpl
        Left  err -> Left $ show err

hsmlParser :: P.Parser I.Template
hsmlParser = IS.hsmlSyntax >>= transform

shsmlTemplate :: String -> Either String I.Template
shsmlTemplate str =
    case P.parse shsmlParser ".HSML" str of
        Right tpl -> Right tpl
        Left  err -> Left $ show err 

shsmlParser :: P.Parser I.Template
shsmlParser = IS.shsmlSyntax >>= transform

------------------------------------------------------------------------------

transform :: IS.Syntax -> P.Parser I.Template
transform IS.Syntax{..} = do
    args <- mapM transformArg syntaxArgs
    (decs, sections) <- transformChunks syntaxChunks
    return I.Template
      { I.templateArgs = args
      , I.templateDecs = decs
      , I.templateSections = sections
      }

transformArg :: I.RArg -> P.Parser I.Arg
transformArg (I.RArg name mtype) =
    case mtype of
        Just stype ->
          case parseType stype of
              Right t -> return . I.Arg name $ Just t
              Left  r -> fail $ "Could not parse type \"" <> stype <> "\", reason: " <> r
        Nothing -> return $ I.Arg name Nothing

transformExp :: I.RExp -> P.Parser I.Exp
transformExp sexp =
    case parseExp sexp of
        Right e -> return e 
        Left  r -> fail $ "Could not parse expression \"" <> sexp <> "\", reason: " <> r

transformChunks :: [IS.Chunk] -> P.Parser ([I.Dec], [I.Section])
transformChunks chunks = (reverse *** reverse) <$> foldM transformChunk ([], []) chunks

transformChunk :: ([I.Dec], [I.Section]) -> IS.Chunk -> P.Parser ([I.Dec], [I.Section])
transformChunk (ds, ss) (IS.ElementNode name rattributes chunks) = do
    attributes <- mapM transformAttribute rattributes
    (decs, sections) <- transformChunks chunks
    return (ds, I.ElementNode name attributes sections decs : ss)
transformChunk (ds, ss) (IS.ElementLeaf name rattributes) = do
    attributes <- mapM transformAttribute rattributes
    return (ds, I.ElementLeaf name attributes : ss)
transformChunk (ds, ss) (IS.Text    text) = return (ds, I.Text text : ss)
transformChunk (ds, ss) (IS.TextRaw text) = return (ds, I.TextRaw text : ss)
transformChunk (ds, ss) (IS.Haskell rhs) = 
    case parseExp rhs of
        Right e -> return (ds, I.Expression e : ss)
        Left er -> 
          case parseDec rhs of
              Right d -> return (d : ds, ss)
              Left dr -> fail $ concat 
                [ "Could not parse haskell \"", rhs
                , "\", as expression, because: ", er
                , "; as declaration, because: ", dr
                ]

transformAttribute :: I.RAttribute -> P.Parser I.Attribute
transformAttribute (I.AttributeExp rexp) = I.AttributeExp <$> transformExp rexp 
transformAttribute (I.Attribute rname rvalue) =
    I.Attribute <$> transformName rname <*> transformValue rvalue 
    where
      transformName (I.AttributeNameExp rexp) = I.AttributeNameExp <$> transformExp rexp
      transformName (I.AttributeNameText text) = return $ I.AttributeNameText text

      transformValue (I.AttributeValueExp rexp) = I.AttributeValueExp <$> transformExp rexp
      transformValue (I.AttributeValueText text) = return $ I.AttributeValueText text

parseType :: String -> Either String I.Type
parseType = toEither . HE.parseTypeWithMode parseMode
{-# INLINE parseType #-}

parseExp :: String -> Either String I.Exp
parseExp = toEither . HE.parseExpWithMode parseMode
{-# INLINE parseExp #-}

parseDec :: String -> Either String I.Dec
parseDec = toEither . HE.parseDeclWithMode parseMode
{-# INLINE parseDec #-}

toEither :: HE.ParseResult a -> Either String a
toEither pr =
    case pr of
        HE.ParseOk     x   -> Right x
        HE.ParseFailed _ e -> Left e
{-# INLINE toEither #-}      

parseMode :: HE.ParseMode
parseMode = HE.defaultParseMode
  { HE.extensions = HE.TemplateHaskell : HE.QuasiQuotes : HE.haskell2010
  }
{-# INLINE parseMode #-}


