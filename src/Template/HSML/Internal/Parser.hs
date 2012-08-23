{-# LANGUAGE RecordWildCards #-}

module Template.HSML.Internal.Parser
(  hsmlParser
, shsmlParser

,  hsmlTemplate
, shsmlTemplate
) where

------------------------------------------------------------------------------
import qualified Language.Haskell.Exts.Parser    as HE
-- import qualified Language.Haskell.Exts.Extension as HE
-- import qualified Language.Haskell.Exts.Syntax    as HE
------------------------------------------------------------------------------
import           Control.Applicative
------------------------------------------------------------------------------
import           Data.Monoid ((<>))
------------------------------------------------------------------------------
import qualified Text.Parsec.String as P
import qualified Text.Parsec.Prim   as P
------------------------------------------------------------------------------
import qualified Template.HSML.Internal.Types      as I
import qualified Template.HSML.Internal.Parser.Raw as I
------------------------------------------------------------------------------

hsmlTemplate :: String -> Either String I.Template
hsmlTemplate str = 
    case P.parse hsmlParser ".HSML" str of
        Right tpl -> Right tpl
        Left  err -> Left $ show err

hsmlParser :: P.Parser I.Template
hsmlParser = I.hsmlParser >>= transform

shsmlTemplate :: String -> Either String I.Template
shsmlTemplate str =
    case P.parse shsmlParser ".HSML" str of
        Right tpl -> Right tpl
        Left  err -> Left $ show err 

shsmlParser :: P.Parser I.Template
shsmlParser = I.shsmlParser >>= transform

------------------------------------------------------------------------------

transform :: I.RTemplate -> P.Parser I.Template
transform I.Template{..} = do
    args <- mapM transformArg templateArgs
    decs <- mapM transformDec templateDecs
    sections <- mapM transformSection templateSections
    return I.Template
      { I.templateArgs = args
      , I.templateDecs = decs
      , I.templateSections = sections
      }

transformArg :: I.RArg -> P.Parser I.Arg
transformArg (I.RArg name mtype) =
    case mtype of
        Just stype ->
          case HE.parseType stype of
              HE.ParseOk t       -> return . I.Arg name $ Just t
              HE.ParseFailed _ r -> fail $ "Could not parse type \"" <> stype <> "\", reason: " <> r
        Nothing -> return $ I.Arg name Nothing

transformDec :: I.RDec -> P.Parser I.Dec
transformDec (I.RDec sdec) = 
    case HE.parseDecl sdec of
        HE.ParseOk d       -> return $ I.Dec d
        HE.ParseFailed _ r -> fail $ "Could not parse declaration \"" <> sdec <> "\", reason: " <> r

transformExp :: I.RExp -> P.Parser I.Exp
transformExp (I.RExp sexp) =
    case HE.parseExp sexp of
        HE.ParseOk e       -> return $ I.Exp e 
        HE.ParseFailed _ r -> fail $ "Could not parse expression \"" <> sexp <> "\", reason: " <> r

transformSection :: I.RSection -> P.Parser I.Section
transformSection (I.ElementNode name rattributes rsections) = do
    attributes <- mapM transformAttribute rattributes
    sections   <- mapM transformSection rsections
    return $ I.ElementNode name attributes sections
transformSection (I.ElementLeaf name rattributes) = do
    attributes <- mapM transformAttribute rattributes
    return $ I.ElementLeaf name attributes
transformSection (I.Text    text) = return $ I.Text    text
transformSection (I.TextRaw text) = return $ I.TextRaw text
transformSection (I.Expression rexp) = I.Expression <$> transformExp rexp

transformAttribute :: I.RAttribute -> P.Parser I.Attribute
transformAttribute (I.Attribute rname rvalue) =
    I.Attribute <$> transformName rname <*> transformValue rvalue 
    where
      transformName (I.AttributeNameExp rexp) = I.AttributeNameExp <$> transformExp rexp
      transformName (I.AttributeNameText text) = return $ I.AttributeNameText text

      transformValue (I.AttributeValueExp rexp) = I.AttributeValueExp <$> transformExp rexp
      transformValue (I.AttributeValueText text) = return $ I.AttributeValueText text

