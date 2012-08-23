{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Template.HSML.Internal.Types
( HSMLTemplate(..)
, Options(..)
, defaultHSML
, defaultSHSML

, PTemplate(..)
, PSection(..)
, PAttribute(..)
, PAttributeName(..)
, PAttributeValue(..)

, Template
, Section
, Attribute
, AttributeName
, AttributeValue

, RTemplate
, RSection
, RAttribute
, RAttributeName
, RAttributeValue

, Arg(..)
, Dec(..)
, Exp(..)

, RArg(..)
, RDec(..)
, RExp(..)
) where

--------------------------------------------------------------------------------
import qualified Language.Haskell.Exts.Syntax as HE
--------------------------------------------------------------------------------
import           Data.Char   (toUpper, toLower)
import           Data.Monoid ((<>))
--------------------------------------------------------------------------------
import qualified Text.Blaze as B (Markup)
--------------------------------------------------------------------------------

class HSMLTemplate a where
    renderTemplate :: a -> B.Markup

defaultHSML :: String -> Options
defaultHSML name = Options
    { optSectionsToMarkup = True
    , optTemplateName = firstUpper name
    , optTemplateFieldName = \a -> firstLower name <> firstUpper a
    }
    where
      firstUpper "" = ""
      firstUpper (c:cs) = toUpper c : cs

      firstLower "" = ""
      firstLower (c:cs) = toLower c : cs

defaultSHSML :: Options
defaultSHSML = Options
    { optSectionsToMarkup = True 
    , optTemplateName = undefined
    , optTemplateFieldName = undefined 
    }

-- | This type let's you customize some behaviour of the templating system.
data Options = Options
    {
    -- ^ If (and only if) set to True, applies `Blaze.Text.toMarkup` to
    -- section expression
      optSectionsToMarkup :: Bool

    -- ^ The name of the generated record.
    -- NOTE: Has no effect on s.HSML templates.
    , optTemplateName :: String

    -- ^ The name of the fields of the genrated record.
    -- NOTE: Has no effect on sHTML templates.
    , optTemplateFieldName :: String -> String
    }

--------------------------------------------------------------------------------
-- | Handy synonyms

type Template       = PTemplate Arg Dec Exp
type Section        = PSection Exp
type Attribute      = PAttribute Exp
type AttributeName  = PAttributeName Exp
type AttributeValue = PAttributeValue Exp

type RTemplate    = PTemplate RArg RDec RExp
type RSection     = PSection  RExp
type RAttribute      = PAttribute RExp
type RAttributeName  = PAttributeName RExp
type RAttributeValue = PAttributeValue RExp

--------------------------------------------------------------------------------
-- | The Template. This includes arguments, declarations and sections

data PTemplate arg dec exp = Template
    { templateArgs :: [arg]
    , templateDecs :: [dec]
    , templateSections :: [PSection exp]
    }

instance Show e => Show (PTemplate a d e) where
    show tpl = unlines . map show $ templateSections tpl

data PSection exp = ElementNode String [PAttribute exp] [PSection exp]
                  | ElementLeaf String [PAttribute exp]
                  | Text        String
                  | TextRaw     String
                  | Expression  exp

instance Show e => Show (PSection e) where
    show (ElementNode name _ sections) = concat [ "<" <> name <> ">\n"
                                              ,  unlines (map show sections)
                                              , "</" <> name <> ">\n"
                                              ]
    show (ElementLeaf name _) = "<" <> name <> "/>"
    show (Text    str) = hsmlEscape str
    show (TextRaw str) = str
    show (Expression e) = show e

hsmlEscape :: String -> String
hsmlEscape "" = ""
hsmlEscape (c:rest) =
    if shouldEscape c
       then '\\' : c : hsmlEscape rest
       else c : hsmlEscape rest
    where
      shouldEscape = (`elem` "<{")        

data PAttribute exp = Attribute (PAttributeName exp) (PAttributeValue exp)

data PAttributeName exp = AttributeNameText String 
                        | AttributeNameExp  exp

data PAttributeValue exp = AttributeValueText String 
                         | AttributeValueExp  exp

data    Arg = Arg String (Maybe HE.Type)
newtype Dec = Dec HE.Decl
newtype Exp = Exp HE.Exp

data    RArg = RArg String (Maybe String) deriving Show
newtype RDec = RDec String deriving Show
newtype RExp = RExp String deriving Show

