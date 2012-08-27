{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Template.HSML.Internal.Types
( IsTemplate(..)
, Options(..)
, defaultOptions
, defaultOptionsS

, Template(..)
, Section(..)
, PAttribute(..)
, PAttributeName(..)
, PAttributeValue(..)

, Attribute
, AttributeName
, AttributeValue

, RAttribute
, RAttributeName
, RAttributeValue

, Arg(..)
, Dec
, Exp
, Type

, RArg(..)
, RDec
, RExp
) where

--------------------------------------------------------------------------------
import qualified Language.Haskell.Exts.Syntax as HE
--------------------------------------------------------------------------------
import           Data.Char   (toUpper, toLower)
import           Data.Monoid ((<>))
--------------------------------------------------------------------------------
import qualified Text.Blaze as B (Markup)
--------------------------------------------------------------------------------

-- | Template type-class.
class IsTemplate a where
    renderTemplate :: a -> B.Markup -- ^ Renders the template.


-- | Default settings for HSML generators.
--
-- > defaultOptions name = Options
-- >     { optExpToMarkup = True
-- >     , optExpToValue = True
-- >     , optTemplateName = firstUpper name
-- >     , optTemplateFieldName = \a -> firstLower name <> firstUpper a
-- >     }
-- >     where
-- >       firstUpper "" = ""
-- >       firstUpper (c:cs) = toUpper c : cs
-- > 
-- >       firstLower "" = ""
-- >       firstLower (c:cs) = toLower c : cs
defaultOptions :: String -> Options
defaultOptions name = Options
    { optExpToMarkup = True
    , optExpToValue = True
    , optTemplateName = firstUpper name
    , optTemplateFieldName = \a -> firstLower name <> firstUpper a
    }
    where
      firstUpper "" = ""
      firstUpper (c:cs) = toUpper c : cs

      firstLower "" = ""
      firstLower (c:cs) = toLower c : cs

-- | Default settings for Simplified HSML generators.
--
-- > defaultOptionsS = Options
-- >     { optExpToMarkup = True 
-- >     , optExpToValue = True
-- >     , optTemplateName = undefined
-- >     , optTemplateFieldName = undefined 
-- >     }
defaultOptionsS :: Options
defaultOptionsS = Options
    { optExpToMarkup = True 
    , optExpToValue = True
    , optTemplateName = undefined
    , optTemplateFieldName = undefined 
    }

-- | This type lets you customize some behaviour of HSML templates.
data Options = Options
    {
      -- | If and only if set to True, applies `Text.Blaze.toMarkup` on
      -- section expressions in your HSML templates.
      optExpToMarkup :: Bool

      -- | If and only if set to True, applies `Text.Blaze.toValue` on
      -- attribute value expressions in your HSML templates.
    , optExpToValue :: Bool

      -- | The name of the generated record.
      --
      -- NOTE: Has no effect on Simplified HSML templates.
    , optTemplateName :: String

      -- | The name of the fields of the genrated record.
      --
      -- NOTE: Has no effect on Simplified HTML templates.
    , optTemplateFieldName :: String -> String
    }

--------------------------------------------------------------------------------
-- | Handy synonyms

type Attribute      = PAttribute Exp
type AttributeName  = PAttributeName Exp
type AttributeValue = PAttributeValue Exp

type RAttribute      = PAttribute RExp
type RAttributeName  = PAttributeName RExp
type RAttributeValue = PAttributeValue RExp

--------------------------------------------------------------------------------
-- | The Template. This includes arguments, declarations and sections

data Template = Template
    { templateArgs :: [Arg]
    , templateDecs :: [Dec]
    , templateSections :: [Section]
    }

data Section = ElementNode
                 { nodeName ::  String
                 , noteAttributes ::  [Attribute]
                 , nodeBody :: [Section]
                 , nodeDecs :: [Dec]
                 }
             | ElementLeaf String [Attribute]
             | Text        String
             | TextRaw     String
             | Expression  Exp

data PAttribute exp = Attribute (PAttributeName exp) (PAttributeValue exp)

data PAttributeName exp = AttributeNameText String 
                        | AttributeNameExp  exp

data PAttributeValue exp = AttributeValueText String 
                         | AttributeValueExp  exp

data Arg = Arg String (Maybe Type)
type Dec = HE.Decl
type Exp = HE.Exp
type Type = HE.Type

data RArg = RArg String (Maybe String)
type RDec = String
type RExp = String

