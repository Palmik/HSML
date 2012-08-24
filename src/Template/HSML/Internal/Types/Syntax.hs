module Template.HSML.Internal.Types.Syntax
( Syntax(..)
, Chunk(..)

, I.RArg(..)
, I.RExp
, I.RDec
, I.PAttribute(..)
, I.RAttribute
, I.PAttributeName(..)
, I.RAttributeName
, I.PAttributeValue(..)
, I.RAttributeValue
) where

------------------------------------------------------------------------------
import qualified Template.HSML.Internal.Types as I
------------------------------------------------------------------------------

data Syntax = Syntax
    { syntaxArgs   :: [I.RArg]
    , syntaxChunks :: [Chunk]
    }

data Chunk = ElementNode String [I.RAttribute] [Chunk]
           | ElementLeaf String [I.RAttribute]
           | Text        String
           | TextRaw     String
           | Haskell     String

