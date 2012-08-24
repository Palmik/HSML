{-# LANGUAGE CPP #-}

module Template.HSML.Internal.Parser.Syntax
#ifdef TESTING
where
#else
(  hsmlSyntax
, shsmlSyntax
) where
#endif

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad
------------------------------------------------------------------------------
import           Data.Char
------------------------------------------------------------------------------
import qualified Text.Parsec.String     as P
import qualified Text.Parsec.Combinator as P
import qualified Text.Parsec.Char       as P
import qualified Text.Parsec.Prim       as P
------------------------------------------------------------------------------
import qualified Template.HSML.Internal.Types.Syntax as I
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- | Syntax

shsmlSyntax :: P.Parser I.Syntax
shsmlSyntax = do
    spaces
    chus <- P.try $ sepEndBy1 chunk spaces
    P.eof
    return $ I.Syntax [] chus

hsmlSyntax :: P.Parser I.Syntax
hsmlSyntax = do
    spaces
    args <- P.try $ sepEndBy argument spaces
    spaces
    chus <- P.try $ sepEndBy1 chunk spaces
    P.eof
    return $ I.Syntax args chus

--------------------------------------------------------------------------------
-- | Chunk
chunk :: P.Parser I.Chunk
chunk = P.try ( P.try elementNode <|>
                P.try elementLeaf <|>
                P.try haskell     <|>
                P.try textRaw     <|>
                text  
              )

--------------------------------------------------------------------------------
-- | Argument
argument :: P.Parser I.RArg
argument = P.try $ do
    P.string "{a|" >> spaces
    aname <- argName
    atype <- maybeParser $ P.spaces >> P.string "::" >> spaces >> typeName 
    spaces >> P.string "|}" >> spaces
    return $ I.RArg aname atype

argName :: P.Parser String
argName = (:) <$> P.lower <*> P.many P.alphaNum

typeName :: P.Parser String
typeName = (:) <$> P.upper <*> P.many (P.satisfy (/= ' '))

--------------------------------------------------------------------------------
-- | Element Node
elementNode :: P.Parser I.Chunk
elementNode = P.try $ do
    (name, attributes) <- tagOpening <* P.string ">"
    body <- many chunk 
    tagClosing name 
    return $ I.ElementNode name attributes body

--------------------------------------------------------------------------------
-- | Element Leaf
elementLeaf :: P.Parser I.Chunk
elementLeaf = P.try $ do
    (name, attributes) <- tagOpening <* P.string "/>"
    return $ I.ElementLeaf name attributes

tagOpening :: P.Parser (String, [I.RAttribute]) 
tagOpening = P.try $ do
    P.char '<' >> spaces
    name <- tagName
    spaces
    attributes <- sepEndBy attribute spaces
    return (name, attributes)

tagClosing :: String -> P.Parser ()
tagClosing name = P.try . void $ 
    P.string "</" *> spaces *> P.string name <* spaces <* P.string ">"

tagName :: P.Parser String
tagName = P.try $ P.many1 P.alphaNum

--------------------------------------------------------------------------------
-- | Attribute
attribute :: P.Parser I.RAttribute
attribute = do
    n <- attributeName
    spaces >> P.char '=' >> spaces
    v <- attributeValue
    return $ I.Attribute n v

attributeValue :: P.Parser I.RAttributeValue
attributeValue = P.try $
    P.try attributeValueText <|>
          attributeValueExp

attributeValueText :: P.Parser I.RAttributeValue
attributeValueText =
    P.char '\"'
    *> (I.AttributeValueText <$> P.many1 valueChar) <*
    P.char '\"'
    where
      valueChar = P.satisfy (\c -> isAlphaNum c || c `elem` " -_")

attributeValueExp :: P.Parser I.RAttributeValue
attributeValueExp = I.AttributeValueExp <$> haskellBody

attributeName :: P.Parser I.RAttributeName
attributeName = P.try $
    P.try attributeNameText <|>
          attributeNameExp  

attributeNameText :: P.Parser I.RAttributeName
attributeNameText = I.AttributeNameText <$> P.many1 nameChar 
    where
      nameChar = P.satisfy (\c -> isAlphaNum c || c `elem` "-_")

attributeNameExp :: P.Parser I.RAttributeName
attributeNameExp = I.AttributeNameExp <$> haskellBody

--------------------------------------------------------------------------------
-- | Text 
text :: P.Parser I.Chunk
text = P.try $ I.Text <$> P.many1 myChar
    where
      myChar = escapedChar <|> P.satisfy (\c -> c `notElem` "<{" && c /= '\\')
      {-# INLINE myChar #-}

--------------------------------------------------------------------------------
-- | Text Raw
textRaw :: P.Parser I.Chunk
textRaw = P.try $
    P.string "{r|"
    *> (I.TextRaw <$> manyUntil myChar (P.string "|}")) <*
    P.string "|}"
    where
      myChar = escapedChar <|> P.anyChar
      {-# INLINE myChar #-}

--------------------------------------------------------------------------------
-- | Haskell
haskell :: P.Parser I.Chunk
haskell = I.Haskell <$> haskellBody

haskellBody :: P.Parser String
haskellBody = P.try (P.string "{h|" *> manyUntil myChar (P.string "|}") <* P.string "|}")
  where
    myChar = P.anyChar
    {-# INLINE myChar #-}

--------------------------------------------------------------------------------
-- CONVENIENCE FUNCTIONS

escapedChar :: P.Parser Char
escapedChar = P.try $ P.char '\\' >> P.anyChar       
{-# INLINE escapedChar #-} 

maybeParser :: P.Parser a -> P.Parser (Maybe a)
maybeParser p = (Just <$> P.try p) <|> return Nothing

spaces :: P.Parser ()
spaces = void $ P.many P.space

-- spaces1 :: P.Parser ()
-- spaces1 = void $ P.many1 P.space

sepEndBy1 :: P.Parser a -> P.Parser b -> P.Parser [a]
sepEndBy1 p sep = P.try $ do
    x <- p
    P.try ((x:) <$> (sep >> sepEndBy p sep)) <|> return [x]

sepEndBy :: P.Parser a -> P.Parser b -> P.Parser [a]
sepEndBy p sep = P.try (sepEndBy1 p sep) <|> return []

manyUntil :: P.Parser a -> P.Parser b -> P.Parser [a]
manyUntil p stop = P.try $ P.manyTill p (P.lookAhead $ void (P.try stop) <|> void P.eof)

{-
many1Until :: P.Parser a -> P.Parser b -> P.Parser [a]
many1Until p stop = P.try $ do
    x  <- p
    xs <- manyUntil p stop
    return $ x : xs

peekChar :: P.Parser (Maybe Char)
peekChar = P.try (Just <$> P.lookAhead P.anyChar) <|> return Nothing
-}

