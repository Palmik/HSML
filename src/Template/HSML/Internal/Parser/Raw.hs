module Template.HSML.Internal.Parser.Raw
(  hsmlParser
, shsmlParser
) where

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
import qualified Template.HSML.Internal.Types as I
------------------------------------------------------------------------------

-- | .HSML parser.
hsmlParser:: P.Parser I.RTemplate
hsmlParser = do
   spaces 
   args <- P.try $ sepEndBy argument spaces
   chus <- P.try $ sepEndBy1 chunk spaces
   P.eof
   return . templateFromSyntax $ Syntax args chus

-- | Simple .HSML parser (argument are not allowed).
shsmlParser :: P.Parser I.RTemplate
shsmlParser = do
    spaces
    chus <- P.try $ sepEndBy1 chunk spaces 
    P.eof
    return . templateFromSyntax $ Syntax [] chus

--------------------------------------------------------------------------------

templateFromSyntax :: Syntax -> I.RTemplate
templateFromSyntax (Syntax args chunks) =
    I.Template args decs secs
    where
      (decs, secs) = foldr go ([], []) chunks
      go (Dec d) (ds, ss) = (d : ds, ss)
      go (Sec s) (ds, ss) = (ds, s : ss) 

--------------------------------------------------------------------------------

chunk :: P.Parser Chunk
chunk = Dec <$> declaration <|>
        Sec <$> section

--------------------------------------------------------------------------------
-- | Declaration

-- | Raw declaration conforms to the following:
-- "{a|" <spaces> <funIdent> [ <spaces> "::" <spaces> <typeIdent> ] <spaces> "|}"
argument :: P.Parser I.RArg
argument = do
    P.string "{a|" >> spaces
    aname <- funIdent
    atype <- maybeParser $ P.spaces >> P.string "::" >> spaces >> typeIdent 
    spaces >> P.string "|}" >> spaces
    return $ I.RArg aname atype

--------------------------------------------------------------------------------
-- | Declaration

-- | Raw declaration conforms to the following:
-- "{d|" <string without "|}" substring> "|}"
declaration :: P.Parser I.RDec
declaration = I.RDec <$> wrappedText 'd' P.anyChar 

--------------------------------------------------------------------------------
-- | Section

section :: P.Parser I.RSection
section = P.try ( elementNode <|>
                  elementLeaf <|>
                  expression  <|>
                  textRaw     <|>
                  text 
                )

--------------------------------------------------------------------------------
-- | Elements

elementNode :: P.Parser I.RSection
elementNode = P.try $ do
    (name, attributes) <- tagOpening <* P.string ">"
    body <- many section 
    tagClosing name 
    return $ I.ElementNode name attributes body

elementLeaf :: P.Parser I.RSection
elementLeaf = P.try $ do
    (name, attributes) <- tagOpening <* P.string "/>"
    return $ I.ElementLeaf name attributes

attribute :: P.Parser I.RAttribute
attribute = do
    n <- attributeName
    spaces >> P.char '=' >> spaces
    v <- attributeValue
    return $ I.Attribute n v

attributeName :: P.Parser I.RAttributeName
attributeName = P.try $
    P.try (I.AttributeNameText <$> attributeIdent) <|>
          (I.AttributeNameExp . I.RExp <$> expressionText)  

attributeValue :: P.Parser I.RAttributeValue
attributeValue = P.try $
    P.try (P.char '\"' *> (I.AttributeValueText <$> attributeIdent)  <* P.char '\"') <|>
          (I.AttributeValueExp . I.RExp <$> expressionText)

attributeIdent :: P.Parser String
attributeIdent = P.many1 $ P.satisfy ((||) <$> isAlphaNum <*> (`elem` " -_"))

--------------------------------------------------------------------------------
-- | Text 

text :: P.Parser I.RSection
text = P.try $ I.Text <$> P.many1 (textChar '\\' (`elem` "<{"))

textRaw :: P.Parser I.RSection
textRaw = I.TextRaw <$> wrappedText 'r' (textChar '\\' (`elem` "<{")) 

textChar :: Char -> (Char -> Bool) -> P.Parser Char
textChar esc needEsc = escapedChar <|> P.satisfy (\c -> not (needEsc c || c == esc))
    where      
      escapedChar = P.try $ P.char esc >> P.anyChar       

--------------------------------------------------------------------------------
-- | Expression

-- | Raw expression conforms to the following:
-- "{e|" <string without "|}" substring> "|}"
expression :: P.Parser I.RSection
expression = I.Expression . I.RExp <$> expressionText

--------------------------------------------------------------------------------

tagOpening :: P.Parser (String, [I.RAttribute]) 
tagOpening = P.try $ do
    P.char '<' >> spaces
    name <- tagIdentifier
    spaces
    attributes <- sepEndBy attribute spaces
    return (name, attributes)

tagClosing :: String -> P.Parser ()
tagClosing tagName = P.try . void $ do
    P.string "</"
    spaces
    P.string tagName
    spaces
    P.char '>'

tagIdentifier :: P.Parser String
tagIdentifier = P.try $ P.many1 P.alphaNum

--------------------------------------------------------------------------------

expressionText :: P.Parser String
expressionText = wrappedText 'e' P.anyChar

wrappedText :: Char -> P.Parser Char -> P.Parser String
wrappedText m p = P.try $
    P.string ['{', m, '|'] *> many1Until p (P.string "|}") <* P.string "|}"

funIdent :: P.Parser String
funIdent = (:) <$> P.lower <*> P.many P.alphaNum

typeIdent :: P.Parser String
typeIdent = (:) <$> P.upper <*> P.many (P.satisfy (/= ' '))

--------------------------------------------------------------------------------

data Syntax = Syntax [I.RArg] [Chunk]

data Chunk = Dec I.RDec
           | Sec I.RSection

--------------------------------------------------------------------------------
-- CONVENIENCE FUNCTIONS

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

many1Until :: P.Parser a -> P.Parser b -> P.Parser [a]
many1Until p stop = P.try $ do
    x  <- p
    xs <- manyUntil p stop
    return $ x : xs

