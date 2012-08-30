{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Template.HSML.Internal.TH
#ifdef TESTING
where
#else
( -- * Quasi Quoters for Simplified HSML
  hsml
, m

, hsmlStringWith
, hsmlString
, hsmlFileWith
, hsmlFile

, shsmlStringWith
, shsmlString
, shsmlFileWith
, shsmlFile
) where
#endif

--------------------------------------------------------------------------------
import qualified Language.Haskell.TH        as TH
import qualified Language.Haskell.TH.Quote  as TH
import qualified Language.Haskell.TH.Syntax as TH
import qualified Language.Haskell.Meta.Syntax.Translate as HE
--------------------------------------------------------------------------------
import qualified Text.Blaze          as B
import qualified Text.Blaze.Internal as B
--------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Arrow
import           Control.Monad
--------------------------------------------------------------------------------
import           Data.String
import           Data.Monoid
--------------------------------------------------------------------------------
import qualified Template.HSML.Internal.Types  as I
import qualified Template.HSML.Internal.Parser as I
--------------------------------------------------------------------------------

-- | QuasiQuoter for Simplified HSML expressions with default options. See
-- `Template.HSML.Internal.Types.defaulOptionsS` for details. 
--
-- Example:
--
-- > example :: Blaze.Text.Markup
-- > example = [hsml|
-- >   <h1>Page Title</h1>
-- >   <p>
-- >     Some interesting paragraph.
-- >   </p>
-- >   |]
hsml :: TH.QuasiQuoter
hsml = TH.QuasiQuoter 
    { TH.quoteExp  = shsmlString  
    , TH.quoteDec  = const $ fail "You can not use the HSML QuasiQuoter as a declaration."
    , TH.quotePat  = const $ fail "You can not use the HSML QuasiQuoter as a pattern."
    , TH.quoteType = const $ fail "You can not use the HSML QuasiQuoter as a type."
    }
{-# INLINE hsml #-}

-- | The same as `Templahe.HSML.Internal.TH.hsml`.
--
-- Example:
--
-- > example :: Blaze.Text.Markup
-- > example = [m|
-- >   <h1>Page Title</h1>
-- >   <p>
-- >     Some interesting paragraph.
-- >   </p>
-- >   |]
m :: TH.QuasiQuoter
m = hsml
{-# INLINE m #-}

--------------------------------------------------------------------------------
-- HSML

-- These functions parse the given file or string as a HSML document.
-- If possible, splicing these generates a record field type and its instance of
-- `Template.HSML.Internal.Types.IsTemplate`.

-- | Parses HSML document from file with the given options. Results in
-- record type and its `Template.HSML.Internal.Types.IsTemplate` instance.
--
-- Example:
--
-- > $(hsmlFileWith (defaultOptions "MyTemplate") "my_template.hsml")
hsmlFileWith :: I.Options     -- ^ HSML options.
             -> FilePath      -- ^ Name of input file.
             -> TH.Q [TH.Dec] -- ^ Resulting type declaration and type-class instance.
hsmlFileWith opts path = TH.runIO (readFile path) >>= hsmlStringWith opts
{-# INLINE hsmlFileWith #-}

-- | Parses HSML document from file with default options. Results in
-- record type and its `Template.HSML.Internal.Types.IsTemplate` instance.
--
-- Example:
--
-- > $(hsmlFile "MyTemplate" "my_template.hsml")
hsmlFile :: String        -- ^ Name of the record type (passed to `Template.HSML.Internal.Types.defaultOptions`).
         -> FilePath      -- ^ Name of input file.
         -> TH.Q [TH.Dec] -- ^ Resulting type declaration and type-class instance.
hsmlFile = hsmlFileWith . I.defaultOptions
{-# INLINE hsmlFile #-}

-- | Parses HSML document string with the given options. Results in
-- record type and its `Template.HSML.Internal.Types.IsTemplate` instance.
--
-- Example:
--
-- > $(hsmlStringWith (defaultOptions "MyTemplate") "<p>Paragraph</p>")
hsmlStringWith :: I.Options     -- ^ HSML options.
               -> String        -- ^ Input string.
               -> TH.Q [TH.Dec] -- ^ Resulting type declaration and type-class instance.
hsmlStringWith opts str = 
    case I.hsmlTemplate str of
        Right tpl -> makeDec opts tpl
        Left  err -> fail err
{-# INLINE hsmlStringWith #-}

-- | Parses HSML document from string with default options. Results in
-- record type and its `Template.HSML.Internal.Types.IsTemplate` instance.
--
-- Example:
--
-- > $(hsmlString "MyTemplate" "<p>Paragraph</p>")
hsmlString :: String        -- ^ Name of the record type (passed to `Template.HSML.Internal.Types.defaultOptions`). 
           -> String        -- ^ Input string.
           -> TH.Q [TH.Dec] -- ^ Resulting type declaration and type-class instance.
hsmlString = hsmlStringWith . I.defaultOptions
{-# INLINE hsmlString #-}

--------------------------------------------------------------------------------
-- Simplified HSML (without arguments)

-- These functions parse the given file or string as a Simplified HSML document.
-- If possible, splicing these results in an expression of the type
-- `Text.Blaze.Markup`.

-- | Parses Simplified HSML document from file with the given options. Results in
-- expression of type `Text.Blaze.Markup`.
--
-- Example:
--
-- > example :: Text.Blaze.Markup
-- > example = $(shsmlFileWith defaultOptionsS "my_template.hsml")
shsmlFileWith :: I.Options -- ^ HSML Options.
              -> FilePath  -- ^ Name of input file.
              -> TH.ExpQ   -- ^ Resulting expression.
shsmlFileWith opts path = TH.runIO (readFile path) >>= shsmlStringWith opts
{-# INLINE shsmlFileWith #-}

-- | Parses Simplified HSML document from file with default options. Results in
-- expression of type `Text.Blaze.Markup`.
--
-- Example:
--
-- > example :: Text.Blaze.Markup
-- > example = $(shsmlFile "my_template.hsml")
shsmlFile :: FilePath -- ^ Name of input file.
          -> TH.ExpQ  -- ^ Resulting expression.
shsmlFile = shsmlFileWith I.defaultOptionsS
{-# INLINE shsmlFile #-}

-- | Parses Simplified HSML document from string with the given options. Results in
-- expression of type `Text.Blaze.Markup`.
--
-- Example:
--
-- > example :: Text.Blaze.Markup
-- > example = $(shsmlStringWith defaultOptionsS "<p>Paragraph</p>")
shsmlStringWith :: I.Options -- ^ HSML options
                -> String    -- ^ Input string.
                -> TH.ExpQ   -- ^ Resulting expression.
shsmlStringWith opts str = 
    case I.shsmlTemplate str of
        Right I.Template{..} -> makeExp opts templateDecs templateSections 
        Left  err            -> fail err
{-# INLINE shsmlStringWith #-}

-- | Parses Simplified HSML document from string with default options. Results in
-- expression of type `Text.Blaze.Markup`.
--
-- Example:
--
-- > example :: Text.Blaze.Markup
-- > example = $(shsmlString "<p>Paragraph</p>")
shsmlString :: String  -- ^ Input string.
            -> TH.ExpQ -- ^ Resulting expression.
shsmlString = shsmlStringWith I.defaultOptionsS
{-# INLINE shsmlString #-}

--------------------------------------------------------------------------------

-- | Generates a record type and its instance of `IsTemplate`.
makeDec :: I.Options -> I.Template -> TH.Q [TH.Dec]
makeDec opts@(I.Options _ _ tname fname) (I.Template args decs sections) = do
    (vars, fields) <- makeVarsAndFields
    tdata <- makeData     vars fields
    tinst <- makeInstance vars
    return [tdata, tinst] 
    where
      dataName :: TH.Name
      dataName = HE.toName tname

      -- | This function generates the type variable names and fields.
      makeVarsAndFields :: TH.Q ([TH.Name], [TH.VarStrictType])
      makeVarsAndFields = (reverse *** reverse) <$> foldM go ([], []) args
          where
            go (vs, fs) (I.Arg an (Just at)) =
              return (vs, (HE.toName $ fname an, TH.NotStrict, HE.toType at) : fs)
            go (vs, fs) (I.Arg an Nothing) = do
              v <- TH.newName "a"
              return (v : vs, (HE.toName $ fname an, TH.NotStrict, TH.VarT v) : fs)

      -- | This function generates the instance of IsTemplate.
      makeInstance :: [TH.Name] -> TH.DecQ 
      makeInstance vars =
          -- instance .IsTemplate (<dataName> <vars>) where
          --     renderTemplate (<dataName>{ <binds> }) = 
          --        <expression generated by makeExp>
          TH.instanceD
              (return [])
              (TH.conT ''I.IsTemplate `TH.appT` contyp)
              [TH.funD 'I.renderTemplate [clause]]
          where
            -- This is the data type applied to all the type variables.
            contyp = foldl (\acc n -> acc `TH.appT` TH.varT n) (TH.conT dataName) vars
            
            -- This is the clause for renderTemplate. 
            clause = 
              TH.clause [TH.recP dataName binds]
                        (TH.normalB $ makeExp opts decs sections) []
            -- This is the { <fieldName1> = <argName1>, ... } pattern.
            binds = map (\(I.Arg n _) -> return (HE.toName $ fname n, TH.VarP $ HE.toName n)) args
      
      -- | This function generates the record type.
      makeData :: [TH.Name] -> [TH.VarStrictType] -> TH.DecQ
      makeData vars fields = return $
          -- data <dataName> <vars> = <dataName> { <fields> }
          TH.DataD [] dataName (map TH.PlainTV vars) [TH.RecC dataName fields] []

-- | Generates an expression of the type `Text.Blaze.Markup`
makeExp :: I.Options -> [I.Dec] -> [I.Section] -> TH.ExpQ
makeExp opts decs sections =
    TH.letE (map (return . HE.toDec) decs) $ sectionsToExp opts sections

--------------------------------------------------------------------------------

sectionsToExp :: I.Options -> [I.Section] -> TH.ExpQ
sectionsToExp opts sections = 
    [e| mconcat $(TH.listE $ map (sectionToExp opts) sections) :: B.Markup |]

sectionToExp :: I.Options -> I.Section -> TH.ExpQ
sectionToExp opts@I.Options{..} section =
  case section of
    (I.ElementNode name atts sections decs) ->
      [e| applyAttributes $(TH.listE $ map (attributeToExp opts) atts) $
            B.Parent (fromString name)
                     (fromString $ "<" <> name)
                     (fromString $ "</" <> name <> ">")
                     $(makeExp opts decs sections) |]
    (I.ElementLeaf name atts) -> 
      [e| applyAttributes $(TH.listE $ map (attributeToExp opts) atts) $
            B.Leaf (fromString name)
                   (fromString $ "<" <> name)
                   (fromString ">") |]
    (I.Text    str) -> [e| B.string str |]
    (I.TextRaw str) -> [e| B.preEscapedString str |]
    (I.Expression e) ->
      if optExpToMarkup
         then [e| B.toMarkup $(toExp e) |]
         else toExp e

applyAttributes :: [B.Attribute] -> B.MarkupM a -> B.MarkupM a
applyAttributes attributes markup = foldl (B.!) markup attributes

attributeToExp :: I.Options -> I.Attribute -> TH.ExpQ
attributeToExp I.Options{..} (I.AttributeExp e) = toExp e
attributeToExp I.Options{..} (I.Attribute aname avalue) =
    [e| B.attribute (fromString $(attributeNameToExp aname))
                    (fromString (" " <> $(attributeNameToExp aname) <> "=\""))
                    $(attributeValueToExp avalue) |]
    where
      attributeNameToExp (I.AttributeNameText str) = [e| str |]
      attributeNameToExp (I.AttributeNameExp e) = toExp e

      attributeValueToExp (I.AttributeValueText str) = [e| B.toValue str |]     
      attributeValueToExp (I.AttributeValueExp e) =
        if optExpToValue
           then [e| B.toValue $(toExp e) |]
           else toExp e 

toExp :: HE.ToExp a => a -> TH.ExpQ
toExp = return . HE.toExp

{-
toExp :: HE.Exp -> TH.ExpQ
toExp (HE.Var n)                 = return $ TH.VarE (HE.toName n)
toExp (HE.Con n)                 = return $ TH.ConE (HE.toName n)
toExp (HE.Lit l)                 = return $ TH.LitE (HE.toLit l)
toExp (HE.InfixApp e o f)        = TH.uInfixE (toExp e) (return $ HE.toExp o) (toExp f)
toExp (HE.LeftSection e o)       = TH.infixE (Just $ toExp e) (return $ HE.toExp o) Nothing
toExp (HE.RightSection o f)      = TH.infixE Nothing (return $ HE.toExp o) (Just $ toExp f)
toExp (HE.App e f)               = TH.appE (toExp e) (toExp f)
toExp (HE.NegApp e)              = TH.appE (TH.varE 'negate) (toExp e)
toExp (HE.Lambda _ ps e)         = TH.lamE (fmap (return . HE.toPat) ps) (toExp e)
toExp (HE.Let bs e)              = TH.letE (map return $ HE.hsBindsToDecs bs) (toExp e)
toExp (HE.If a b c)              = TH.condE (toExp a) (toExp b) (toExp c)
toExp (HE.Do ss)                 = TH.doE (map (return . HE.toStmt) ss)
toExp (HE.Tuple xs)              = TH.tupE (fmap toExp xs)
toExp (HE.List xs)               = TH.listE (fmap toExp xs)
toExp (HE.Paren e)               = TH.parensE (toExp e)
toExp (HE.RecConstr n xs)        = TH.recConE (HE.toName n) (map (return . HE.toFieldExp) xs)
toExp (HE.RecUpdate e xs)        = TH.recUpdE (toExp e) (map (return . HE.toFieldExp) xs)
toExp (HE.EnumFrom e)            = TH.arithSeqE $ TH.fromR (toExp e)
toExp (HE.EnumFromTo e f)        = TH.arithSeqE $ TH.fromToR (toExp e) (toExp f)
toExp (HE.EnumFromThen e f)      = TH.arithSeqE $ TH.fromThenR (toExp e) (toExp f)
toExp (HE.EnumFromThenTo e f g)  = TH.arithSeqE $ TH.fromThenToR (toExp e) (toExp f) (toExp g)
toExp (HE.ExpTypeSig _ e t)      = TH.sigE (toExp e) (return $ HE.toType t)
toExp (HE.Case e alts)           = TH.caseE (toExp e) (map (return . HE.toMatch) alts)
toExp (HE.QuasiQuote n string)   = TH.parensE ([e|TH.quoteExp|] `TH.appE` TH.varE (HE.toName n)) `TH.appE` TH.litE (TH.stringL string)
toExp _ = error "Failed toExp"
-}

