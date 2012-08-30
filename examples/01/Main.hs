{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE RecordWildCards #-}

------------------------------------------------------------------------------
import           Data.Monoid 
------------------------------------------------------------------------------
import           Control.Monad
------------------------------------------------------------------------------
import qualified Text.Blaze.Html5            as B
import qualified Text.Blaze.Html5.Attributes as B
------------------------------------------------------------------------------
import           Template.HSML
------------------------------------------------------------------------------

opt :: Monoid a => Bool -> a -> a
opt c m = if c then m else mempty

test :: String -> String -> Bool -> B.Markup
test name value condition = [m|
  <div class="test" href={h|value|} {h|name|}={h|value|} {h| opt condition $ B.selected $ B.toValue "selected" |}>
    test
  </div>
  |]

data User = User
    { userID :: Int
    , userName :: String
    , userAge :: Int
    } 

$(hsmlFileWith (defaultOptions "Default") "default_layout.hsml")

homeTemplate :: [User] -> B.Markup
homeTemplate users = renderTemplate Default
    { defaultTitle = "Home page"
    , defaultSectionMiddle = middle
    , defaultSectionFooter = [m| <p>Generated HSML</p> |]
    }
    where
      middle = [m|
        <ul class="users">
          {h| forM_ users wrap |}
        </ul> |]
      wrap u = [m|<li> {h| userTemplate u |} </li>|]
        
userTemplate :: User -> B.Markup
userTemplate User{..} = [m|
  <ul class={h| "user-" <> show userID |}>
    <li>Name: {h|userName|}</li>
    <li>Age: {h|userAge|}</li>
  </ul> |]

