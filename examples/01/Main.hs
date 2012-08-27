{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE RecordWildCards #-}

------------------------------------------------------------------------------
import           Data.Monoid ((<>))
------------------------------------------------------------------------------
import           Control.Monad
------------------------------------------------------------------------------
import qualified Text.Blaze.Html5 as B
------------------------------------------------------------------------------
import           Template.HSML
------------------------------------------------------------------------------

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
    , defaultSectionFooter = [m| <p>Created by Petr Pilař in 2012</p> |]
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

