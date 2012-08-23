{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE RecordWildCards #-}

------------------------------------------------------------------------------
import           Data.Monoid ((<>))
------------------------------------------------------------------------------
import qualified Text.Blaze.Html             as B (Html)
import qualified Text.Blaze.Html5            as B 
import qualified Text.Blaze.Html5.Attributes as B
import qualified Text.Blaze.Renderer.Pretty  as B
------------------------------------------------------------------------------
import           Template.HSML
------------------------------------------------------------------------------

data User = User
    { userID :: Int
    , userName :: String
    , userAge :: Int
    } 

$(hsmlFileWith (defaultHSML "Default") "default_layout.hsml")

homeTemplate :: User -> B.Markup
homeTemplate user = renderTemplate Default
    { defaultTitle = "Home page"
    , defaultSectionMiddle = userTemplate user
    , defaultSectionFooter = [m|<p>Created by Petr Pilař in 2012</p>|]
    }

userTemplate :: User -> B.Markup
userTemplate User{..} = [m|
  <ul class={e| "user-" <> show userID |}>
    <li>Name: {e|userName|}</li>
    <li>Age: {e|userAge|}</li>
  </ul>
|]

