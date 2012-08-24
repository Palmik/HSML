module Main
( main
) where
    
------------------------------------------------------------------------------
import           Test.Framework (defaultMain, testGroup)
------------------------------------------------------------------------------
import qualified Parser.Syntax 
------------------------------------------------------------------------------

main :: IO ()
main = defaultMain
    [ testGroup "Parser.Syntax" Parser.Syntax.tests
    ]

