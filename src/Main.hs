{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (readFile, writeFile)
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit (exitFailure)

import Types
import qualified Transform
import PrettyPrint (usageHeader, usageFooter)

options :: [OptDescr Flag]
options = [ Option ['a'] ["all", "hosts"] (NoArg All) "Apply action to all host graphs"
          , Option ['r'] ["rules"] (NoArg Rules) "Apply action to all rule graphs"
          , Option [] ["id"] (ReqArg GraphID "id") "Apply action to graphs with specified ID"
          , Option [] ["name"] (ReqArg GraphName "name") "Apply action to graphs with specified name"
          , Option ['h', '?'] ["help"] (NoArg Help) "Display this help"
          ]

parseArgs :: [String] -> IO ([Flag], [String])
parseArgs args = case getOpt Permute options args of
    (flags, nonOptions, []) -> return (flags, nonOptions)
    (_, _, errors)          -> ioError (userError (concat errors ++ usageInfo'))

main :: IO ()
main = do
    (flags, nonOpts) <- getArgs >>= parseArgs

    case nonOpts of
      ("list":file:_) -> Transform.list file flags
      ("snap":x:y:file:_) -> Transform.snap file flags x y
      _ -> putStrLn ("Unknown action or insufficient parameters.\n\n" ++ usageInfo') >> exitFailure

usageInfo' :: String
usageInfo' = usageInfo usageHeader options ++ "\n" ++ usageFooter
