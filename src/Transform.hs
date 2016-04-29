{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Transform ( list
                 , snap
                 ) where

import Prelude hiding (readFile, writeFile)
import Data.Monoid ((<>), Endo)
import Data.Foldable (foldl')
import qualified Data.Map as Map
import qualified Data.Text as Text
import Text.Read (readMaybe) -- Why the hell is this not in the Prelude?
import System.Exit (exitFailure)

import Text.XML
import Text.XML.Lens

import Types
import PrettyPrint (graphInfo)

list :: FilePath -> [Flag] -> IO ()
list file flags = do
    putStrLn "List"
    let graphs = graphsByFlags flags
    doc <- readFile def file
    sequence_ $ (flip fmap) graphs $ \graph -> do
        let ids = Text.unpack <$> doc ^.. graph . attr "ID"
            names = Text.unpack <$> doc ^.. graph . attr "name"
        putStrLn $ graphInfo ids names

snap :: FilePath -> [Flag] -> String -> String -> IO ()
snap file flags x y = putStrLn "snap"



-- graphsByFlags :: [Flag] -> [Traversal' Document Element]
-- Type below is slightly more specific than type above. They are very similar,
-- and for the purposes of this program the same, however
graphsByFlags :: Monoid a => [Flag] -> [(Element -> Const a Element) -> Document -> Const a Document]
graphsByFlags [] = []
graphsByFlags (x:xs) = graphsByFlag x : graphsByFlags xs

graphsByFlag :: Flag -> Traversal' Document Element
graphsByFlag All = hostGraphs
graphsByFlag Rules = ruleGraphs
graphsByFlag (GraphID id) = graphByID id
graphsByFlag (GraphName name) = graphByName name

header :: Traversal' Document Element
header = root . el "Document" ./ el "GraphTransformationSystem"

hostGraphs :: Traversal' Document Element
hostGraphs = header ./ el "Graph"

ruleGraphs :: Traversal' Document Element
ruleGraphs = header ./ el "Rule" ./ el "Graph"

graphByID :: String -> Traversal' Document Element
graphByID id = header ./ deep (el "Graph" . attributeIs "ID" (Text.pack id))

graphByName :: String -> Traversal' Document Element
graphByName name = header ./ deep (el "Graph" . attributeIs "name" (Text.pack name))

stringToInt :: String -> IO Int
stringToInt x = case readMaybe x of
                  Nothing -> putStrLn ("Invalid integer: " ++ x) >> exitFailure
                  Just y -> pure y
