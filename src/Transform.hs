{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
-- Bye, bye type inference :'(
{-# LANGUAGE RankNTypes #-}
--{-# LANGUAGE ImpredicativeTypes #-}
module Transform ( list
                 , snap
                 ) where

import Prelude hiding (readFile, writeFile)
import Control.Monad.Identity (Identity)
import Data.Monoid ((<>), Endo)
import Data.Foldable (foldl')
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import Text.Read (readMaybe) -- Why the hell is this not in the Prelude?
import System.Exit (exitFailure)
import System.IO.Unsafe (unsafePerformIO)

import Text.XML
import Text.XML.Lens

import Types
import PrettyPrint (graphInfo)

list :: FilePath -> [Flag] -> IO ()
list file flags = do
    let graphs = graphsByFlags flags
    doc <- readFile def file
    sequence_ $ (flip fmap) graphs $ \graph -> do
        let ids = Text.unpack <$> doc ^.. graph . attr "ID"
            names = Text.unpack <$> doc ^.. graph . attr "name"
        putStrLn $ graphInfo ids names

snap :: FilePath -> [Flag] -> String -> String -> IO ()
snap file flags w h = do
    let graphs = graphsByFlags flags
    doc <- readFile def file
    let doc' = snapGraphs (stringToInt w) (stringToInt h) graphs doc
    putStrLn . LText.unpack $ renderText def{rsPretty=True} doc'

snapGraphs :: Int -> Int -> [LensLike Identity Document Document Element Element] -> Document -> Document
snapGraphs _ _ [] doc = doc
snapGraphs w h (graphs:graphss) doc = let doc' = snapGraphs' w h graphs doc
                                      in  snapGraphs w h graphss doc'

snapGraphs' :: Int -> Int -> LensLike Identity Document Document Element Element -> Document -> Document
snapGraphs' w h trav doc = (doc & attrX %~ snapS w) & attrY %~ snapS h
    where attrX = trav ./ el "Node" ./ attr "X"
          attrY = trav ./ el "Node" ./ attr "Y"
          snapS :: Int -> Text.Text -> Text.Text
          snapS sn = Text.pack . show . snapVal sn . stringToInt . Text.unpack
          snapVal :: Int -> Int -> Int
          snapVal sn val = round (fromIntegral val / fromIntegral sn) * sn


graphsByFlags :: forall (f :: * -> *). Applicative f
              => [Flag] -> [(Element -> f Element) -> Document -> f Document]
-- Type below is slightly more specific than type above. They are very similar,
-- and for the purposes of this program the same, however
--graphsByFlags :: Monoid a => [Flag] -> [(Element -> Const a Element) -> Document -> Const a Document]
graphsByFlags [] = []
graphsByFlags (x:xs) = graphsByFlag x : graphsByFlags xs

graphsByFlag :: Flag -> Traversal Document Document Element Element
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

stringToInt :: String -> Int
stringToInt x = case readMaybe x of
                  Nothing -> unsafePerformIO $ putStrLn ("Invalid integer: " ++ x) >> exitFailure -- at that point you probably don't care that it's unsafe
                  Just y -> y
