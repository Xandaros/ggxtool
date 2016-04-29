{-# LANGUAGE OverloadedStrings #-}
module PrettyPrint where

import Text.PrettyPrint.Boxes

usageHeader :: String
usageHeader = "ggxtool [option...] <action> <file>"

usageFooter :: String
usageFooter = render $ hsep 2 top
    [ vcat left
        [ "Actions:"
        , "snap <width> <height>"
        , "list"
        ]
    , vcat left
        [ ""
        , "Snap vertices to a grid, where <width> and <height> define the grid size."
        , "List all graphs. Will print nothing without options"
        ]
    ]

graphInfo :: [String] -> [String] -> String
graphInfo ids names = render $ hsep 2 top
    [ vcat left $ ["ID:"] ++ (text <$> ids)
    , vcat left $ ["Name:"] ++ (text <$> names)
    ]
