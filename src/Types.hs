module Types where

data Flag = All
          | Rules
          | Help
          | GraphID String
          | GraphName String
          deriving (Eq)
