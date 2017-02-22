module Node where

import           Text.Printf

data Node = Node {
      name :: String
    , foo  :: Int
    , perse :: Maybe Bool
   } deriving (Eq, Show)
