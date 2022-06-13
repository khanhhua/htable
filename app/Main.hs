module Main where

data Record = Record
  { no :: String
  , name :: String
  }

records :: [Record]
records =
  [ Record "1" "Tom"
  ]

main :: IO ()
main = do
  putStrLn $ tableAscii []
  
tableAscii :: [Record] -> String
tableAscii rows =
  undefined


