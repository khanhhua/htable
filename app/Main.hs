module Main where
import Data.List (intersperse)

type Header = String

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
  putStrLn $ tableAscii ["no", "name"] records

tableAscii :: [Header] -> [Record] -> String
tableAscii headers items = headerRow <> "\n" <> concatMap f items
  where
    headerRow = unwords $ intersperse " | "  headers 
    f (Record no name) = no <> " | " <> name <> "\n"