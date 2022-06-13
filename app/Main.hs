module Main where
import Data.List (intersperse)

type Header = String
type Formatted = String

data Record = Record
  { no :: String
  , name :: String
  }

data Column = Column
  { header :: Header
  , cells :: [Formatted]
  }

records :: [Record]
records =
  [ Record "1" "Tom"
  ]

main :: IO ()
main = do
  let
    columns =
      [ Column "no" $ map no records
      , Column "name" $ map name records
      ]
  putStrLn $ columnsAscii columns


columnsAscii :: [Column] -> String
columnsAscii columns = headerRow <> "\n" <> body
  where
    headerRow = unwords $ intersperse " | " $ map header columns
    rows = map cells columns
    body = unwords $ zipWith (curry f) (head rows) (rows!!1)
    f (a, b) = a <> " | " <> b


tableAscii :: [Header] -> [Record] -> String
tableAscii headers items = headerRow <> "\n" <> concatMap f items
  where
    headerRow = unwords $ intersperse " | "  headers
    f (Record no name) = no <> " | " <> name <> "\n"