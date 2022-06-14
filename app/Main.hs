module Main where
import Data.List (intersperse)
import Control.Monad (join)

type Header = String
type Formatted = String

data Record = Record
  { no :: String
  , name :: String
  }

data Column a = Column
  { header :: Header
  , field :: a -> Formatted
  }


type RecordColumn = Column Record

columnNo :: RecordColumn
columnNo = Column "no" no


columnName :: RecordColumn
columnName = Column "name" name


records :: [Record]
records =
  [ Record "1" "Tom"
  , Record "2" "Jerry"
  , Record "3" "Pitbull"
  ]

main :: IO ()
main = do
  let
    columns =
      [ columnNo
      , columnName
      ]
  putStrLn $ columnsAscii columns records


columnsAscii :: [Column a] -> [a] -> String
columnsAscii columns rows = headerRow <> "\n" <> body
  where
    headerRow = unwords $ intersperse " | " $ map header columns
    extractors = map field columns
    extractRow row = map (\f -> f row) extractors
    rowsFormatted = map extractRow rows
    body = join . intersperse "\n" $ map (unwords . intersperse " | ") rowsFormatted
