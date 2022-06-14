module Main where
import Data.Monoid (mconcat,(<>))
import Data.List (intersperse, transpose)
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

newtype Table a = Table [Column a]

instance Semigroup (Table a) where
  (Table xs) <> (Table ys) = Table $ xs <> ys

instance Monoid (Table a) where
  mempty = Table []

singleton :: Column a -> Table a
singleton column = Table [column]

type RecordColumn = Column Record

columnNo :: RecordColumn
columnNo = Column "no" no


columnName :: RecordColumn
columnName = Column "name" name


records :: [Record]
records =
  [ Record "1" "Tom"
  , Record "2" "Jerry"
  , Record "3000" "Pitbull"
  ]

main :: IO ()
main = do
  let
    table = mconcat
      [ singleton $ Column "no" no
      , singleton $ Column "name" name
      ]
  putStrLn $ columnsAscii table records


leftAlign :: [String] -> [String]
leftAlign cells =
  let
    maxLength = foldl max 0 $ map length cells
    pad s = if length s >= maxLength
        then s
        else s <> replicate (maxLength - length s) ' '
  in map pad cells

columnsAscii :: Table a -> [a] -> String
columnsAscii (Table columns) rows = output
  where
    headerCells = map header columns
    extractors = map field columns
    extractRow row = map (\f -> f row) extractors

    cellsFormatted = headerCells : map extractRow rows
    cellsFormattedAligned = (transpose . map leftAlign . transpose) cellsFormatted

    output = join . intersperse "\n" $ map (unwords . intersperse "|") cellsFormattedAligned
