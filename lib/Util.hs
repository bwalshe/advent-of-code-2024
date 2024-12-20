module Util where 


import Data.Text.IO as TIO (readFile)
import Text.Megaparsec
import Data.Text (Text)
import Data.Void

type Parser = Parsec Void Text


runDailyTasks :: Parser a -> (a -> IO()) -> String -> IO ()
runDailyTasks parser tasks fileName = do
  fileText <- TIO.readFile fileName
  case runParser parser fileName fileText of
    Right input -> tasks input
    Left e -> putStrLn $ errorBundlePretty e


