module Data.MQO.Import
  ( readMQO
  ) where

import Debug.Trace

import Linear.V4
import Data.Void

import qualified Data.ByteString as BS

import Control.Monad.Combinators
import Text.Megaparsec
-- import Text.Megaparsec.Byte
import Text.Megaparsec.Char

import Data.MQO.Types

type Parser = Parsec Void String

readMQO :: FilePath -> IO () -- Object
readMQO path = do
  contents <- readFile path
  -- putStrLn contents
  -- parseTest test contents
  parseTest material mat
  where
    mat = "\t\"reddish\" shader(3) col(1.000 0.208 0.059 1.000) dif(0.800) amb(0.600) emi(0.287) spc(0.000) power(5.00)"

material :: Parser Material
material = do
  tab
  Material
    <$> between (char '\"') (char '\"') (some (notChar '\"'))
    <*  space
    <*> ((toEnum . read) <$> one "shader")
    <*  space
    <*> color
    <*  space
    <*> doubleBy "dif"
    <*  space
    <*> doubleBy "amb"
    <*  space
    <*> doubleBy "emi"
    <*  space
    <*> doubleBy "spc"
    <*  space
    <*> doubleBy "power"
  where
    doubleBy tag = read <$> one tag

    one :: String -> Parser String
    one tag = do
      string tag
      between (char '(') (char ')') number

    color :: Parser (V4 Double)
    color = do
      string "col"
      between (char '(') (char ')') v4
      where
        v4 = V4 <$> double <* space
                <*> double <* space
                <*> double <* space
                <*> double
        double = read <$> number

    number = some (digitChar <|> char '.')

test :: Parser [String]
test =
  some work
  where
    work = do
      n <- some digitChar
      traceM (show n)
      space <|> (eol >> pure ())
      return n
