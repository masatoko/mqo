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
import qualified Text.Megaparsec.Char.Lexer as L

import Data.MQO.Types

type Parser = Parsec Void String

readMQO :: FilePath -> IO () -- Object
readMQO path = do
  contents <- readFile path
  -- putStrLn contents
  -- parseTest test contents
  parseTest materials mats
  where
    mats = "Material 2 {\n\t\"reddish\" shader(3) col(1.000 0.208 0.059 1.000) dif(0.800) amb(0.600) emi(0.287) spc(0.000) power(5.00)\n\t\"blue\" shader(3) col(0.259 0.278 1.000 1.000) dif(0.800) amb(0.600) emi(0.637) spc(0.000) power(5.00)\n}"

symbol :: String -> Parser String
symbol = L.symbol space

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

dquot :: Parser a -> Parser a
dquot = between (symbol "\"") (symbol "\"")

materials :: Parser [Material]
materials = do
  string "Material"
  space
  some digitChar
  space
  between (symbol "{") (symbol "}") (some material)

material :: Parser Material
material = do
  skipMany newline
  skipMany tab
  Material
    <$> dquot (some (notChar '\"'))
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
      parens number
      where
        number = some (digitChar <|> char '.')

    color :: Parser (V4 Double)
    color = do
      string "col"
      parens v4
      where
        v4 = V4 <$> double <* space
                <*> double <* space
                <*> double <* space
                <*> double
        double = L.float
