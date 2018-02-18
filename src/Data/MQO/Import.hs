module Data.MQO.Import
  ( readMQO
  ) where

import           Debug.Trace

import qualified Data.Vector                as V
import           Data.Void
import           Linear.V2
import           Linear.V3
import           Linear.V4

import qualified Data.ByteString            as BS

import           Control.Monad.Combinators
import           Text.Megaparsec
-- import Text.Megaparsec.Byte
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import           Data.MQO.Types

type Parser = Parsec Void String

readMQO :: FilePath -> IO () -- Object
readMQO path = do
  contents <- readFile path
  parseTest metasequoia contents

symbol :: String -> Parser String
symbol = L.symbol space

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "{") (symbol "}")

dquot :: Parser a -> Parser a
dquot = between (symbol "\"") (symbol "\"")

nameDQuoted :: Parser String
nameDQuoted = dquot (some alphaNumChar)

skipToHeadOf str = skipManyTill anyChar (lookAhead (string str))

metasequoia :: Parser ([Material], Object)
metasequoia = do
  skipToHeadOf "Material"
  (,) <$> materials <*> object

object :: Parser Object
object = do
  string "Object"
  space
  name <- nameDQuoted
  space
  (vs, fs) <- brackets content
  return $ Object name (V.fromList vs) (V.fromList fs)
  where
    num = L.signed space L.float

    content = do
      skipToHeadOf "vertex"
      vs <- vertices
      skipToHeadOf "face"
      fs <- faces
      return (vs, fs)

    vertices :: Parser [Vertex]
    vertices = do
      string "vertex"
      space
      some digitChar
      space
      between (symbol "{") (end >> symbol "}") (some (try vertex))
      where
        end = newline >> tab
        vertex = do
          skipMany newline
          skipMany tab
          V3 <$> num <* space <*> num <* space <*> num

    faces :: Parser [Face]
    faces = do
      string "face"
      space
      some digitChar
      space
      between (symbol "{") (symbol "}") (some face)
      where
        face = do
          skipMany tab
          digitChar
          space
          Face <$> one "V" v3 <* space
               <*> one "M" L.decimal <* space
               <*> one "UV" uv3
          where
            number = (fromIntegral <$> L.decimal) <|> L.float

            v3 = V3 <$> L.decimal <*  space1
                    <*> L.decimal <*  space1
                    <*> L.decimal
            uv = V2 <$> number <* space1 <*> number
            uv3 = V3 <$> uv <* space1
                     <*> uv <* space1
                     <*> uv

        one :: Show a => String -> Parser a -> Parser a
        one tag p = do
          string tag
          parens p

materials :: Parser [Material]
materials = do
  string "Material"
  space
  some digitChar
  space
  brackets (some material)

material :: Parser Material
material = do
  skipMany newline
  skipMany tab
  Material
    <$> nameDQuoted
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
