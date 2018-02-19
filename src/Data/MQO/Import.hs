{-# LANGUAGE OverloadedStrings #-}
module Data.MQO.Import
  ( mqo
  ) where

import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as C8
import qualified Data.Vector                as V
import           Data.Void
import           Linear.V2
import           Linear.V3
import           Linear.V4
import           Safe                       (readMay)
import Data.Word (Word8)

import           Control.Monad.Combinators
import           Text.Megaparsec
import           Text.Megaparsec.Byte
import qualified Text.Megaparsec.Byte.Lexer as L

import           Data.MQO.Types

type Parser = Parsec Void BS.ByteString

symbol :: BS.ByteString -> Parser BS.ByteString
symbol = L.symbol space

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

dquot :: Parser a -> Parser a
dquot = between (symbol "\"") (symbol "\"")

nameDQuoted :: Parser String
nameDQuoted = toString <$> dquot (some (noneOf (BS.unpack "\"")))
  where
    toString :: [Word8] -> String
    toString = C8.unpack . BS.pack

skipToHeadOf str = skipManyTill anyChar (lookAhead (string str))

mqo :: Parser ([Material], Object)
mqo = do
  skipToHeadOf "Material"
  (,) <$> materials <*> object

object :: Parser Object
object = do
  string "Object"
  space
  name <- nameDQuoted
  space
  (vs, fs) <- braces content
  return $ Object name (V.fromList vs) (V.fromList fs)
  where
    num = L.signed space (try L.float <|> (fromIntegral <$> L.decimal))

    content = (,)
      <$> (skipToHeadOf "vertex" *> vertices)
      <*> (skipToHeadOf "face" *> faces)

    vertices :: Parser [Vertex]
    vertices = do
      string "vertex" *> space *> L.decimal *> space
      braces (some vertex)
      where
        vertex = V3 <$> num <* space <*> num <* space <*> num <* space

    faces :: Parser [Face]
    faces = do
      string "face" *> space *> L.decimal *> space
      braces (some face)
      where
        face = do
          skipMany tab
          numVert <- L.decimal
          space
          case numVert of
            3 -> Face3 <$> one "V" v3 <* space
                       <*> one "M" L.decimal <* space
                       <*> one "UV" uv3
            4 -> Face4 <$> one "V" v4 <* space
                       <*> one "M" L.decimal <* space
                       <*> one "UV" uv4
            _ -> error $ "Unexpected num of face vertices: " ++ show numVert
          where
            number = L.signed space (try L.float <|> (fromIntegral <$> L.decimal))

            v3 = V3 <$> L.decimal <* space1 <*> L.decimal <* space1 <*> L.decimal
            v4 = V4 <$> L.decimal <* space1 <*> L.decimal <* space1 <*> L.decimal <* space1 <*> L.decimal
            uv3 = V3 <$> uv <* space1 <*> uv <* space1 <*> uv
            uv4 = V4 <$> uv <* space1 <*> uv <* space1 <*> uv <* space1 <*> uv
            uv = V2 <$> number <* space1 <*> number

        one :: Show a => BS.ByteString -> Parser a -> Parser a
        one tag p = string tag *> parens p

materials :: Parser [Material]
materials = do
  string "Material" *> space
  some digitChar *> space
  braces (some material)

material :: Parser Material
material = do
  skipMany newline
  skipMany tab
  Material
    <$> nameDQuoted <*  space
    <*> (toEnum <$> oneInt "shader") <*  space
    <*> color <*  space
    <*> oneFloat "dif" <* space
    <*> oneFloat "amb" <* space
    <*> oneFloat "emi" <* space
    <*> oneFloat "spc" <* space
    <*> oneFloat "power"
    <*> optional (oneFloat3 "amb_col") <* space
    <*> optional (oneFloat3 "emi_col") <* space
    <*> optional (oneFloat3 "spc_col") <* space
    <*> optional (oneFloat "reflect") <* space
    <*> optional (oneStr "tex") <* space
    <*> optional (oneStr "bump") <* space
  where
    oneInt tag = string tag *> parens L.decimal
    oneFloat tag = string tag *> parens L.float
    oneStr tag = string tag *> parens nameDQuoted
    oneFloat3 tag = do
      string tag
      parens (V3 <$> L.float <* space <*> L.float <* space <*> L.float)

    color :: Parser (V4 Float)
    color = string "col" *> parens v4
      where
        v4 = V4 <$> L.float <* space
                <*> L.float <* space
                <*> L.float <* space
                <*> L.float
