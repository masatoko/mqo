# mqo

A minimum [Metasequoia](http://www.metaseq.net/) file parser

```haskell
import qualified Data.ByteString as BS
import           Text.Megaparsec (parse)

import           Data.MQO        (mqo)

main :: IO ()
main = do
  bytes <- BS.readFile "model.mqo" -- Input
  case parse mqo "" bytes of
    Left error             -> print error
    Right (materials, obj) -> do
      print materials
      print obj

```
