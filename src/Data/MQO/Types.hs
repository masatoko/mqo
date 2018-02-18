module Data.MQO.Types where

import qualified Data.ByteString as BS
import qualified Data.Vector as V

import           Linear.V2
import           Linear.V3
import           Linear.V4

type Index = Int

type Vertex = V3 Float
type UV = V2 Float

data Face = Face
  { faceVerts  :: V3 Index
  , faceMatIdx :: Index
  , faceUVs    :: V3 UV
  } deriving Show

data Object = Object
  { objectName     :: String
  , objectVertices :: V.Vector Vertex
  , objectFaces    :: V.Vector Face
  } deriving Show

--

data Shader
  = Classic
  | Constant
  | Lambert
  | Phong
  | Blinn
  deriving (Eq, Ord, Show, Read, Enum)

data Material = Material
  { matName     :: String
  , matShader   :: Shader
  , matColor    :: V4 Float
  , matDiffuse  :: Float
  , matAmbient  :: Float
  , matEmissive :: Float
  , matSpecular :: Float
  , matSpcPower :: Float -- ^ Power of specular
  } deriving Show
