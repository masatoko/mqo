module Data.MQO.Types where

import qualified Data.Vector.Unboxed as V

import           Linear.V2
import           Linear.V3
import           Linear.V4

type Index = Int

type Vertex = V3 Double
type UV = V2 Double

data Face = Face
  { faceVerts  :: V3 Index
  , faceMatIdx :: Index
  , faceUVs    :: V3 UV
  }

data Object = Object
  { objectName     :: String
  , objectVertices :: V.Vector Vertex
  , objectFaces    :: V.Vector Face
  }

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
  , matColor    :: V4 Double
  , matDiffuse  :: Double
  , matAmbient  :: Double
  , matEmissive :: Double
  , matSpecular :: Double
  , matSpcPower :: Double -- ^ Power of specular
  } deriving Show
