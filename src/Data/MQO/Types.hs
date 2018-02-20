module Data.MQO.Types where

import qualified Data.ByteString as BS
import qualified Data.Vector     as V

import           Linear.V2
import           Linear.V3
import           Linear.V4

type Index = Int
type Radian = Float

type Vertex = V3 Float
type UV = V2 Float

data Face
  = Face3
    { faceVerts3 :: V3 Index
    , faceMatIdx :: Index
    , faceUVs3   :: V3 UV
    }
  | Face4
    { faceVerts4 :: V4 Index
    , faceMatIdx :: Index
    , faceUVs4   :: V4 UV
    }
  deriving Show

data Object = Object
  { objectName     :: String
  , objectFacet    :: Radian
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
  , matAmbientCol  :: Maybe (V3 Float)
  , matEmissiveCol :: Maybe (V3 Float)
  , matSpecularCol :: Maybe (V3 Float)
  , matReflect  :: Maybe Float
  , matTexPath  :: Maybe FilePath -- ^ Texture file path
  , matBumpPath :: Maybe FilePath -- ^ Bumpmap file path
  } deriving Show
