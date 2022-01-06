-- | The n-sphere S^n
module Math.Topology.SSet.Sphere where

import Math.Topology.SSet
import Math.Topology.SSet.Effective

newtype Sphere = Sphere { sphereDim :: Int }
  deriving (Eq, Ord, Show)

data SphereSimplex = Basepoint | Cell
  deriving (Eq, Ord, Show)

instance SSet Sphere where
  type GeomSimplex Sphere = SphereSimplex

  isGeomSimplex _ _ = True

  geomSimplexDim (Sphere n) Basepoint = 0
  geomSimplexDim (Sphere n) Cell = n

  geomFace (Sphere n) Basepoint _ = undefined
  geomFace (Sphere n) Cell _ = constantAt Basepoint (n - 1)
  geomFaces (Sphere n) Basepoint = []
  geomFaces (Sphere n) Cell = replicate (n+1) $ constantAt Basepoint (n - 1)

instance LevelwiseFinite Sphere where
  geomBasis (Sphere n) i
    | i == 0 = [Basepoint]
    | i == n = [Cell]
    | otherwise = []

instance Pointed Sphere where
  basepoint _ = Basepoint

instance Effective Sphere