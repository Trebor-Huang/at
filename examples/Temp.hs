module Temp where
import Math.Algebra.Group
import Math.Algebra.ChainComplex
import Math.Algebra.ChainComplex.Tensor
import Math.Topology.SSet
import Math.Topology.SSet.Sphere
import Math.Topology.SGrp
import Math.Topology.SGrp.Wbar
import Math.Topology.SGrp.WbarDiscrete
import Math.Topology.SGrp.KGn
import Math.Topology.SSet.Effective
import Math.Topology.SSet.Product
import Math.Topology.SSet.NChains
import Math.Algebra.ChainComplex.Reduction

main :: IO ()
main = do
  let p = Product (Sphere 3) (Sphere 2)
  putStrLn "Starting:"
  print (reductionG (ezReduction p) `onBasis` (BasisSimplex Cell, BasisSimplex Cell))
