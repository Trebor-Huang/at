module Pi4S3 where

import System.IO

import Math.Topology.SSet
import Math.Topology.SSet.Effective
import Math.Topology.SSet.Sphere
import Math.Topology.SSet.Product
import Math.Topology.SSet.TwistedProduct
import Math.Topology.SGrp.Wbar
import Math.Topology.SGrp.KGn

s3 = Sphere 3
kz2 = Wbar kz1
kz3 = Wbar (Wbar kz1)

-- TODO make this automatic, in the library
-- i.e. fundamental map of S^n -> K(Z/nZ, n)
classifying :: Morphism Sphere (Wbar (Wbar KZ1))
classifying = Morphism m
  where m Cell = nonDegen $ WbarSimplex [
              nonDegen (WbarSimplex [nonDegen [1], nonDegen []]),
              (FormalDegen (WbarSimplex []) (DegenSymbol [2])),
              nonDegen (WbarSimplex [])
            ]
        m Basepoint = nonDegen $ WbarSimplex []

fibration :: Twist Sphere (Wbar KZ1)
fibration = pullback kz3 kz2 (canonicalTwist kz2) classifying

x :: TotalSpace Sphere (Wbar KZ1)
x = totalSpace s3 (Wbar kz1) fibration

main = do
  hSetBuffering stdout NoBuffering
  putStr "π₄ S³ is: "
  print (homology x !! 4)
