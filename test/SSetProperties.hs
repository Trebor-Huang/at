-- |
module SSetProperties where

import Control.Monad (forM_, unless, when)
import Test.Hspec
import Test.QuickCheck
import Math.Topology.SSet
import Math.Topology.SSet.Product
import Math.Topology.SSet.TwistedProduct
import Math.Topology.SGrp
import ArbitraryInstances ()

slowCompose :: [Int] -> [Int] -> [Int]
slowCompose p q = map ((q !!) . (p !!)) [0..]

prop_dComposeCorrect :: DegenSymbol -> DegenSymbol -> Bool
prop_dComposeCorrect d1 d2 =
  case d1 <> d2 of
    NonDegen -> True
    DegenSymbol d  ->
      last d /= 1 &&
      let len =
            (sum (dsymbol d1) + length (dsymbol d1)) +
            (sum (dsymbol d2) + length (dsymbol d2)) in
        take len (slowDegen 0 d1 `slowCompose` slowDegen 0 d2)
          == take len (slowDegen 0 (DegenSymbol d))

prop_exchangeCorrect :: FaceSymbol -> DegenSymbol -> Bool
prop_exchangeCorrect f d =
  let (d', f') = f # d in
  let len =
        (sum (fsymbol f) + length (fsymbol f)) +
        (sum (dsymbol d) + length (dsymbol d)) in
    take len (slowFace 0 f `slowCompose` slowDegen 0 d)
      == take len (slowDegen 0 d' `slowCompose` slowFace 0 f')
    && if length (dsymbol d') > 0 then
      last (dsymbol d') /= 1
    else True

checkSSetSymbols :: Spec
checkSSetSymbols = do
  it "should compose correctly" $
    property prop_dComposeCorrect
  it "should exchange correctly" $
    property prop_exchangeCorrect

checkIdentities :: (SSet a, Show (GeomSimplex a)) => a -> GeomSimplex a -> Expectation
checkIdentities a g = do
  let d = geomSimplexDim a g
      s = nonDegen g

  -- it "satisfies the simplicial identity ∂i ∘ ∂j = ∂(j-1) ∘ ∂i if i < j" $
  when (d > 1) $
    sequence_ $ do
      j <- [1 .. d]
      i <- [0 .. (j - 1)]
      return $
        unless (face a (face a s j) i == face a (face a s i) (j - 1)) $
          expectationFailure $ "On simplex " ++ show g ++ ", ∂" ++ show i ++ " ∘ ∂" ++ show j ++ " = " ++ show (face a (face a s j) i) ++ " but " ++ "∂" ++ show (j -1) ++ " ∘ ∂" ++ show i ++ " = " ++ show (face a (face a s i) (j - 1))

  -- The rest should follow from the formal degeneracy operations but
  -- may as well do them as a sanity check
  -- it "satisfies the simplicial identity si ∘ sj = sj ∘ s(i-1) if i > j" $
  sequence_ $ do
    i <- [1 .. d]
    j <- [0 .. (i - 1)]
    return $ degen (degen s j) i `shouldBe` degen (degen s (i - 1)) j

  -- it "satisfies the simplicial identity ∂i ∘ sj = s(j-1) ∘ ∂i if i < j" $
  sequence_ $ do
    j <- [1 .. d]
    i <- [0 .. (j - 1)]
    return $ 
      unless (face a (degen s j) i == degen (face a s i) (j - 1)) $
        expectationFailure $ "∂" ++ show i ++ " ∘ s" ++ show j ++ " on " ++ show s

  -- it "satisfies the simplicial identity ∂j ∘ sj = id" $
  sequence_ $ do
    j <- [0 .. d]
    return $ face a (degen s j) j `shouldBe` s

  -- it "satisfies the simplicial identity ∂(j+1) ∘ sj = id" $
  sequence_ $ do
    j <- [0 .. d]
    return $ face a (degen s j) (j + 1) `shouldBe` s

  -- it "satisfies the simplicial identity ∂i ∘ sj = sj ∘ ∂(i-1) if i > j+1" $
  sequence_ $ do
    i <- [2 .. d]
    j <- [0 .. (i - 2)]
    return $ face a (degen s j) i `shouldBe` degen (face a s (i - 1)) j

checkFaces :: (SSet a, Show (GeomSimplex a)) => a -> GeomSimplex a -> Expectation
checkFaces a g =
  forM_ (zip [0..] (geomFaces a g)) (\(i, s) -> unless (isSimplex a s) $ expectationFailure $ show i ++ "th face " ++ show s ++ " of " ++ show g ++ " is not a valid simplex")

checkDims :: (SSet a, Show (GeomSimplex a)) => a -> GeomSimplex a -> Expectation
checkDims a g =
  let d = geomSimplexDim a g
   in forM_ (geomFaces a g) (\s -> simplexDim a s `shouldBe` d - 1)

checkOn :: (SSet a, Show (GeomSimplex a)) => a -> [GeomSimplex a] -> Spec
checkOn a gs = do
  it "should have the correct number of faces" $
    forM_
      gs
      ( \g ->
          let d = geomSimplexDim a g
           in when (d >= 1) $ length (geomFaces a g) `shouldBe` d + 1
      )
  it "faces should be valid simplices " $
    forM_ gs (checkFaces a)
  it "faces should have correct dimensions" $
    forM_ gs (checkDims a)
  it "should satisfy the simplicial identities " $
    forM_ gs (checkIdentities a)

check :: (FiniteType a, Show (GeomSimplex a)) => Int -> a -> Spec
check n a = do
  -- TODO compute the bases once instead of repeatedly
  it "basis simplices should be valid" $
    forM_ [0 .. n] (\i -> forM_ (geomBasis a i) (\g -> g `shouldSatisfy` isGeomSimplex a))
  it "basis simplices have correct dimension" $
    forM_ [0 .. n] (\i -> forM_ (geomBasis a i) (\g -> geomSimplexDim a g `shouldBe` i))
  it "should have the correct number of faces" $
    forM_ [1 .. n] (\i -> forM_ (geomBasis a i) (\g -> length (geomFaces a g) `shouldBe` (i + 1)))
  it "faces should be valid simplices " $
    forM_ [0 .. n] (\i -> forM_ (geomBasis a i) (checkFaces a))
  it "faces should have correct dimensions" $
    forM_ [0 .. n] (\i -> forM_ (geomBasis a i) (checkDims a))
  it "should satisfy the simplicial identities " $
    forM_ [0 .. n] (\i -> forM_ (geomBasis a i) (checkIdentities a))

checkMorphismFaces :: (SSet a, SSet b, Show (GeomSimplex a), Show (GeomSimplex b)) => a -> b -> Morphism a b -> GeomSimplex a -> Expectation
checkMorphismFaces a b m g = do
  let d = geomSimplexDim a g
      s = nonDegen g

  when (d > 0) $
    sequence_ $ do
      i <- [0 .. d]
      return $
        unless (m `onSimplex` (face a s i) == face b (m `onSimplex` s) i) $
          expectationFailure $ "Morphism did not commute with face " ++ show i ++ " of " ++ show g

  sequence_ $ do
    i <- [0 .. d]

    return $
      unless (m `onSimplex` (degen s i) == degen (m `onSimplex` s) i) $
        expectationFailure $ "Morphism did not commute with degen " ++ show i ++ " of " ++ show g

checkMorphismOn :: (SSet a, SSet b, Show (GeomSimplex a), Show (GeomSimplex b)) => a -> b -> Morphism a b -> [GeomSimplex a] -> Spec
checkMorphismOn a b m gs = do
  it "should have valid images" $
    forM_ gs (\g -> m `onGeomSimplex` g `shouldSatisfy` isSimplex b)

  it "should commute with faces and degeneracies" $
    forM_ gs (\g -> checkMorphismFaces a b m g)

checkTwistFaces :: (SSet a, SGrp b, Show (GeomSimplex a), Show (GeomSimplex b)) => a -> b -> Twist a b -> GeomSimplex a -> Expectation
checkTwistFaces a b m g = do
  let d = geomSimplexDim a g
      s = nonDegen g
      twistOn = twistOnFor a b

  when (d > 0) $ do
    sequence_ $ do
      i <- [1 .. d-1]
      return $
        unless (m `twistOn` face a s i == face b (m `twistOn` s) i) $
          expectationFailure $ "Morphism did not commute with face " ++ show i ++ " of " ++ show g ++ ", f∂" ++ show i ++ " = " ++ show (m `twistOn` face a s i) ++ " but " ++ "∂" ++ show i ++ "f = " ++ show (face b (m `twistOn` s) i)

    let i = 0
    unless (prodMor b `onSimplex` prodNormalise (m `twistOn` face a s 1, invMor b `onSimplex` (m `twistOn` face a s 0)) == face b (m `twistOn` s) 0) $
      expectationFailure $ "Morphism did not commute with face " ++ show i ++ " of " ++ show g

  -- sequence_ $ do
  --   i <- [0 .. d]

  --   return $
  --     unless (m `onSimplex` (degen s i) == degen (m `onSimplex` s) i) $
  --       expectationFailure $ "Morphism did not commute with degen " ++ show i ++ " of " ++ show g

checkTwistOn :: (SSet a, SGrp b, Show (GeomSimplex a), Show (GeomSimplex b)) => a -> b -> Twist a b -> [GeomSimplex a] -> Spec
checkTwistOn a b m gs = do
  let twistOn = twistOnFor a b

  it "should have valid images" $
    forM_ gs (\g -> m `twistOnGeom` g `shouldSatisfy` isSimplex b)

  it "should have images of the correct dimension" $
    forM_ gs (\g ->
        unless (geomSimplexDim a g == 0 || simplexDim b (m `twistOnGeom` g) == (geomSimplexDim a g - 1)) $
          expectationFailure $ "Image " ++ show (m `twistOnGeom` g) ++ " of " ++ show g ++ " is the wrong dimension"
             )

  it "should commute with faces and degeneracies" $
    forM_ gs (\g -> checkTwistFaces a b m g)
