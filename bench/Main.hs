module Main (main) where

import qualified Data.Map.Lazy as ML
import qualified Data.IntMap.Lazy as IML
import qualified Data.Set as S
import qualified Data.IntSet as IS
import qualified Data.HashMap.Lazy as HML
import qualified Data.HashSet as HS
import Data.Text (Text, pack)
import Gauge

main :: IO ()
main = defaultMain
    [ bgroup "IntMap->Map" intMapToMapBench
    , bgroup "HashMap->Map" hashMapToMapBench
    , bgroup "IntSet->Set" intSetToSetBench
    , bgroup "HashSet->Set" hashSetToSetBench
    ]

----------------------------------------------------------------------------
-- IntMap->Map
----------------------------------------------------------------------------

intMapToMapBench :: [Benchmark]
intMapToMapBench =
    [ bench "foldr" $
          whnf (IML.foldrWithKey ML.insert mempty) sampleIntMap
    , bench "foldl'" $
          whnf (IML.foldlWithKey' (\m k v -> ML.insert k v m) mempty) sampleIntMap
    , bench "list" $
          whnf (ML.fromDistinctAscList . IML.toAscList) sampleIntMap
    ]

sampleIntMap :: IML.IntMap ()
sampleIntMap = IML.fromList [(i, ()) | i <- [1..1000 :: Int]]

----------------------------------------------------------------------------
-- HashMap->Map
----------------------------------------------------------------------------

hashMapToMapBench :: [Benchmark]
hashMapToMapBench =
    [ -- There was a possibility that for 'Int' the list variant would be
      -- faster because 'hash' is monotonic for 'Int', but the bencmarks
      -- show it's not faster
      bgroup "Int"
          [ bench "foldr" $
                whnf (HML.foldrWithKey ML.insert mempty) sampleHashMapInt
          , bench "foldl'" $
                whnf (HML.foldlWithKey' (\m k v -> ML.insert k v m) mempty) sampleHashMapInt
          , bench "list" $
                whnf (ML.fromList . HML.toList) sampleHashMapInt
          ]
    , bgroup "Text"
          [ bench "foldr" $
                whnf (HML.foldrWithKey ML.insert mempty) sampleHashMapText
          , bench "foldl'" $
                whnf (HML.foldlWithKey' (\m k v -> ML.insert k v m) mempty) sampleHashMapText
          , bench "list" $
                whnf (ML.fromList . HML.toList) sampleHashMapText
          ]
    ]

sampleHashMapInt :: HML.HashMap Int ()
sampleHashMapInt = HML.fromList [(i, ()) | i <- [1..1000 :: Int]]

sampleHashMapText :: HML.HashMap Text ()
sampleHashMapText = HML.fromList [(pack (show i), ()) | i <- [1..1000 :: Int]]

----------------------------------------------------------------------------
-- IntSet->Set
----------------------------------------------------------------------------

intSetToSetBench :: [Benchmark]
intSetToSetBench =
    [ bench "foldr" $
          whnf (IS.foldr S.insert mempty) sampleIntSet
    , bench "foldl'" $
          whnf (IS.foldl' (flip S.insert) mempty) sampleIntSet
    , bench "list" $
          whnf (S.fromDistinctAscList . IS.toAscList) sampleIntSet
    ]

sampleIntSet :: IS.IntSet
sampleIntSet = IS.fromList [1..1000 :: Int]

----------------------------------------------------------------------------
-- HashSet->Set
----------------------------------------------------------------------------

hashSetToSetBench :: [Benchmark]
hashSetToSetBench =
    [ bench "foldr" $
          whnf (HS.foldr S.insert mempty) sampleHashSet
    , bench "foldl'" $
          whnf (HS.foldl' (flip S.insert) mempty) sampleHashSet
    , bench "list" $
          whnf (S.fromList . HS.toList) sampleHashSet
    ]

sampleHashSet :: HS.HashSet Text
sampleHashSet = HS.fromList [pack (show i) | i <- [1..1000 :: Int]]
