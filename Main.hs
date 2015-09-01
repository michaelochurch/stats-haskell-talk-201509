{-# LANGUAGE RecordWildCards, TypeFamilies #-}

import Control.Monad
import qualified Data.Map as M
import Text.Printf

type FieldName = String

type Point = M.Map FieldName (Maybe Double)
type Label = Double

class DataPass a where
  type Init a
  create :: (Init a) -> a
  update :: a -> [(Point, Label)] -> a
  apply1 :: a ->  Point  ->  Point
  apply  :: a -> [Point] -> [Point]

data MeanImputer = MI {fieldNames :: [FieldName],
                       moments    :: [(Double, Int)]}

instance DataPass MeanImputer where
  type Init MeanImputer = [String]
  create = createMI
  update = updateMI
  apply datapass points = map (apply1 datapass) points
  apply1 = imputeMean

imputeMean :: MeanImputer -> Point -> Point
imputeMean mi point =
  foldr update point (zip (fieldNames mi) (moments mi))
  where update (fname, (d, n)) pt =
          case M.lookup fname pt of
           (Just (Just v)) -> pt
           _               -> M.insert fname (Just (d / fromIntegral n)) pt

createMI :: [FieldName] -> MeanImputer
createMI fieldNames = MI fieldNames $ replicate (length fieldNames) (0.0, 0)

updateMI1 :: MeanImputer -> (Point, Label) -> MeanImputer
updateMI1 mi@(MI {..}) (point, _) =
  mi {moments = moments'}
  where moments' = zipWith update fieldNames moments
        update fname (d, n) =
          case M.lookup fname point of
           Just (Just value) -> (d + value, n + 1)
           _                 -> (d        , n)

updateMI :: MeanImputer -> [(Point, Label)] -> MeanImputer
updateMI mi labeledPoints =
  foldl updateMI1 mi labeledPoints



testData :: [Point]
testData = [M.fromList [("A", Just  0), ("B", Just  1), ("C", Nothing), ("D", Just 4)], 
            M.fromList [("A", Nothing), ("B", Just  2), ("C", Just 14), ("D", Just 8)],
            M.fromList [("A", Just  1), ("B", Nothing), ("C", Nothing)               ], 
            M.fromList [("A", Nothing), ("B", Just  4), ("C", Just 22), ("D", Just 3)],
            M.fromList [("A", Just  0), ("B", Just  5), ("C", Just 11), ("D", Just 1)]]

testDataWithLabels :: [(Point, Label)]
testDataWithLabels = zip testData (repeat 0.0) -- labels not used here.

printDF :: [Point] -> IO ()
printDF = mapM_ printPoint
  where printPoint pt = line pt >> putStrLn ""
        line pt = forM_ (M.toList pt) $ \(fname, maybeVal) -> do
          putStr $ fname ++ "= "
          case maybeVal of
           Just v  -> printf "%6.2f" v
           Nothing -> putStr "  NA  "
          putStr "; "

demo :: IO ()
demo = do
  let dpEmpty = create ["A", "B", "D"] :: MeanImputer
      dpFull  = update dpEmpty testDataWithLabels
  putStrLn "Before imputation:"
  printDF testData
  putStrLn "After imputation: "
  printDF $ apply dpFull testData

main :: IO ()
main = demo
