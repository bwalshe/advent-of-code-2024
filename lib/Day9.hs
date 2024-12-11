module Day9 where


import Data.Text (Text)


fillBlocks :: Int -> Int -> [Maybe Int]
fillBlocks fileNo n = replicate n $ Just fileNo

emptyBlocks :: Int -> Int -> [Maybe Int]
emptyBlocks _ n = replicate n Nothing

data ExpandDiskMapState = {getBlocks :: [[Maybe Int]], nextFn :: Int -> [MaybeInt]}
expandDiskMap :: ExpandDiskMapState -> Text -> [Maybe Int]
expandDiskMap 
