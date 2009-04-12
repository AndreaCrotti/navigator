module Main where
    
import Char
import Random
import System.FilePath
import IO
import Text.ParserCombinators.Parsec
import Data.Map as Map

stradario = "stradario.txt"
percorso = "percorso.txt"

type City = String
type Dist = Int
type Inter = (Dist, [City])
type MinDist = Map (City, City) Inter
type Table = Map (City, City) Dist

main = do
    (n, c, d) <- parseInput
    return $ (mkMap n c d, c)
    
-- FIXME separate pure code from impure code
parseInput :: IO (Int, [City], [Dist])
parseInput = do
    strad <- getInputDotted "stradario.txt"
    path <- getInputDotted "percorso.txt"
    let n = read (strad !! 0)::Int
    let cities = take n $ tail strad
    let dists = [read (strad !! i)::Int | i <- [(n+1)..(length strad - 1)]]
    return (n, cities, dists)

mkMap :: Int -> [String] -> [Int] -> Table
mkMap n cities dists =
    let tabCities = [ (x,y) | x <- cities, y <- cities ]
    in 
        -- zipping all possible combinations of cities with the distance
        Map.fromList $ zip tabCities dists

getDist :: City -> City -> Table -> Dist
getDist c1 c2 hash =
    case (Map.lookup (c1, c2) hash) of 
        Just x -> x
        Nothing -> error "this shouldn't happen"
            -- case (Map.lookup (c2, c2) hash) of
            --     Just x -> x
            --     Nothing -> error "nothing found, this shouldn't happen"

compose :: Inter -> Inter -> Inter
compose i1 i2 = (fst i1 + fst i2, snd i1 ++ snd i2)

-- handles negative distances => no link between cities
minDist :: Dist -> Dist -> Dist
minDist d1 d2
    | d1 < 0 = d2
    | d2 < 0 = d1
    | otherwise = min d1 d2

-- floyd warshall algorithm
-- floyd :: Int -> Int -> MinDist -> MinDist
-- floyd n k t
--     | n == k = t
--     | otherwise = 
--         let old = t
--         in

-- split with a delimiter, really haskellian
splitBy :: (a -> Bool) -> [a] -> [[a]] 
splitBy _ [] = [] 
splitBy f list =  first : splitBy f (dropWhile f rest) where 
  (first, rest) = break f list


getInputDotted :: FilePath -> IO [String]
getInputDotted file = do
    input <- readFile file
    let splitted = splitBy (== '.') input
    return splitted
