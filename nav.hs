module Main where
    
import Char
import Random
import System.FilePath
import IO
import Text.ParserCombinators.Parsec
import Data.Graph
import Data.Map

stradario = "stradario.txt"
percorso = "percorso.txt"

main = do
    parseInput
    
parseInput = do
    strad <- getInputDotted "stradario.txt"
    path <- getInputDotted "percorso.txt"
    -- check consistency of the strad
    let n = read (strad !! 0)::Int
    let cities = take n $ tail strad
    let dists = [read (strad !! i)::Int | i <- [(n+1)..(length strad - 1)]]
    return (n,cities,dists)




-- split with a delimiter, really haskellian
splitBy :: (a -> Bool) -> [a] -> [[a]] 
splitBy _ [] = [] 
splitBy f list =  first : splitBy f (dropWhile f rest) where 
  (first, rest) = break f list


getInputDotted file = do
    input <- readFile file
    let splitted = splitBy (== '.') input
    return splitted
