module Board
    ( Board
    , build
    , rows
    , score
    , initialize
    , placeRandomCell
    , moveUp
    , moveDown
    , moveLeft
    , moveRight
    , isWon
    , isLost
    ) where

import Data.List
import System.Random

{-
    Definire tipul 'Board', astfel incat sa retina informaii despre tabla
    de joc și despre scorul curent.
-}

data Board = Board {
    rows :: [[Int]],
    score :: Int
} deriving Eq

--    Instantiere clasa 'Show' cu tipul 'Board'. 

instance Show Board where
    show (Board rows score) = (foldl (\acc line -> acc ++ showLine line ++ "\n") "" rows) ++ "Score: " ++ show(score) where
        showLine line = foldl (\acc el -> if el /= 0 then acc ++ (take (4 - (length $ show el)) (repeat ' ')) ++ show(el) ++ " |" else acc ++  "   . |") "" line

{-  
    Construire tabla de joc pe baza unei configurații, furnizate pe linii,
    și a unui scor.
-}

build :: [[Int]] -> Int -> Board
build rows score = Board rows score

--    Plaseaza aleator o celula pe tabla de joc, celulele existente nemodificandu-se.

find_zeros_matrix :: [[Int]] -> [(Int, Int)]
find_zeros_matrix matrix = 
    find_zeros_acc matrix 0 [] where
        find_zeros_acc matrix idx indexes =
            if length matrix == 0 then indexes else
                find_zeros_acc (tail matrix) (idx + 1) (indexes ++ (map (\x -> (idx, x)) (findIndices (== 0) $ head matrix)))

modify_row :: (RandomGen g) => [Int] -> Int -> g -> [Int]
modify_row lst idx g =
    if idx == 0 then
        [2 * fst (randomR (1, 2) g)] ++ tail lst else
        (head lst) : modify_row (tail lst) (idx - 1) g

modify_rows :: (RandomGen g) => [[Int]] -> (Int, Int) -> g -> [[Int]]
modify_rows m pos g = 
    if fst pos == 0 then modify_row (head m) (snd pos) g : (tail m) else
    (head m) : modify_rows (tail m) ((fst pos) - 1, snd pos) g

placeRandomCell :: RandomGen g => Board -> g -> (Board, g)
placeRandomCell b g = (build (modify_rows (rows b) (rnd_index) g) (score b), g) where
    zeros = find_zeros_matrix (rows b) 
    rnd_index = zeros !! (fst $ randomR (0, (length zeros) - 1) g)  
    
--    Genereaza aleator o tabla de joc cu doua celule ocupate.

initialize :: RandomGen g => g -> (Board, g)
initialize g = placeRandomCell (fst $ placeRandomCell (build (take 4 $ repeat [0,0,0,0]) 0) g) g

--    Implementez cele patru mutari posibile: stanga, dreapta, sus, jos.

mergeLeft :: [Int] -> ([Int], Int)
mergeLeft row = ((fst merged) ++ (take (4 - (length $ fst merged)) $ repeat 0), snd merged) where
    pozitive = filter (/= 0) row
    merged = mergeLeft1 0 0 where
        mergeLeft1 :: Int -> Int -> ([Int], Int)
        mergeLeft1 idx scor = if idx >= ((length pozitive) - 1) then 
            (snd $ splitAt idx pozitive, scor) else
                if (pozitive !! idx == pozitive !! (idx + 1)) then
                    ((2 * (pozitive !! idx)) : fst (mergeLeft1 (idx + 2) (scor + 2 * (pozitive !! idx))),
                     snd (mergeLeft1 (idx + 2) (scor + 2 * (pozitive !! idx)))) else
                    ((pozitive !! idx) : fst (mergeLeft1 (idx + 1) scor), (snd (mergeLeft1 (idx + 1) scor)))  

moveLeft :: Board -> Board
moveLeft b = build (map (fst) mergedRows) ((score b) + foldl (\acc el -> acc + (snd el)) 0 mergedRows) where
    mergedRows = map (mergeLeft) (rows b)
         
moveUp :: Board -> Board
moveUp b = build (transpose $ rows $ left_b) (score left_b) where
    left_b = moveLeft $ build (transpose $ rows b) (score b)

mirror :: [[Int]] -> [[Int]]
mirror = map (reverse)

moveRight :: Board -> Board
moveRight b = build (mirror $ rows right_b) (score right_b) where
    right_b = moveLeft $ build (mirror $ rows b) (score b)

moveDown :: Board -> Board
moveDown b = build (transpose $ rows down_b) (score down_b) where
    down_b = moveRight $ build (transpose $ rows b) (score b)

--    Intoarce 'True' daca tabla contine o configuratie castigatoare.

isWon :: Board -> Bool
isWon b = (/= 0) $ length $ foldl (++) [] $ map (findIndices (== 2048)) $ rows b 

--    Intoarce 'True' daca tabla contine o configuratie in care jucatorul pierde.

isLost :: Board -> Bool
isLost b = (== 4) $ length $ findIndices (== b) $ map (\move -> move b) [moveLeft, moveRight, moveUp, moveDown]  
