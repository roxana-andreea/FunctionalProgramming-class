module AIEmpty where

import Board
import Interactive
import Data.List

{-
    Intoarce tabla rezultata din aplicarea acelei mutari care maximizeaza
    numarul de celule libere.
-}

move :: Board -> Board
move b = states !! bstStateIndex where
    states = map (\function -> function b) [moveLeft, moveRight, moveUp, moveDown]
    zerosCount = map (foldl (+) 0 . map (length . filter (== 0))) $ map rows states
    bstStateIndex = head $ findIndices (== maximum zerosCount) zerosCount
        
{-
    Urmareste pas cu pas evolutia jocului, conform strategiei implementate.
-}
userMode :: IO ()
userMode = ai move
