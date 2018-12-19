{-# LANGUAGE OverloadedStrings #-}
module Bot
    ( nextMove, checkMoves, makeMove, getMoveData
    ) where

import Data.Text
import Data.Char
import Data.List as DL
--import Network.HTTP.Client

import Parser
--import HTTPhelper

boardFinal ::  [[String]]
boardFinal =
    [["B", "3"], ["B", "4"], ["B", "5"], ["C", "4"], ["D", "4"],
    ["C", "8"], ["C", "9"], ["C", "10"], ["B", "10"], ["D", "10"],
    ["E", "6"], ["F", "6"], ["G", "5"], ["G", "6"], ["G", "7"],
    ["G", "2"], ["H", "2"], ["I", "1"], ["I", "2"], ["I", "3"],
    ["H", "9"], ["I", "7"], ["I", "8"], ["I", "9"], ["J", "9"]]
boardFinalAlt =
    [["B", "3"], ["B", "4"], ["B", "5"], ["C", "4"], ["D", "4"],
    ["C", "8"], ["C", "9"], ["C", "10"], ["B", "10"], ["D", "10"],
    ["G", "6"], ["H", "6"], ["I", "5"], ["I", "6"], ["I", "7"],
    ["G", "2"], ["H", "2"], ["I", "1"], ["I", "2"], ["I", "3"],
    ["F", "9"], ["F", "10"], ["F", "8"], ["G", "9"], ["H", "9"]]


-- boardFinal = [["A", "2"], ["A", "3"], ["A", "4"]]
-- boardFinalAlt = [["A", "8"], ["A", "9"], ["A", "10"]] 

    {-
runMain :: IO ()
runMain = do
    putStrLn "Enter game id"
    gameID <- getLine
    putStrLn "Select role (A or B)"
    playerID <- getLine
    case [(Data.Char.toUpper (DL.head playerID))] of
        "A" -> playA gameID [(Data.Char.toUpper (DL.head playerID))]
        "B" -> playGame gameID [(Data.Char.toUpper (DL.head playerID))] boardFinal []
        _ -> putStrLn "Invalid role"

playA :: String -> String -> IO ()
playA gameID playerID = do
    result <- sendPost gameID playerID (Move ["A", "1"] Nothing Nothing)
    case result of
        Left err -> putStrLn err
        Right res -> do
            putStrLn res
            playGame gameID playerID boardFinalAlt [["A", "1"]]


playGame :: String -> String -> [[String]] -> [[String]] -> IO ()
playGame gameID playerID board madeMoves = do
    result <- sendGet gameID playerID
    case result of
        Left err -> 
            if (err == "Game over")
                then
                    putStrLn "==============================\nGame over. Congratulations! You won!"
                else
                    putStrLn err
        Right moves -> do
            let move_result = checkMoves (coord moves) board
            case move_result of
                Nothing -> do
                    result <- sendPost gameID playerID (Move [] (Just "HIT") (Just moves))
                    case result of
                        Left err -> putStrLn err
                        Right res -> putStrLn "==============================\nGame over. You lost."
                Just (res, newBoard) -> do
                    putStrLn $ "Other player made move " ++ (DL.intercalate "" (coord moves)) ++ ". It's result is " ++ res
                    case makeMove madeMoves of
                        Left err -> putStrLn err
                        Right updMoves -> do
                            result <- sendPost gameID playerID (Move (DL.head updMoves) (Just res) (Just moves))
                            case result of
                                Left err -> putStrLn err
                                Right res -> do
                                    putStrLn res
                                    playGame gameID playerID newBoard updMoves

-}
   
           
checkMoves :: [String] -> [[String]] -> Maybe (String, [[String]])
checkMoves coords board = 
    let
        result =
            if elem coords board
                then "HIT"
                else "MISS"
        newBoard = delete coords board
    in
        if newBoard == []
            then Nothing
            else (,) <$> Just result <*> Just newBoard

makeMove :: [[String]] -> Either String [[String]]
makeMove [] = Right (["A", "1"] : [])
makeMove madeMoves = makeMov madeMoves (nextMove (DL.head madeMoves))
    where
        makeMov :: [[String]] -> [String] -> Either String [[String]]
        makeMov madeMoves [] = Left "No available moves left"
        makeMov madeMoves coords = Right (coords : madeMoves)

nextMove :: [String] -> [String]
nextMove [x, y] = 
    if ((read y :: Int) <= 9)
        then [x, show ((read y :: Int) + 1)]
        else if (x <= "I")
            then [[(chr (ord (DL.head x) + 1))], "1"]
            else []

            
getMoveData :: Move -> ([[String]], [[String]])
getMoveData move = 
    case (prev move) of
        Nothing -> (DL.reverse [], boardFinal)
        Just prevMove -> getMoves prevMove ([], boardFinal) 2
    where
        getMoves move (madeMoves, board) acc = 
            case (even acc) of
                True ->
                    case (prev move) of
                        Nothing -> (DL.reverse madeMoves, board)
                        Just prevMove -> getMoves prevMove ((coord move):madeMoves, board) (acc+1)
                False -> 
                    let 
                        newBoard = checkMove move board
                    in 
                        case (prev move) of
                            Nothing -> (DL.reverse madeMoves, newBoard)
                            Just prevMove -> getMoves prevMove (madeMoves, newBoard) (acc+1)

checkMove :: Move -> [[String]] -> [[String]]
checkMove move board = delete (coord move) board
