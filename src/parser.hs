{-# LANGUAGE DeriveGeneric #-}
module Parser where

import Data.List
import Data.Char
import GHC.Generics

data Move = Move {
    coord :: [String]
    ,result :: Maybe String
    ,prev :: Maybe Move
    } deriving (Generic, Eq, Show)


mapToMove :: ([(String, [String])], [(String, String)]) -> Move
mapToMove ([("coord", [])], [("result", "HIT")]) = Move [] (Just "HIT") Nothing
mapToMove (a, b) = 
    let
        (_,ha) = head a
    in
        mapMove (tail a) b (Move ha Nothing Nothing)
    
mapMove :: [(String, [String])] -> [(String, String)] -> Move -> Move
mapMove [] [] move = move
mapMove a b move = 
    let
        (_,ha) = head a
        (_,hb) = head b
    in
        mapMove (tail a) (tail b) (Move ha (Just hb) (Just move))


encode :: Move -> String
encode moves = do
    encodeCoords moves


encodeCoords :: Move -> String
encodeCoords moves = encodeC moves ""
    where
        encodeC :: Move -> String -> String
        encodeC moves str = 
            case (prev moves) of
                Nothing -> "d5:coordl1:" ++ (head (coord moves)) ++ (show (length (last (coord moves)))) ++ ":" ++ (last (coord moves)) ++ "ee"
                Just prevMove -> do
                    let result = getMoveResult moves
                    case (coord moves) of
                        [] -> "d5:coordle4:prev" ++ (encodeC prevMove str) ++ "6:result" ++ (show (length (result))) ++ ":" ++ (result) ++ "e"
                        coords -> "d5:coordl1:" ++ (head (coord moves)) ++ (show (length (last (coord moves)))) ++ ":" ++ (last (coord moves)) ++ "e4:prev" ++ (encodeC prevMove str) ++ "6:result" ++ (show (length (result))) ++ ":" ++ (result) ++ "e"

getMoveResult :: Move -> String
getMoveResult move =
    case (result move) of
        Nothing -> ""
        Just a -> a
        
decode :: String -> Maybe Move
decode str = 
    let
        parsed = parseBencode str
    in
        case parsed of
            Right a -> Just (mapToMove a)
            Left err -> Nothing

parseBencode :: String -> Either String ([(String, [String])], [(String, String)])
parseBencode msg = 
    case parseDict msg [] of
        Left e -> Left e
        Right a -> 
            case a of
                Nothing -> Right ([("coord", [])], [("result", "HIT")])
                Just (d, leftovers) -> 
                    case leftovers of
                        "" -> 
                            let
                                moves = readMoves d
                                results = readResults d
                            in
                                if (validate moves)
                                    then if (validateResults results)
                                        then Right (moves, results)
                                        else Left "One or more result value are invalid. Valid values are MISS and HIT"
                                    else Left "One or more moves are invalid."
                        _ -> Left $ "Message has additional text: " ++ leftovers

parseDict :: String -> [String] -> Either String (Maybe ([String], String))
parseDict ('d':m) acc = readDict m acc 0
    where
        readDict ('e':l) acc a =
            if a /= 0
                then readDict l acc (a - 1 :: Int)
                else Right $ Just (reverse acc, l)
        readDict "" acc a = Left "String ending with 'e' expected"
        readDict ('l':'e':m) acc a = Right Nothing--Left "Game over"
        readDict ('l':m) acc a = readDict m acc (a + 1 :: Int)
        readDict ('d':m) acc a = readDict m acc (a + 1 :: Int)
        readDict z acc a =
            case parseStr z of
                Left e -> Left e
                Right (r, l) ->
                    case readDict l (r:acc) a of
                        Left e -> Left e
                        Right d -> 
                            case d of 
                                Nothing -> Right Nothing
                                Just (a, b) -> Right $ Just (a, b)
parseDict _ _ = Left "Dictionary starting with 'd' expected"

parseStr :: String -> Either String (String, String)
parseStr msg = readString len $ drop (length lengthString) msg
    where
        lengthString = takeWhile isDigit msg
        len :: Int
        len = read lengthString
        readString :: Int -> String -> Either String (String, String)
        readString n (':':m)= Right (take n m, drop n m)
        readString _ _ = Left "Invalid bencode. '<number>:' expected"

readMoves :: [String] -> [(String, [String])]
readMoves d = readMov d []
    where
        readMov [] a = (a)
        readMov z movs = do
            if (head z) == "coord"
                then readMov (drop 3 z) (("coord", [ (z !! 1), (z !! 2)]) : movs)
                else readMov (drop 1 z) movs

readResults :: [String] -> [(String, String)]
readResults d = readRes d []
    where
        readRes [] a = (reverse a)
        readRes a res = do
            if (head a) == "result"
                then readRes (drop 2 a) (("result", a !! 1): res)
                else readRes (drop 1 a) res

validate :: [(String, [String])] -> Bool
validate d = 
    let
        result = all validateCoords d
            where
                validateCoords (_, coords) = 
                    let
                        x = coords !! 0
                        y = read (coords !! 1)
                    in
                        if ("A" <= x && x <= "J") && (1 <= y && y <= 10)
                            then True
                            else False
        result1 =  
            let
                coords = getCoords d
                (a, b) = getCoordsPairs coords
            in
                if isDuplicated a
                    then False
                    else if isDuplicated b
                        then False
                        else True
    in
        (result && result1)

validateResults :: [(String, String)] -> Bool
validateResults d = 
    do
        all validMoves d
            where 
                validMoves (_, result) = 
                    case result of 
                        "HIT" -> True
                        "MISS" -> True
                        _ -> False

isDuplicated :: [[String]] -> Bool
isDuplicated d = isDup d
    where
        isDup [] = False
        isDup (x : others) =
            if elem x others
                then True
                else isDup others 
                

getCoords :: [(String, [String])] -> [[String]]
getCoords d =
    do
        map getC d
            where
                getC (_, coords) = coords


getCoordsPairs :: [[String]] -> ([[String]], [[String]])
getCoordsPairs d =  getPairs d 0 ([],[])
    where
        getPairs [] _ (a, b) = (reverse a, reverse b)
        getPairs (x:z) acc (a,b) =
            case even acc of
                True -> getPairs z (acc+1) (x:a, b)
                False -> getPairs z (acc+1) (a, x:b)

-- Splits data into two parts by moves
getDataPairs :: ([(String, [String])], [(String, String)]) -> (([(String, [String])], [(String, String)]), ([(String, [String])], [(String, String)]))
getDataPairs d =  getPairs d 0 (([("", [])], [("", "")]), ([("", [""])], [("", "")]))
    where
        getPairs :: ([(String, [String])], [(String, String)]) -> Int -> (([(String, [String])], [(String, String)]), ([(String, [String])], [(String, String)])) -> (([(String, [String])], [(String, String)]), ([(String, [String])], [(String, String)]))
        getPairs ([(_, [])], [(_, _)]) _ ((a,b),(c,d)) = ((reverse a, reverse b),(reverse c, reverse d))
        getPairs (x:m, []) acc ((a,b),(c,d)) = 
            case even acc of
                True -> ((reverse (x:a), reverse (("", ""):b)),(reverse c, reverse d))
                False -> ((reverse a, reverse b),(reverse (x:c), reverse (("", ""):d)))
        getPairs (x:m, y:n) acc ((a,b),(c,d)) =
            case even acc of
                True -> getPairs (m,n) (acc+1) ((x:a, y:b),(c,d))
                False -> getPairs (m,n) (acc+1) ((a,b),(x:c, y:d))



