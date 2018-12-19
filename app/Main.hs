{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, DeriveGeneric #-}
module Main (main) where 

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad.Reader

import Data.Default.Class
import Data.String
import Data.Text.Lazy (Text, pack)
import qualified Data.List as DL
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Map (Map)
import qualified Data.Map as M

import Network.Wai.Middleware.RequestLogger
import Network.HTTP.Types

import Prelude

import GHC.Generics
import Data.Aeson (ToJSON, FromJSON)
import qualified Data.Aeson as JSON
import Parser
import Bot

import Web.Scotty.Trans


data Game = Game { move :: Move, gameID :: String }

instance ToJSON Move
instance FromJSON Move

newtype GameState = GameState { game :: Map String Game }

instance Default GameState where
    def = (GameState (M.fromList [("", (Game (Move ["", ""] Nothing Nothing) ""))]))

newtype GameM a = GameM { runGameM :: ReaderT (TVar GameState) IO a }
    deriving (Applicative, Functor, Monad, MonadIO, MonadReader (TVar GameState))


gameM :: MonadTrans t => GameM a -> t GameM a
gameM = lift


gets :: (GameState -> b) -> GameM b
gets f = ask >>= liftIO . readTVarIO >>= return . f

modify :: (GameState -> GameState) -> GameM ()
modify f = ask >>= liftIO . atomically . flip modifyTVar' f

main :: IO ()
main = do
    sync <- newTVarIO def
    let runActionToIO m = runReaderT (runGameM m) sync

    scottyT 3000 runActionToIO app


app :: ScottyT Text GameM ()
app = do
    -- -- DEBUGGING
    -- middleware logStdoutDev

    get "/game/:bar/player/A" $ do
        content <- header "Accept"
        case content of
            Nothing -> do
                status status400
                text $ pack $ "Unsupported accept header"
            Just contentType -> do
                gid <- param "bar"
                gameMap <- gameM $ gets game
                case getGame gid gameMap of
                    Nothing -> do
                        status status400
                        text $ pack $ "Game with id '" ++ gid ++ "' not started"
                    Just mov -> do
                        if (coord mov) == []
                            then do
                                liftIO $ putStrLn "\nGame over. Player A won."
                                liftIO $ putStrLn $ "Final results for game '" ++ gid ++ "' :\n" ++ encode mov
                                setHeader "Connection" "close"
                                raw $ handleEncoding contentType mov
                            else 
                                raw $ handleEncoding contentType mov
        

    post "/game/:bar/player/A" $ do
        gid <- param "bar"
        b <- body
        content <- header "Content-Type"
        
        liftIO $ putStrLn $ "\nGame '" ++ gid ++ "':"
        case (handleDecoding content b) of
            Left err -> 
                if (err == "Game over")
                    then do
                        liftIO $ putStrLn "Game over. Server won"
                        liftIO $ putStrLn $ "Final results for game '" ++ gid ++ "' :\n" ++ (L8.unpack b)
                        status status204
                    else do
                        status status400
                        raw $ L8.pack err
            Right moves -> do
                let (madeMoves, board) = getMoveData moves
                let move_result = checkMoves (coord moves) board
                gameMap <- gameM $ gets game
                res <- liftIO (doGame gameMap move_result (madeMoves, board) moves)
                case res of
                    Left err -> do
                        status status400
                        raw $ L8.pack err
                    Right mov -> do
                        x <- postGame gid mov
                        gameM $ modify $ x
                        status status204

doGame :: (Map String Game) -> Maybe (String, [[String]]) -> ([[String]], [[String]]) -> Move-> IO (Either String Move)
doGame gameMap move_result (madeMoves, board) moves = 
    case move_result of
        Nothing -> do
            return $ Right $ (Move [] (Just "HIT") (Just moves))
        Just (res, newBoard) -> do
            liftIO $ putStrLn $ "Player A made move " ++ (DL.intercalate "" (coord moves)) ++ ". It's result is " ++ res
            case makeMove madeMoves of
                Left err -> return $ Left err
                Right updMoves -> do
                    let newMove = Move (DL.head updMoves) (Just res) (Just moves)
                    liftIO $ putStrLn $ "Made move " ++ (DL.intercalate "" (coord newMove))
                    return $ Right $ newMove

getGame :: String -> (Map String Game) -> Maybe Move
getGame gid gameMap = do
    case M.lookup gid gameMap of
        Just gm -> Just $ move gm
        Nothing -> Nothing

postGame :: String -> Move -> ActionT Text GameM (GameState -> GameState)
postGame gid mov = do
    gameMap <- gameM $ gets game
    let gm = (Game mov gid)
    let newMap = M.insert gid gm gameMap
    return (\ st -> st { game = newMap })

handleDecoding :: Maybe Text -> ByteString -> Either String Move
handleDecoding content reqBody = 
    case content of
        Nothing -> Left ""
        Just contentType -> 
            case (decode (L8.unpack reqBody)) <|> (JSON.decode reqBody) of
                Nothing -> Left "Parsing error"
                Just moves ->
                    if (coord moves) == []
                        then Left "Game over"
                        else Right moves

handleEncoding :: Text -> Move -> ByteString
handleEncoding content mov = 
    case content of
        "application/bencoding" -> L8.pack $ encode mov
        "application/json" -> JSON.encode mov
