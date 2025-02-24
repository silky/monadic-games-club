{-# language DuplicateRecordFields #-}
{-# language OverloadedRecordDot   #-}

module Main where

import "aeson" Data.Aeson hiding (json)
import "base" Control.Monad.IO.Class (MonadIO, liftIO)
import "base" Data.Functor.Identity (Identity)
import "base" Data.Functor.Identity (runIdentity)
import "crem" Crem.StateMachine (StateMachineT (..), run)
import "monadic-games-club" Game.Skulls.Model
import "servant" Servant.API ((:>),)
import "servant-server" Servant (Server, serve, Proxy(Proxy))
import "servant-websockets" Servant.API.WebSocket (WebSocket)
import "wai" Network.Wai (Application)
import "warp" Network.Wai.Handler.Warp qualified as Warp
import "websockets" Network.WebSockets.Connection (Connection)
import "websockets" Network.WebSockets.Connection qualified as WS

-- * Game setup

initGame :: StateMachineT Identity Command GameResult
initGame = Basic $ skullsMachine

-- * API server

type Api = "game" :> WebSocket

server :: Server Api
server = playGame initGame
 where
  -- TODO: I think we need the 'pingThread' so the connection doesn't go
  -- stale.
  playGame :: MonadIO m
           => StateMachineT Identity Command GameResult
           -> Connection
          -> m ()
  playGame machine conn = do
    liftIO $ WS.receiveData conn >>= \json -> do
      case eitherDecode' json of
        Left err -> do
          liftIO $ putStrLn err
          playGame machine conn
        Right command -> do
          let (output, machine') = runIdentity $ run machine command
          WS.sendTextData conn (encode output)
          case output of
            GameResult (Right (WonGame, _)) -> putStrLn "Winner!"
            _ -> playGame machine' conn

-- * WAI/Servant busywork

app :: Application
app = do
  serve @Api Proxy server

main :: IO ()
main = do
  putStrLn "Listening on 8008"
  Warp.run 8008 app
