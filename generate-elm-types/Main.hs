module Main where

import "elm-street" Elm (generateElm, defaultSettings)
import "monadic-games-club" Game.Skulls.Model

type Types =
  '[ Suit
   , Card
   , PlayerId
   , BetData
   , PlayerCount
   , InitialData
   , TurnData
   , PassData
   , Command
   , Event
   , StateData
   , HitSkull
   , BetStateData
   , ResolvingBetStateData
   , GameError
   , StateBlob
   ]

main :: IO ()
main = generateElm @Types
  $ defaultSettings "ui/src" ["Game", "Skulls", "Generated"]
