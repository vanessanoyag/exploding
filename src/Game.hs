module Game where

import Common

initGame :: Int -> State

-- [AiPlayer "Player 1" [], AiPlayer "Player 2" [], AiPlayer "Player 3" [], AiPlayer "Player 4" []]

-- TODO: Implement a method to initialize a new game given n players
initGame n = State { players = take n $ repeat (AiPlayer "Player" []),
                     deck = fullDeck,
                     d_stack = []}

-- TODO: Implement a method to setup the game
setupGame :: State -> IO State
setupGame gs = return (gs)
