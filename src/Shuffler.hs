module Shuffler where

import Common
import System.Random
import System.Random.Shuffle
import Data.List

shuffleDeck :: State -> IO State

-- TODO: Implement a random shuffling algorithm
shuffleDeck state@State{
					players = ps,
					deck = d,
					d_stack = dstack,
					e_players = e,
					cur_player = c
				   } = return State {
				   						players = ps,
				   						deck = shuffleDeck' d,
				   						d_stack = dstack,
				   						e_players = e,
				   						cur_player = c
									}

shuffleDeck' :: Deck -> Deck
shuffleDeck' d = shuffle' d (length d) s

s = mkStdGen 1000



-- shuffleDeck :: State -> IO State

-- -- TODO: Implement a random shuffling algorithm
-- shuffleDeck state = return (state)
