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
					d_stack = dstack
				   } = return State {
				   						players = ps,
				   						deck = shuffleDeck' d,
				   						d_stack = dstack
									}

shuffleDeck' :: Deck -> Deck
shuffleDeck' d = shuffle' d (length d) s

s = mkStdGen 1000

-- shuffled :: (RandomGen gen) => gen -> [Card]
-- shuffled = shuffle' initDeck 153