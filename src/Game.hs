module Game where

import Common
import Shuffler

initialCardCount :: Int
initialCardCount = 5

initGame :: Int -> State

-- TODO: Implement a method to initialize a new game given n players
initGame n = State { players = take n $ repeat (AiPlayer "Player" []),
                     deck = initDeck,
                     d_stack = [ ] }

-- TODO: Implement a method to setup the game
setupGame :: State -> IO State
setupGame gs@State{players = ps, deck = d, d_stack = ds} 
		= return (setup gs)

setup :: State -> State
setup gs@State{ players = ps, deck = d, d_stack = ds }
		= State{ players = deal d ps,
					deck = (drop 16 (shuffleDeck' d)) ++ (drop (length ps) defuseCards) ++ (take (length ps - 1) explodingCards),
				 d_stack = take 1 explodingCards
				}

deal :: Deck -> [Player] -> [Player]
deal [] _ = []
deal _ [] = []
deal d (p:ps) = p{hand = (take 4 d) ++ take 1 defuseCards} : deal (drop 4 d) ps