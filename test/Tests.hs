{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE RecordWildCards #-}

import Test.Hspec (Spec, describe, it, shouldBe, shouldNotBe, pending, pendingWith)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)
import System.Console.ANSI (Color(Black, Red, Green, Yellow, Blue, Magenta, Cyan, White))

import ExplodingI
import Common
import Shuffler
import Game

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs = describe "exploding" $ do
  interactiveSpecs
  commonSpecs
  shufflerSpecs
  gameSpecs

interactiveSpecs :: Spec
interactiveSpecs = describe "explodingInteractive" $ do
  it "Say 'Hello' if input == 1" $ do
    (explodingInteractive 1) `shouldBe` "Hello"
  it "Say 'Hello World' if input == 2" $ do
    (explodingInteractive 2) `shouldBe` "Hello World"

commonSpecs :: Spec
commonSpecs = describe "Common" $ do
  describe "Full deck has..." $ do
    it "56 cards" $ do
      (length fullDeck) `shouldBe` 56
    it "4 exploding cards" $ do
      (length $ fullDeck `getCards` ExplodingCard) `shouldBe` 4
    it "6 defuse cards" $ do
      (length $ fullDeck `getCards` DefuseCard) `shouldBe` 6
    it "5 nope cards" $ do
      (length $ fullDeck `getCards` NopeCard) `shouldBe` 5
    it "4 attack cards" $ do
      (length $ fullDeck `getCards` AttackCard) `shouldBe` 4
    it "4 skip cards" $ do
      (length $ fullDeck `getCards` SkipCard) `shouldBe` 4
    it "4 favor cards" $ do
      (length $ fullDeck `getCards` FavorCard) `shouldBe` 4
    it "4 shuffle cards" $ do
      (length $ fullDeck `getCards` ShuffleCard) `shouldBe` 4
    it "5 future cards" $ do
      (length $ fullDeck `getCards` FutureCard) `shouldBe` 5
    it "20 cat cards" $ do
      (length [ c | c <- fullDeck, isCatCard c ]) `shouldBe` 20
    it "4 Taco cats" $ do
      (length $ fullDeck `getCards` TacoCat) `shouldBe` 4
    it "4 Melon cats" $ do
      (length $ fullDeck `getCards` MelonCat) `shouldBe` 4
    it "4 Potato cats" $ do
      (length $ fullDeck `getCards` PotatoCat) `shouldBe` 4
    it "4 Beard cats" $ do
      (length $ fullDeck `getCards` BeardCat) `shouldBe` 4
    it "4 Rainbow cats" $ do
      (length $ fullDeck `getCards` RainbowCat) `shouldBe` 4

  describe "Cards..." $ do
    it "MelonCat is a cat card" $ do
      isCatCard MelonCat `shouldBe` True

  describe "Utils..." $ do
    it "should take exploding cards" $ do
      let (h1, h2) = [ ExplodingCard ] `takeCards` explodingHand
      h1 `shouldBe` [ ExplodingCard ]
    it "should ONLY take 1 exploding cards" $ do
      let (h1, h2) = [ ExplodingCard ] `takeCards` explodingHand
      h2 `shouldBe` [ NopeCard, MelonCat, TacoCat ]
    it "should take exploding and defuse cards" $ do
      let (h1, h2) = [ ExplodingCard, DefuseCard ] `takeCards` defusableHand
      h1 `shouldBe` [ ExplodingCard, DefuseCard ]
    it "should ONLY take exploding and defuse cards" $ do
      let (h1, h2) = [ ExplodingCard, DefuseCard ] `takeCards` defusableHand
      h2 `shouldBe` [ BeardCat, RainbowCat, AttackCard ]

shufflerSpecs :: Spec
shufflerSpecs = describe "Shuffler" $ do
  it "State has 2 players" $ do
    length (players demoState) `shouldBe` 2
  it "State has 56 cards" $ do
    length (deck demoState) `shouldBe` 56
  it "Shuffled to the same number of cards" $ do
    -- pendingWith "Implement shuffleDeck function"
    demoState' <- shuffleDeck demoState
    length (deck demoState) `shouldBe` 56
  it "Shuffled to different positions" $ do
    -- pendingWith "Implement shuffleDeck function"
    let originalDeck = deck demoState
    demoState' <- shuffleDeck demoState
    (deck demoState') `shouldNotBe` originalDeck

gameSpecs :: Spec
gameSpecs = describe "Game" $ do
  describe "initGame" $ do
    it "should create 4 players" $ do
      -- pendingWith "Implement the initGame function"
      let gs = initGame numberOfTestPlayers
      length (players gs) `shouldBe` numberOfTestPlayers
    it "should initialize the deck with 46 cards" $ do
      -- pendingWith "Implement the initGame function"
      let gs = initGame numberOfTestPlayers
      length (deck gs) `shouldBe` 46
    it "should initialize discard pile to empty" $ do
      -- pendingWith "Implement the initGame function"
      let gs = initGame numberOfTestPlayers
      length (d_stack gs) `shouldBe` 0
    it "should set noPlayer as current player" $ do
      -- pendingWith "Implement the initGame function"
      let gs = initGame numberOfTestPlayers
      (cur_player gs) `shouldBe` noPlayer

  describe "setupGame" $ do
    it "should shuffle the deck" $ do
      -- pendingWith "Implement the setupGame function"
      let gs = initGame numberOfTestPlayers
      gs' <- setupGame gs
      (deck gs') `shouldNotBe` (deck gs)
    it "should set the deck properly" $ do
      -- pendingWith "Implement the setupGame function"
      let gs = initGame numberOfTestPlayers
      gs' <- setupGame gs
      (length $ deck gs') `shouldBe` 35
    it "should set the deck with enough exploding cards" $ do
      -- pendingWith "Implement the setupGame function"
      let gs = initGame numberOfTestPlayers
      gs' <- setupGame gs
      (length $ getCards (deck gs') ExplodingCard) `shouldBe` 3
    it "should set the discard stack with 1 exploding card" $ do
      -- pendingWith "Implement the setupGame function"
      let gs = initGame numberOfTestPlayers
      gs' <- setupGame gs
      (length $ getCards (d_stack gs') ExplodingCard) `shouldBe` 1
    it "each player should have 1 defuse card" $ do
      -- pendingWith "Implement the setupGame function"
      let gs = initGame numberOfTestPlayers
      gs' <- setupGame gs
      and (map (\p -> (length $ getCards (hand p) DefuseCard) == 1) $ players gs') `shouldBe` True
    it "should distribute cards to players" $ do
      -- pendingWith "Implement the setupGame function"
      let gs = initGame numberOfTestPlayers
      gs' <- setupGame gs
      and (map (\p -> (length $ hand p) == initialCardCount) $ players gs') `shouldBe` True

  describe "pickNextPlayer" $ do
    it "should pick first player" $ do
      -- pendingWith "Implement getNextPlayer function"
      let gs = initGame numberOfTestPlayers
      gs' <- setupGame gs
      gs' <- pickNextPlayer gs'
      (cur_player gs') `shouldBe` (head $ (players gs'))
    it "should double pick second player" $ do
      -- pendingWith "Implement getNextPlayer function"
      let gs = initGame numberOfTestPlayers
      gs' <- setupGame gs
      gs' <- pickNextPlayer gs'
      gs' <- pickNextPlayer gs'
      (cur_player gs') `shouldBe` ((players gs') !! 1)
    it "should turn around and pick first player" $ do
      -- pendingWith "Implement getNextPlayer function"
      let gs = initGame numberOfTestPlayers
      gs' <- setupGame gs
      gs' <- updateCurPlayer gs' $ (players gs') !! 3
      gs' <- pickNextPlayer gs'
      (cur_player gs') `shouldBe` (head $ (players gs'))
    it "should declare an overall winner" $ do
      -- pendingWith "Implement playerHasWon function"
      let gs = initGame numberOfTestPlayers
      gs' <- setupGame gs
      gs' <- pickNextPlayer gs'
      gs' <- return (gs' { players = [ head (players gs') ] })
      playerHasWon gs' `shouldBe` True

  describe "explodePlayer" $ do
    it "should explode player" $ do
      -- pendingWith "Implement explodePlayer function"
      let gs = initGame numberOfTestPlayers
      gs' <- setupGame gs
      gs' <- pickNextPlayer gs'
      gs' <- explodePlayer gs'
      (length $ e_players gs') `shouldBe` 1
    it "should remove player from players" $ do
      -- pendingWith "Implement explodePlayer function"
      let gs = initGame numberOfTestPlayers
      gs' <- setupGame gs
      gs' <- pickNextPlayer gs'
      gs' <- explodePlayer gs'
      (length $ players gs') `shouldBe` (numberOfTestPlayers - 1)
    it "should set next player as current" $ do
      -- pendingWith "Implement explorePlayer function"
      let gs = initGame numberOfTestPlayers
      gs' <- setupGame gs
      gs' <- pickNextPlayer gs'
      gs' <- explodePlayer gs'
      (head $ players gs') `shouldBe` (cur_player gs')

  describe "takeFromHand" $ do
    it "should remove from current hand" $ do
      -- pendingWith "Implement takeFromHand function"
      let gs = initGame numberOfTestPlayers
      gs' <- setupGame gs
      gs' <- pickNextPlayer gs'
      gs' <- updateCurHand gs' [ ShuffleCard ]
      (action, gs') <- takeAction UseCard [ ShuffleCard ] gs'
      (cardInCurHand ShuffleCard gs') `shouldBe` False
    it "should move to discard hand" $ do
      -- pendingWith "Implement takeFromHand function"
      let gs = initGame numberOfTestPlayers
      gs' <- setupGame gs
      gs' <- pickNextPlayer gs'
      gs' <- updateCurHand gs' [ ShuffleCard ]
      (action, gs') <- takeAction UseCard [ ShuffleCard ] gs'
      (ShuffleCard `elem` d_stack gs') `shouldBe` True

  describe "takeFromDeck" $ do
    it "should take 1 card and put in hand" $ do
      -- pendingWith "Implement drawNCards function"
      let gs = initGame numberOfTestPlayers
      gs' <- setupGame gs
      gs' <- pickNextPlayer gs'
      c0 <- return $ (deck gs') !! 0
      cur_hand_count <- return (length $ curHand gs')
      (action, gs') <- takeFromDeck gs'
      if isExplodingCard c0
        then (length $ curHand gs') `shouldBe` cur_hand_count - 1
        else (last $ (curHand gs')) `shouldBe` c0
    it "should take only 1 card from deck" $ do
      -- pendingWith "Implement drawNCards function"
      let gs = initGame numberOfTestPlayers
      gs' <- setupGame gs
      gs' <- pickNextPlayer gs'
      c0 <- return $ (deck gs') !! 0
      deck_count <- return $ length (deck gs')
      (action, gs') <- takeFromDeck gs'
      if isExplodingCard c0
        then (length $ deck gs') `shouldBe` deck_count
        else (length $ deck gs') `shouldBe` (deck_count - 1)

  describe "useSimpleStrategy" $ do
    it "should always take card from deck" $ do
      -- pendingWith "Implement useSimpleStrategy function"
      let gs = initGame numberOfTestPlayers
      gs' <- setupGame gs
      gs' <- pickNextPlayer gs'
      let (action, card) = useSimpleStrategy gs' [ ] (curHand gs')
      action `shouldBe` TakeFromDeck

  describe "playCard" $ do
    it "should return EndTurn" $ do
      -- pendingWith "Implemen the other tests first"
      let gs = initGame numberOfTestPlayers
      gs' <- setupGame gs
      gs' <- pickNextPlayer gs'
      (action, gs'') <- playPlayer gs' False
      action `shouldBe` EndTurn
    it "should return Exploded" $ do
      -- pendingWith "Implement the other tests first"
      let gs = initGame numberOfTestPlayers
      gs' <- setupGame gs
      gs' <- pickNextPlayer gs'
      gs' <- injectCards [ ExplodingCard, ExplodingCard ] gs'
      (action, gs') <- explodeOrEndTurn gs'
      (action, gs') <- explodeOrEndTurn gs'
      action `shouldBe` Exploded
    it "should take card from deck" $ do
      -- pendingWith "Implement the other tests first"
      let gs = initGame numberOfTestPlayers
      gs' <- setupGame gs
      gs' <- pickNextPlayer gs'
      (action, gs'') <- takeFromDeck gs'
      (action `elem` [ EndTurn, Exploded, AttackNextPlayer ]) `shouldBe` True
    it "should end after other players are exploded" $ do
      -- pendingWith "Implement the other first"
      let gs = initGame numberOfTestPlayers
      gs' <- setupGame gs
      gs' <- pickNextPlayer gs'
      gs' <- explodePlayer gs'
      gs' <- explodePlayer gs'
      gs' <- explodePlayer gs'
      gs' <- startGame gs'
      (length $ players gs') `shouldBe` 1

  describe "Game..." $ do
    it "should start and end" $ do
      -- pendingWith "Implement other functions first, before enabling."
      let gs = initGame numberOfTestPlayers
      gs' <- setupGame gs
      gs' <- startGame gs'
      (playerHasWon gs') `shouldBe` True


-- Test data fixtures
--
numberOfTestPlayers = 4
johnyCash = HPlayer { name = "Johny", hand = [ ] }
elvisPresly = HPlayer { name = "Elvis", hand = [ ] }

demoState = State { players = [ johnyCash, elvisPresly ]
                  , e_players = [ ]
                  , deck = fullDeck
                  , d_stack = [ ]
                  , cur_player = noPlayer
                  }

explodingHand = [ ExplodingCard, NopeCard, MelonCat, TacoCat ]
defusableHand = [ BeardCat, RainbowCat, DefuseCard, AttackCard, ExplodingCard ]
