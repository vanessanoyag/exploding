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

shufflerSpecs :: Spec
shufflerSpecs = describe "Shuffler" $ do
  it "State has 2 players" $ do
    length (players demoState) `shouldBe` 2
  it "State has 56 cards" $ do
    length (deck demoState) `shouldBe` 56
  it "Shuffled to the same number of cards" $ do
    pendingWith "Implement shuffleDeck function"
    -- demoState' <- shuffleDeck demoState
    -- length (deck demoState) `shouldBe` 56
  it "Shuffled to different positions" $ do
    pendingWith "Implement shuffleDeck function"
    -- let originalDeck = deck demoState
    -- demoState' <- shuffleDeck demoState
    -- (deck demoState') `shouldNotBe` originalDeck

gameSpecs :: Spec
gameSpecs = describe "Game" $ do
  describe "initGame" $ do
    it "should create 4 players" $ do
      -- pendingWith "Implement the initGame function"
      let gs = initGame 4
      length (players gs) `shouldBe` 4
    it "should initialize the deck with 56 cards" $ do
      -- pendingWith "Implement the initGame function"
      let gs = initGame 4
      length (deck gs) `shouldBe` 56
    it "should initialize discard pile to empty" $ do
      -- pendingWith "Implement the initGame function"
      let gs = initGame 4
      length (d_stack gs) `shouldBe` 0
  describe "setupGame" $ do
    it "should shuffle the deck" $ do
      pendingWith "Implement the setupGame function"
      -- let gs = initGame 4
      -- gs' <- setupGame gs
    it "should distribute cards to players" $ do
      pendingWith "Implement the setupGame function"
      -- let gs = initGame 4
      -- gs' <- setupGame gs

-- Test data fixtures
--
johnyCash = HPlayer { name = "Johny", hand = [ ] }
elvisPresly = HPlayer { name = "Elvis", hand = [ ] }

demoState = State { players = [ johnyCash, elvisPresly ]
                  , deck = fullDeck
                  , d_stack = [ ]
                  }
