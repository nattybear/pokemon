{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}

module Main where

import Data.List
import Data.Text
import GHC.Generics
import Numeric.Natural
import System.Random.Stateful

data Card = PokemonCard { name    :: Text
                        , typ     :: Energy
                        , hp      :: HP
                        , attacks :: [Attack] }
          | EnergyCard  { typ     :: Energy   }

newtype HP = HP Natural
           deriving (Eq, Show, Num)

data Energy = Colorless
            | Grass
            | Fire
            | Water
            | Lightning
            | Fighting
            | Psychic
            | Darkness
            | Metal
            | Dragon
            deriving Show

instance Eq Energy where
  Colorless == _         = True
  _         == Colorless = True
  Grass     == Grass     = True
  Fire      == Fire      = True
  Water     == Water     = True
  Lightning == Lightning = True
  Fighting  == Fighting  = True
  Psychic   == Psychic   = True
  Darkness  == Darkness  = True
  Metal     == Metal     = True
  Dragon    == Dragon    = True
  _         == _         = False

data Attack = Attack { attackName :: Text
                     , cost       :: [Energy]
                     , damage     :: Natural }

scratch :: Attack
scratch = Attack "Scratch" [Colorless] 10

beat :: Attack
beat = Attack "Beat" [Grass, Colorless] 20

tackle :: Attack
tackle = Attack "Tackle" [Colorless] 10

melt :: Attack
melt = Attack "Melt" [Water, Psychic] 20

enoughEnergy :: [Energy] -> [Card] -> Bool
enoughEnergy []     _     = True
enoughEnergy (x:xs) cards = if x `elem` cards'
                            then enoughEnergy xs cards''
                            else False
  where
    cards'  = typ <$> cards
    cards'' = EnergyCard <$> delete x cards'

missingEnergy :: [Energy] -> [Card] -> Maybe [Energy]
missingEnergy []   _      = Nothing
missingEnergy cost []     = Just cost
missingEnergy cost (x:xs) = if x' `elem` cost
                            then missingEnergy (delete x' cost) xs
                            else missingEnergy cost xs
  where
    x' = typ x

data FlipOutcome = Heads | Tails
                 deriving ( Show
                          , Generic
                          , Uniform )

data Action = FlipCoin (FlipOutcome -> Action)
            | Damage Natural

surpriseAttackAction :: Action
surpriseAttackAction = FlipCoin $ \case Heads -> Damage 30
                                        Tails -> Damage 0

ironTailAction :: Action
ironTailAction = go 0
  where
    go acc = FlipCoin $ \case Tails -> Damage acc
                              Heads -> go (acc + 30)

interpretRandom :: Action -> IO Natural
interpretRandom (Damage d)   = pure d
interpretRandom (FlipCoin f) = do
  outcome <- flipCoin
  interpretRandom (f outcome) 

flipCoin :: IO FlipOutcome
flipCoin = uniformM globalStdGen
  
main :: IO ()
main = do
  coin <- flipCoin
  print coin
