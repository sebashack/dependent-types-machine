{-# LANGUAGE GADTs             #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}

module DoorTypes ( DoorCmd(..)
                 , DoorState(..)
                 , (|>)
                 , (|>>)
                 , cmd
                 , evalDoor) where

import Data.Text (Text)
import Data.Monoid ((<>))
import System.Random
import System.Directory (getHomeDirectory)
import qualified Data.Text.IO as T
import qualified Data.Text as T


data DoorState = DoorOpen | DoorClosed

type Name = Text

type Secret = Int

data DoorCmd :: DoorState -> DoorState -> * -> * where
  Open :: Bool -> DoorCmd 'DoorClosed 'DoorOpen (Maybe Secret)
  Close :: ty -> DoorCmd 'DoorOpen 'DoorClosed ty
  RingBell :: Name -> DoorCmd 'DoorClosed 'DoorClosed Name
  Check :: Name -> DoorCmd 'DoorClosed 'DoorClosed Bool

  Pure :: ty -> DoorCmd s s ty

  Bind :: DoorCmd s1 s2 a -> (a -> DoorCmd s2 s3 b) -> DoorCmd s1 s3 b


-- | This acts like the sequencing operator `>>`
(|>) :: DoorCmd s1 s2 a -> DoorCmd s2 s3 b -> DoorCmd s1 s3 b
(|>) cmd1 cmd2 = Bind cmd1 (const cmd2)


-- | This acts like the binding operator `>>=`
(|>>) :: DoorCmd s1 s2 a -> (a -> DoorCmd s2 s3 b) -> DoorCmd s1 s3 b
(|>>) = Bind

-- | Lifting commands
cmd :: ty -> DoorCmd s s ty
cmd = Pure

-- | Evaluator
evalDoor :: DoorCmd inS outS res -> IO res
evalDoor (RingBell nm) = do
  putStrLn "Someone rings the door"
  putStrLn "Storing that person in DB ..."
  home <- getHomeDirectory
  T.appendFile (home ++ "/panda-core/ring.txt") (nm <> "\n")
  return nm
evalDoor (Check nm) = do
  home <- getHomeDirectory
  people <- T.readFile $ home ++ "/panda-core/ring.txt"
  return $ nm `elem` T.lines people
evalDoor (Open hasRung) = do
  putStrLn "I will give you the secret number"
  if hasRung then Just <$> randomIO else return Nothing
evalDoor (Close ty) =
  putStrLn "Closing door... Good Bye!!" >> pure ty
evalDoor (Pure v) = return v
evalDoor (Bind v f) = do
  v' <- evalDoor v
  evalDoor $ f v'
