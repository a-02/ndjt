module Util where

import Data.List.NonEmpty.Zipper as Z
import Data.List.NonEmpty qualified as NE

import Types

listToZipper :: [a] -> Z.Zipper a
listToZipper = Z.fromNonEmpty . NE.fromList

-- set the active deck to On
on :: Decks -> Decks
on = deckReplace True

-- set the active deck to Off
off :: Decks -> Decks
off = deckReplace False

add :: Int -> Zipper Int -> Zipper Int
add x z = replace ((+ x) . current $ z) z

-- replace the active decks status with 'b'
deckReplace :: Bool -> Decks -> Decks
deckReplace b z =
  let f (DeckInfo a _ c d e) = DeckInfo a b c d e
   in replace (f $ current z) z

first :: (a -> t) -> (a, b, c) -> (t, b, c)
first f (a, b, c) = (f a, b, c)

second :: (b -> t) -> (a, b, c) -> (a, t, c)
second f (a, b, c) = (a, f b, c)

third :: (c -> t) -> (a, b, c) -> (a, b, t)
third f (a, b, c) = (a, b, f c)

zipNE3 :: NE.NonEmpty a -> NE.NonEmpty b -> NE.NonEmpty c -> NE.NonEmpty (a,b,c)
zipNE3 ~(x NE.:| xs) ~(y NE.:| ys) ~(z NE.:| zs) = (x, y, z) NE.:| zip3 xs ys zs

showOperatingMode :: OperatingMode -> String
showOperatingMode = show

showZipper :: Show a => Zipper a -> String
showZipper z = 
  let l = lefts z
      c = current z
      r = rights z
   in show l ++ show c ++ show r

-- legit worst function ive ever written
-- so inefficient it hurts
-- also its PARTIAL too!!
-- even better!
hungryRotate :: Int -> [a] -> [a]
hungryRotate i list =
  let f (x:xs) = xs ++ pure x
      f [] = []
   in iterate f list !! i

uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f (a,b,c) = f a b c
