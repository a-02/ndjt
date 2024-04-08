module Util where

import Data.List.NonEmpty.Zipper as Z
import Data.List.NonEmpty qualified as NE

import Types

listToZipper :: [a] -> Z.Zipper a
listToZipper = Z.fromNonEmpty . NE.fromList

on :: Decks -> Decks
on = deckReplace True

off :: Decks -> Decks
off = deckReplace False

add :: Int -> Zipper Int -> Zipper Int
add x z = replace ((+ x) . current $ z) z

deckReplace :: Bool -> Decks -> Decks
deckReplace b z =
  let f (DeckInfo a _ c) = DeckInfo a b c
   in replace (f $ current z) z

first :: (a -> t) -> (a, b, c) -> (t, b, c)
first f (a, b, c) = (f a, b, c)

second :: (b -> t) -> (a, b, c) -> (a, t, c)
second f (a, b, c) = (a, f b, c)

third :: (c -> t) -> (a, b, c) -> (a, b, t)
third f (a, b, c) = (a, b, f c)

zipNE3 :: NE.NonEmpty a -> NE.NonEmpty b -> NE.NonEmpty c -> NE.NonEmpty (a,b,c)
zipNE3 ~(x NE.:| xs) ~(y NE.:| ys) ~(z NE.:| zs) = (x, y, z) NE.:| zip3 xs ys zs
