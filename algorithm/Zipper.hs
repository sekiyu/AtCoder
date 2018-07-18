module Zipper where
import Tree

data Crumb a = LeftCrumb a (Tree a)
  | RightCrumb a (Tree a) deriving (Show)

type Zipper a = (Tree a, [Crumb a])

goLeft :: (Tree a, [Crumb a]) -> (Tree a, [Crumb a])
goLeft (Node x l r, crumbs) = (l, (LeftCrumb x r):crumbs)

goRight :: (Tree a, [Crumb a]) -> (Tree a, [Crumb a])
goRight (Node x l r, crumbs) = (r, RightCrumb x r:crumbs)

goUp :: (Tree a, [Crumb a]) -> (Tree a, [Crumb a])
goUp (tree, LeftCrumb a r:cs) =(Node a tree r, cs)
goUp (tree, RightCrumb a l:cs) =(Node a l tree, cs)

goTop :: (Tree a, [Crumb a]) -> (Tree a, [Crumb a])
goTop tree@(t, []) = (t, [])
goTop tree@(t, c:cs) = goTop $ goUp tree

modify :: (a -> a) -> (Tree a, [Crumb a]) -> (Tree a, [Crumb a])
modify _ (EmptyTree, cs) = (EmptyTree, cs)
modify f (Node a l r, cs) = (Node (f a) l r, cs)

attach :: Tree a -> (Tree a, [Crumb a]) -> (Tree a, [Crumb a])
attach tree (_, cs) = (tree, cs)

sampleZipper = (sampleTree, [])
