{-# Language OverloadedStrings #-}

-- based loosely on http://learnyouahaskell.com/zippers

module Zippers where

import           Data.List            (break)
import           Data.ByteString.Lazy () -- instance IsString ByteString
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text            as T


type Name = T.Text
type Data = BL.ByteString

data FSItem   = File Name Data | Folder Name [FSItem] deriving Show
data FSCrumb  = FSCrumb Name [FSItem] [FSItem]        deriving Show
type FSZipper = (FSItem, [FSCrumb])

fsUp :: FSZipper -> FSZipper
fsUp (item, (FSCrumb name ls rs) : bs) = (Folder name (ls ++ [item] ++ rs), bs)

fsTo :: Name -> FSZipper -> FSZipper
fsTo name (Folder folderName items, bs) =
    let (ls, item:rs) = break (nameIs name) items
    in  (item, FSCrumb folderName ls rs:bs)

nameIs :: Name -> FSItem -> Bool
nameIs name (File   fileName   _) = name == fileName
nameIs name (Folder folderName _) = name == folderName


myDisk :: FSItem
myDisk =
    Folder "root"
        [ File "goat_yelling_like_man.wmv" "baaaaaa"
        , File "pope_time.avi" "god bless"
        , Folder "pics"
            [ File "ape_throwing_up.jpg" "bleargh"
            , File "watermelon_smash.gif" "smash!!"
            , File "skull_man(scary).bmp" "Yikes!"
            ]
        , File "dijon_poupon.doc" "best mustard"
        , Folder "programs"
            [ File "fartwizard.exe" "10gotofart"
            , File "owl_bandit.dmg" "mov eax, h00t"
            , File "not_a_virus.exe" "really not a virus"
            , Folder "source code"
                [ File "best_hs_prog.hs" "main = print (fix error)"
                , File "random.hs" "main = print 4"
                ]
            ]
        ]




data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)
data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show)
type Zipper a = (Tree a, [Crumb a])


(-:) :: a -> (a -> b) -> b
x -: f = f x

goLeft :: Zipper a -> Zipper a
goLeft (Node x l r, bs) = (l, LeftCrumb x r:bs)

goRight :: Zipper a -> Zipper a
goRight (Node x l r, bs) = (r, RightCrumb x l:bs)

goUp :: Zipper a -> Zipper a
goUp (t, (LeftCrumb  x r) : bs) = (Node x t r, bs)
goUp (t, (RightCrumb x l) : bs) = (Node x l t, bs)

modify :: (a -> a) -> Zipper a -> Zipper a
modify f ((Node x l r), bs) = (Node (f x) l r, bs)
modify _ (Empty,        bs) = (Empty, bs)

attach :: Tree a -> Zipper a -> Zipper a
attach t (_, bs) = (t, bs)

topMost :: Zipper a -> Zipper a
topMost (t,[]) = (t,[])
topMost z      = topMost (goUp z)
