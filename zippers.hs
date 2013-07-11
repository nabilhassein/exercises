{-# Language OverloadedStrings #-}

-- based loosely on http://learnyouahaskell.com/zippers
module Zippers where

import           Data.Monoid                ((<>))
import           Data.ByteString.Lazy.Char8 () -- instance IsString ByteString
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text            as T


type Name = T.Text
type Data = BL.ByteString

data FSItem   = File Name Data | Folder Name [FSItem] deriving Show
data FSCrumb  = FSCrumb Name [FSItem] [FSItem]        deriving Show
type FSZipper = (FSItem, [FSCrumb])

fsUp :: FSZipper -> Either T.Text FSZipper
fsUp (_, [])                      = Left "No upward path to follow"
fsUp (x, FSCrumb name ls rs : bs) = Right (Folder name (ls ++ [x] ++ rs), bs)

fsTo :: Name -> FSZipper -> Either T.Text FSZipper
fsTo _    (File   _          _     , _ ) = Left "fsTo: not focused on a Folder"
fsTo name (Folder folderName items , bs) = case break (nameIs name) items of
  (_ , [])      -> Left $ "The name " <> name <> " is not in this Folder"
  (ls, item:rs) -> Right (item, FSCrumb folderName ls rs : bs)

nameIs :: Name -> FSItem -> Bool
nameIs name (File   fileName   _) = name == fileName
nameIs name (Folder folderName _) = name == folderName

fsRename :: Name -> FSZipper -> FSZipper
fsRename newName (Folder _ items , bs) = (Folder newName items , bs)
fsRename newName (File   _ dat   , bs) = (File   newName dat   , bs)


-- a simple but nontrivial example for ease of interactive testing in ghci
myDisk :: FSItem
myDisk =
    Folder "root"
        [
          File "goat_yelling_like_man.wmv" "baaaaaa"
        , File "pope_time.avi" "god bless"
        , Folder "pics"
            [
              File "ape_throwing_up.jpg" "bleargh"
            , File "watermelon_smash.gif" "smash!!"
            , File "skull_man(scary).bmp" "Yikes!"
            ]
        , File "dijon_poupon.doc" "best mustard"
        , Folder "programs"
            [
              File "fartwizard.exe" "10gotofart"
            , File "owl_bandit.dmg" "mov eax, h00t"
            , File "not_a_virus.exe" "really not a virus"
            , Folder "source code"
                [
                  File "best_hs_prog.hs" "main = print (fix error)"
                , File "random.hs" "main = print 4"
                ]
            ]
        ]
