{-# LANGUAGE OverloadedStrings #-}

module Test.Imj.ParseCommand
          ( testParseCommand
          , testMaxOneSpace
          ) where

import           Data.Attoparsec.Text(parseOnly)

import           Imj.Game.Hamazed.Types
import           Imj.Game.Hamazed.Command

testMaxOneSpace :: IO ()
testMaxOneSpace = do
  maxOneSpace "" `shouldBe` ""
  maxOneSpace " " `shouldBe` ""
  maxOneSpace "  " `shouldBe` ""
  maxOneSpace "   a    " `shouldBe` "a"
  maxOneSpace "   a   b  " `shouldBe` "a b"
  maxOneSpace "   aaa   bb  " `shouldBe` "aaa bb"

testParseCommand :: IO ()
testParseCommand = do
  parse "Hello!" `shouldBe` (Right $ Says "Hello!")
  parse "  a" `shouldBe` (Right $ Says "a")
  parse "a  " `shouldBe` (Right $ Says "a")
  parse "a a" `shouldBe` (Right $ Says "a a")
  parse "a  a" `shouldBe` (Right $ Says "a a")

  parse "/a" `shouldBe` (Left "string")
  parse "/name Newname" `shouldBe` (Right $ AssignName $ PlayerName "Newname")
  parse "/name:Newname" `shouldBe` (Right $ AssignName $ PlayerName "Newname")
  parse "/name:  Newname  " `shouldBe` (Right $ AssignName $ PlayerName "Newname")
  parse "    /name:  Newname  " `shouldBe` (Right $ AssignName $ PlayerName "Newname")
 where
  parse = parseOnly command

shouldBe :: (Show a, Eq a) => a -> a -> IO ()
shouldBe actual expected =
  if actual == expected
    then
      return ()
    else
      error $ "expected\n" ++ show expected ++ " but got\n" ++ show actual
