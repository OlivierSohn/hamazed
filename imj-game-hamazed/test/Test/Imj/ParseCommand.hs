{-# LANGUAGE OverloadedStrings #-}

module Test.Imj.ParseCommand
          ( testParseCommand
          , testMaxOneSpace
          ) where

import           Data.Attoparsec.Text(parseOnly)

import           Imj.Game.Hamazed.Types
import           Imj.Game.Hamazed.Network.Types

import           Imj.Game.Hamazed.Command
import           Imj.Graphics.Color

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
  parse "Hello!" `shouldBe` (Right $ Right $ ClientCmd $ Says "Hello!")
  parse "  a" `shouldBe` (Right $ Right $ ClientCmd $ Says "a")
  parse "a  " `shouldBe` (Right $ Right $ ClientCmd $ Says "a")
  parse "a a" `shouldBe` (Right $ Right $ ClientCmd $ Says "a a")
  parse "a  a" `shouldBe` (Right $ Right $ ClientCmd $ Says "a a")

  parse "/a" `shouldBe` Left "string"
  let cmd = Right $ Right $ ClientCmd $ AssignName $ PlayerName "Newname"
  parse "/name Newname" `shouldBe` cmd
  parse "/name:Newname" `shouldBe` cmd
  parse "/name:  Newname  " `shouldBe` cmd
  parse "    /name:  Newname  " `shouldBe` cmd

  parse "/color" `shouldBe` (Right $ Right $ ServerRep $ Get ColorSchemeCenterKey)
  parse "/color 1 2 3" `shouldBe` (Right $ Right $ ServerCmd $ Put $ ColorSchemeCenter $ rgb 1 2 3)
 where
  parse = parseOnly command

shouldBe :: (Show a, Eq a) => a -> a -> IO ()
shouldBe actual expected =
  if actual == expected
    then
      return ()
    else
      error $ "expected\n" ++ show expected ++ " but got\n" ++ show actual
