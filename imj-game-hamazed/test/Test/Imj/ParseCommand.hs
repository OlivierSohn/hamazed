{-# LANGUAGE OverloadedStrings #-}

module Test.Imj.ParseCommand
          ( testParseCommand
          , testMaxOneSpace
          ) where

import           Data.Attoparsec.Text(parseOnly, Parser)
import           Data.Text

import           Imj.Game.Hamazed.Network.Types
import           Imj.Game.Hamazed.State.Types
import           Imj.Game.Hamazed.Logic

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

  parse "/a" `shouldBe` Left "Failed reading: 'a' is an unknown command."
  let cmd = Right $ Right $ ClientCmd $ AssignName $ ClientName "Newname"
  parse "/name Newname" `shouldBe` cmd
  parse "/name:Newname" `shouldBe` cmd
  parse "/name :Newname" `shouldBe` cmd
  parse "/name : Newname" `shouldBe` cmd
  parse "/name:  Newname  " `shouldBe` cmd
  parse "    /name:  Newname  " `shouldBe` cmd

  parse "/color" `shouldBe` (Right $ Right $ ServerRep $ Get ColorSchemeCenterKey)
  parse "/color " `shouldBe` (Right $ Right $ ServerRep $ Get ColorSchemeCenterKey)

  parse "/ color 1 2 3" `shouldBe` (Right $ Right $ ServerCmd $ Put $ ColorSchemeCenter $ rgb 1 2 3)
  parse "/ color    1   2     3" `shouldBe` (Right $ Right $ ServerCmd $ Put $ ColorSchemeCenter $ rgb 1 2 3)
  parse "/ color 1 2 3 " `shouldBe` (Right $ Right $ ServerCmd $ Put $ ColorSchemeCenter $ rgb 1 2 3)
  parse "/ color:1 2 3" `shouldBe` (Right $ Right $ ServerCmd $ Put $ ColorSchemeCenter $ rgb 1 2 3)
  parse "/ color : 1 2 3" `shouldBe` (Right $ Right $ ServerCmd $ Put $ ColorSchemeCenter $ rgb 1 2 3)
  parse "/color 1 2 3" `shouldBe` (Right $ Right $ ServerCmd $ Put $ ColorSchemeCenter $ rgb 1 2 3)
 where
  parse = parseOnly (command :: Parser (Either Text (Command (ServerT HamazedGame))))

shouldBe :: (Show a, Eq a) => a -> a -> IO ()
shouldBe actual expected =
  if actual == expected
    then
      return ()
    else
      error $ "expected\n" ++ show expected ++ " but got\n" ++ show actual
