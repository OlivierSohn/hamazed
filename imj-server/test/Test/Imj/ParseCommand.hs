{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Test.Imj.ParseCommand
          ( testParseCommand
          , testMaxOneSpace
          ) where

import           Imj.Prelude
import           Control.DeepSeq(NFData(..))
import           Data.Attoparsec.Text(parseOnly, Parser)

import           Imj.Server.Class

import           Imj.Server.Command
import           Imj.Graphics.Color
import           Imj.Util

-- This is defined here, not in the module defining 'class Server' to satisfy
-- injectivity rules defined in 'Server' when building imj-game-tutorial-increment.
data VoidServer = VoidServer
  deriving(Generic,NFData)
instance Server VoidServer where
  type StateValueT  VoidServer = ()

  type ClientEventT VoidServer = ()
  type ServerEventT VoidServer = ()

  type ValuesT      VoidServer = ()
  type ClientViewT  VoidServer = ()

  mkInitialState = return ((),VoidServer)
  mkInitialClient = ()
  handleClientEvent _ = return ()

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
  parse "Hello!" `shouldBe` (Right $ Right $ RequestApproval $ Says "Hello!")
  parse "  a" `shouldBe` (Right $ Right $ RequestApproval $ Says "a")
  parse "a  " `shouldBe` (Right $ Right $ RequestApproval $ Says "a")
  parse "a a" `shouldBe` (Right $ Right $ RequestApproval $ Says "a a")
  parse "a  a" `shouldBe` (Right $ Right $ RequestApproval $ Says "a a")

  parse "/a" `shouldBe` Left "Failed reading: 'a' is an unknown command."
  let cmd = Right $ Right $ RequestApproval $ AssignName $ ClientName "Newname"
  parse "/name Newname" `shouldBe` cmd
  parse "/name:Newname" `shouldBe` cmd
  parse "/name :Newname" `shouldBe` cmd
  parse "/name : Newname" `shouldBe` cmd
  parse "/name:  Newname  " `shouldBe` cmd
  parse "    /name:  Newname  " `shouldBe` cmd

  parse "/color" `shouldBe` (Right $ Right $ Report $ Get ColorSchemeCenterKey)
  parse "/color " `shouldBe` (Right $ Right $ Report $ Get ColorSchemeCenterKey)

  parse "/ color 1 2 3" `shouldBe` (Right $ Right $ Do $ Put $ ColorSchemeCenter $ rgb 1 2 3)
  parse "/ color    1   2     3" `shouldBe` (Right $ Right $ Do $ Put $ ColorSchemeCenter $ rgb 1 2 3)
  parse "/ color 1 2 3 " `shouldBe` (Right $ Right $ Do $ Put $ ColorSchemeCenter $ rgb 1 2 3)
  parse "/ color:1 2 3" `shouldBe` (Right $ Right $ Do $ Put $ ColorSchemeCenter $ rgb 1 2 3)
  parse "/ color : 1 2 3" `shouldBe` (Right $ Right $ Do $ Put $ ColorSchemeCenter $ rgb 1 2 3)
  parse "/color 1 2 3" `shouldBe` (Right $ Right $ Do $ Put $ ColorSchemeCenter $ rgb 1 2 3)
 where
  parse = parseOnly (command :: Parser (Either Text (Command VoidServer)))

shouldBe :: (Show a, Eq a) => a -> a -> IO ()
shouldBe actual expected =
  if actual == expected
    then
      return ()
    else
      error $ "expected\n" ++ show expected ++ " but got\n" ++ show actual
