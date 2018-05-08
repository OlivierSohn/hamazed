{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Imj.Server.Command
      ( command
      ) where


import           Imj.Prelude

import           Data.Attoparsec.Text(Parser, takeText, endOfInput, char, decimal
                                    , peekChar, peekChar', skipSpace, takeWhile1)
import           Data.Char(toLower, isAlphaNum)
import qualified Data.Text as Text
import           Data.Map((!?))

import           Imj.Graphics.Color.Types
import           Imj.Network
import           Imj.Server.Class
import           Imj.Util(maxOneSpace)

{- Returns a parser of commands.
-}
command :: (ServerCmdParser s) => Parser (Either Text (Command s))
command = do
  skipSpace
  peekChar' >>= \case -- we peek to issue an error on wrong commands (instead of interpreting them as a message)
    '/' -> do
      char '/' *> skipSpace
      cmdName <- Text.map toLower <$> takeWhile1 isAlphaNum
      skipSpace
      peekChar >>= maybe
        (return ())
        (\c -> if c == ':'
            then
              void $ char ':'
            else
              return ())
      skipSpace
      case cmdName of
        "name" -> Right . RequestApproval . AssignName . ClientName . maxOneSpace <$> takeText <* endOfInput
        "color" ->
          tryReport <|> tryCmd
         where
          tryReport = do
            void endOfInput
            return $ Right $ Report $ Get ColorSchemeCenterKey
          tryCmd = do
            r <- decimal
            skipSpace
            g <- decimal
            skipSpace
            b <- decimal
            return $ Do . Put . ColorSchemeCenter <$> userRgb r g b
        _ ->
          maybe
            (return $ Left $ "'" <> cmdName <> "' is an unknown command.")
            (\parser -> Right <$> parser)
            $ cmdParsers !? cmdName
    _ -> Right . RequestApproval . Says . maxOneSpace <$> (takeText <* endOfInput)
