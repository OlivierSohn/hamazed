{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Imj.Music.Compose
      ( musicSymbol
      , musicSymbols
      , mkScore
      , notes
      , mkVoice
      ) where

import           Imj.Prelude hiding ((<|>))

import           Language.Haskell.TH
import           Language.Haskell.TH.Quote

import           Data.Text(pack)
import           Text.Parsec((<|>), parse, char, noneOf, spaces, eof, many1, alphaNum, skipMany, choice
                            , between, SourcePos, setPosition)
import           Text.Parsec.Text(Parser)
import           Text.Parsec.Pos(newPos)

import           Imj.Music.Types

-- TODO support multiline / inline comments :
-- do ré {this is an inline comment} mi fa sol {note that
--  inline comments can span
--  over multiple lines! }
-- TODO support changing instruments (with a statefull parser)
musicSymbol :: Parser Symbol
musicSymbol = do
  between
    skipIgnored
    skipIgnored
    $ choice
      [ rest
      , prolong
      , note 0
      ]

 where
  skipIgnored = do
    spaces
    skipMany $ do
      comment
      spaces

  rest = do
    _ <- char '.'
    return Rest

  prolong = do
    _ <- char '-'
    return Extend

  lower = do
    _ <- char 'v'
    spaces
    return $ -1

  upper = do
    _ <- char '^'
    spaces
    return 1

  note n = choice
    [ lower >>= note . (+) n
    , upper >>= note . (+) n
    , go n
    ]

   where
     go x = do
      noteName <- many1 (alphaNum <|> char '#') >>= \case
        "do"   -> return Do
        "ré"   -> return Ré
        "mi"   -> return Mi
        "fa"   -> return Fa
        "sol"  -> return Sol
        "la"   -> return La
        "si"   -> return Si
        "do#"  -> return Réb
        "dod"  -> return Réb
        "réb"  -> return Réb
        "réd"  -> return Réb
        "ré#"  -> return Réb
        "mib"  -> return Mib
        "fad" -> return Solb
        "fa#" -> return Solb
        "solb" -> return Solb
        "sold"  -> return Lab
        "sol#"  -> return Lab
        "lab"  -> return Lab
        "lad"  -> return Sib
        "la#"  -> return Sib
        "sib"  -> return Sib
        str -> fail $"Wrong note:" ++ str
      return $ Note $ NoteSpec noteName (x + noOctave) defaultInstrument

  comment = do
    _ <- char ':'
    skipMany $ noneOf ['\n','\r']

musicSymbols :: Parser [Symbol]
musicSymbols = many1 musicSymbol

location' :: Q SourcePos
location' = aux <$> location
  where
    aux :: Loc -> SourcePos
    aux loc = uncurry (newPos (loc_filename loc)) (loc_start loc)

topLevel :: Parser a -> Parser a
topLevel p = spaces *> p <* eof

notes :: QuasiQuoter
notes = QuasiQuoter {
      quoteExp = \str -> do
        l <- location'
        let c' = parse (setPosition l *> topLevel musicSymbols) "" $ pack str
        c <- either (error.show) return c'
        dataToExpQ (const Nothing) c
    , quotePat  = undefined
    , quoteType = undefined
    , quoteDec  = undefined
    }
