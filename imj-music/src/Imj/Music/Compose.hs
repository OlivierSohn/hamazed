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
      , naturalNote
      , noOctave
      , mkScore
      , notes
      , mkVoice
      ) where

import           Language.Haskell.TH
import           Language.Haskell.TH.Quote

import           Data.Text(pack)
import           Text.Parsec(parse, (<|>), char, anyChar, spaces, space, eof, many1, choice, manyTill
                            , SourcePos, setPosition)
import           Text.Parsec.Text(Parser)
import           Text.Parsec.Pos(newPos)

import           Imj.Music.Types

noOctave :: Octave
noOctave = Octave 6

musicSymbol :: Parser Symbol
musicSymbol = do
  spaces
  choice
    [ rest
    , prolong
    , note 0
    ]
 where
  rest = do
    _ <- char '.'
    spaces
    return Rest

  prolong = do
    _ <- char '-'
    spaces
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
      noteName <- manyTill anyChar (eof <|> (do _ <- space; return ())) >>= \case
        "do"   -> return Do
        "ré"   -> return Ré
        "mi"   -> return Mi
        "fa"   -> return Fa
        "sol"  -> return Sol
        "la"   -> return La
        "si"   -> return Si
        "réb"  -> return Réb
        "mib"  -> return Mib
        "solb" -> return Solb
        "lab"  -> return Lab
        "sib"  -> return Sib
        str -> fail $"Wrong note:" ++ str
      spaces
      return $ Note $ NoteSpec noteName $ x + noOctave

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

naturalNote :: NoteName -> Bool
naturalNote = \case
  Do -> True
  Réb -> False
  Ré -> True
  Mib -> False
  Mi -> True
  Fa -> True
  Solb -> False
  Sol -> True
  Lab -> False
  La -> True
  Sib -> False
  Si -> True
