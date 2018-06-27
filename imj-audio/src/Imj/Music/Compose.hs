{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}


module Imj.Music.Compose
      ( voice
      , voices
      , concatSystems
      ) where

import           Imj.Prelude hiding ((<|>), many)

import           Language.Haskell.TH
import           Language.Haskell.TH.Quote

import           Data.Either(rights)
import           Data.List(replicate, length, take)
import           Data.String(lines)
import           Data.Text(pack, strip)
import           Text.Parsec((<|>), parse, char, noneOf, spaces, eof, many, many1, manyTill, alphaNum, skipMany, choice
                            , between, SourcePos, setPosition)
import           Text.Parsec.Text(Parser)
import           Text.Parsec.Pos(newPos)

import           Imj.Music.Instruction


monophonicSymbol :: Parser (Either () Instruction)
monophonicSymbol =
  between
    spaces
    spaces
    $ fmap Left monoLineComment <|> fmap Right symbol


-- TODO support multiline / inline comments :
-- do ré {this is an inline comment} mi fa sol {note that
--  inline comments can span
--  over multiple lines! }
monoLineComment :: Parser ()
monoLineComment = do
  _ <- char ':'
  skipMany $ noneOf ['\n','\r']

symbol :: Parser Instruction
symbol =
  choice
    [ rest
    , prolong
    , note 0
    ]

 where

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
      return $ Note noteName (x + noOctave)

location' :: Q SourcePos
location' = aux <$> location
  where
    aux :: Loc -> SourcePos
    aux loc = uncurry (newPos (loc_filename loc)) (loc_start loc)

{- | Expression 'QuasiQuoter' producing a /monophonic/ voice, i.e. ['Instruction'],
and supporting the following syntaxes:

@
[voice|do ré mi|]   -- is a shortcut notation for [Note Do noOctave,
                                                   Note Ré noOctave,
                                                   Note Mi noOctave]
@

one octave higher:

@[voice|^do ^ré ^mi|]@

two octaves higher:

@[voice|^^do ^^ré ^^mi|]@

one octave lower:

@[voice|vdo vré vmi|]@

two octaves lower:

@[voice|vvdo vvré vvmi|]@

with a different rythm (@.@ represents 'Rest', @-@ represents 'Extend'):

@[voice|do . . ré mi - -|]@

altered (@d@ and @#@ shift the pitch up one semitone,
@b@ shifts the pitch down one semitone):

@[voice|dob réd mi do#|]@

a voice can be split over several lines, and single-line comments are supported:

@
[voice|
: This melody could be used during the game intro
do ré mi . . sol
la sol fa sol - -
sol mi do ré - -    : comments start with ':' and continue until the end of the line
mi . mi . ré mi     : maybe we could add a second voice to that, using 'voices')
|]@

-}
voice :: QuasiQuoter
voice = QuasiQuoter {
      quoteExp = \str -> do
        l <- location'
        let c' = parse (setPosition l *> fmap rights (many monophonicSymbol) <* eof) "" $ pack str
        c <- either (error.show) return c'
        dataToExpQ (const Nothing) c
    , quotePat  = undefined
    , quoteType = undefined
    , quoteDec  = undefined
    }

groupBySystem :: [[Instruction]] -> [[[Instruction]]]
groupBySystem = reverse . go [] []
 where
  go allSystems [] [] = allSystems
  go allSystems (curSystem@(_:_)) [] = reverse curSystem : allSystems
  go allSystems [] ([]:inputs) = go allSystems [] inputs
  go allSystems curSystem ([]:is) = go (reverse curSystem : allSystems) [] is
  go allSystems curSystem (i@(_:_):is) = go allSystems (i : curSystem) is

concatSystems :: [[[Instruction]]] -> [[Instruction]]
concatSystems = go 0 []
 where
  go _ acc [] = acc
  go _ _ ([]:_) = error "logic" -- groupBySystem prevents that from happening
  go lengthAllSystems acc (system:systems) = go newLengthAllSystems newAcc systems
   where
    -- lines of the same system can have a different number of symbols:
    -- we pad them so tht each line has the same length.
    paddedSystem = map (take lengthSystem . flip (++) (repeat Rest)) system
    lengthSystem = fromMaybe 0 $ maximumMaybe $ map length system

    newLengthAllSystems = lengthAllSystems + lengthSystem

    -- we allow systems to have different lines counts
    newAcc = zipWith (++)
      (take nLines $ acc ++ (repeat $ replicate lengthAllSystems Rest))
      (take nLines $ paddedSystem ++ (repeat $ replicate lengthSystem Rest))
    nLines = max (length acc) $ length paddedSystem



{- | Expression 'QuasiQuoter' producing /polyphonic/ voices, i.e. [['Instruction']],
where the number of voices can vary over time.

For example:

@
[voices|
  do sol la sol mi do
            : blocks of simultaneous voices are separated by a blank line
  do ré mi
  mi fa sol
  sol la si
  ^do ^ré ^mi

  fa        : voices that are shorter that the longest voice in the block are padded with 'Rest'.
  ré -   -
  |]
@

which is equivalent to:

@
[ [voice| do sol la sol mi do do  ré  mi  fa .   .  |]
, [voice| .  .   .  .   .  .  mi  fa  sol ré -   -  |]
, [voice| .  .   .  .   .  .  sol la  si  .  .   .  |]
, [voice| .  .   .  .   .  .  ^do ^ré ^mi .  .   .  |]
]
@

And, implementing the suggestion made in 'voice', here is the modified song:

@
[voices|
: This melody could be used during the game intro
do ré mi . . sol

la sol fa sol - -

sol mi do ré - -

mi  . mi . ré mi
sol . la . fa sol   : Man, this second voice rules...
|]
@

-}
voices :: QuasiQuoter
voices = QuasiQuoter {
      quoteExp = \str -> do
        l <- location'
        let symbolsByLine =
              map (either (error.show) id)
              $ map (parse (setPosition l *> fmap rights (manyTill monophonicSymbol eof)) "")
              $ map (strip . pack) -- for some reason I don't fully understand, strip is necessary.
              $ lines str
        dataToExpQ (const Nothing) $ concatSystems $ groupBySystem symbolsByLine
    , quotePat  = undefined
    , quoteType = undefined
    , quoteDec  = undefined
    }
