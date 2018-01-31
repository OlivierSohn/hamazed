{-# LANGUAGE TemplateHaskell #-}

module Imj.Graphics.Font
    (fontFiles
    ) where

import           Data.ByteString(ByteString)
import           Data.FileEmbed(embedFile)
{-| This is a font based on
<https://github.com/adobe-fonts/source-code-pro Source Code Pro Bold>,
modified to reduce the negative height
of the Pipe glyph used for hamazed game in animations. The reason I reduced
this height is so that the drawn pixels are kept within the logical square reserved
to that 'Coords', else, since I use delta rendering, there would be leftovers of
previous frames outside the boundaries. -}
fontFiles :: [ByteString]
-- Note that I don't embed the directory, I want compilation to fail if this particular file
-- is not found.
fontFiles = [$(embedFile "fonts/SrcCodPro-Bold-PipeReduced.ttf")]
