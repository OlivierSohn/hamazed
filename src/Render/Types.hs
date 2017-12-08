module Render.Types
            ( RenderSize(..)
            ) where

data RenderSize = TerminalSize
                | UserDefined !Int !Int
