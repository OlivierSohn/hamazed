
module Render.Delta.Internal.DefaultPolicies
           where


import System.IO(BufferMode(..))

import Render.Delta.Types
import Color


defaultResizePolicy :: ResizePolicy
defaultResizePolicy = MatchTerminalSize

defaultClearColor :: Color8Code
defaultClearColor = black

defaultClearPolicy :: ClearPolicy
defaultClearPolicy = ClearAtEveryFrame

defaultStdoutMode :: BufferMode
defaultStdoutMode =
  BlockBuffering $ Just maxBound -- maximize the buffer size to avoid screen tearing
