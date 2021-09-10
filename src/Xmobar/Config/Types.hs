{-# LANGUAGE DeriveFunctor, StandaloneDeriving, GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Xmobar.Config
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jose A. Ortega Ruiz <jao@gnu.org>
-- Stability   :  unstable
-- Portability :  unportable
--
-- The configuration types
--
-----------------------------------------------------------------------------

module Xmobar.Config.Types
    ( -- * Configuration
      -- $config
      Config
    , ConfigF (..)
    , XPosition (..), Align (..), Border(..)
    ) where

import Data.Bifunctor (Bifunctor (..))
import Xmobar.Run.Runnable (Runnable(..))

-- $config
-- Configuration data type

-- | Convenience for backwards compatibility.
type Config = ConfigF Runnable String

-- | The configuration data type.
-- The original implementation of `Config` uses `String`s and
-- some cool read magic to deserialize the `Command`s.  For migrating
-- to dhall let's use the flexibility of a Bifunctor abstracting over
-- both the string members and the commands.
data ConfigF cmd str =
    ConfigF { font :: str            -- ^ Font
            , additionalFonts :: [str] -- ^ List of alternative fonts
            , wmClass :: str         -- ^ X11 WM_CLASS property value
            , wmName :: str          -- ^ X11 WM_NAME property value
            , bgColor :: str         -- ^ Backgroud color
            , fgColor :: str         -- ^ Default font color
            , position :: XPosition  -- ^ Top Bottom or Static
            , textOffset :: Int      -- ^ Offset from top of window for text
            , textOffsets :: [Int]   -- ^ List of offsets for additionalFonts
            , iconOffset :: Int      -- ^ Offset from top of window for icons
            , border :: Border       -- ^ NoBorder TopB BottomB or FullB
            , borderColor :: str     -- ^ Border color
            , borderWidth :: Int     -- ^ Border width
            , alpha :: Int           -- ^ Transparency from 0 (transparent)
                                     --   to 255 (opaque)
            , hideOnStart :: Bool    -- ^ Hide (Unmap) the window on
                                     --   initialization
            , allDesktops :: Bool    -- ^ Tell the WM to map to all desktops
            , overrideRedirect :: Bool -- ^ Needed for dock behaviour in some
                                       --   non-tiling WMs
            , pickBroadest :: Bool   -- ^ Use the broadest display
                                     --   instead of the first one by
                                     --   default
            , lowerOnStart :: Bool   -- ^ lower to the bottom of the
                                     --   window stack on initialization
            , persistent :: Bool     -- ^ Whether automatic hiding should
                                     --   be enabled or disabled
            , iconRoot :: FilePath   -- ^ Root folder for icons
            , commands :: [cmd]  -- ^ For setting the command,
                                     --   the command arguments
                                     --   and refresh rate for the programs
                                     --   to run (optional)
            , sepChar :: str         -- ^ The character to be used for indicating
                                     --   commands in the output template
                                     --   (default '%')
            , alignSep :: str        -- ^ Separators for left, center and
                                     --   right text alignment
            , template :: str        -- ^ The output template
            , verbose :: Bool        -- ^ Emit additional debug messages
            } deriving (Read, Show)

-- | A helper instance to avoid writing it by hand for all the stringy fields.
deriving instance Functor (ConfigF cmd)
-- | ConfigF as a bifunctor lets us read Dhall and transform to our legacy
-- config with a single data type.
instance Bifunctor ConfigF where
  bimap fCmd fStr conf = fmap fStr conf { commands = map fCmd $ commands conf }

data XPosition = Top
               | TopH Int
               | TopW Align Int
               | TopSize Align Int Int
               | TopP Int Int
               | Bottom
               | BottomH Int
               | BottomP Int Int
               | BottomW Align Int
               | BottomSize Align Int Int
               | Static {xpos, ypos, width, height :: Int}
               | OnScreen Int XPosition
                 deriving ( Read, Show, Eq )

data Align = L | R | C deriving ( Read, Show, Eq )

data Border = NoBorder
            | TopB
            | BottomB
            | FullB
            | TopBM Int
            | BottomBM Int
            | FullBM Int
              deriving ( Read, Show, Eq )
