module Main  where

import Prelude hiding (append)

import Control.Monad.Eff (kind Effect, Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.JQuery (JQuery, JQueryEvent, on, append, css, create, appendText, body, ready, setText, getValue, setValue)
import Control.Monad.Eff.Ref as R
import Control.Monad.Eff.Timer as T
import Control.Monad.Except (runExcept)
import DOM (DOM)
import DOM.HTML.Event.EventTypes (click)
import Data.Foldable (for_)
import Data.Foreign (readString)
import Data.String.CodePoints (count)
import Partial.Unsafe (unsafePartial)
import Layout.Main (mainlayout)

  
main :: forall eff. Eff (dom :: DOM, ref :: R.REF , timer :: T.TIMER , console :: CONSOLE | eff ) Unit
main = 
  ready $ do
    mainlayout


