module Main  where

import Prelude hiding (append)

import Control.Monad.Eff (kind Effect, Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.JQuery (JQuery, JQueryEvent, on, append, css, create, appendText, body, ready, setText, getValue)
import Control.Monad.Except (runExcept)
import DOM (DOM)
import DOM.HTML.Event.EventTypes (click)
import Data.Foldable (for_)
import Data.Foreign (readString)
import Partial.Unsafe (unsafePartial)
import Control.Monad.Eff.Timer as T

main :: forall eff. Eff ( dom :: DOM
                        , console :: CONSOLE
                        | eff
                        ) Unit
main =
  ready $ do
    -- Get the document body
    body <- body

    -- Create a text box
    div   <- create "<div>"
    input <- create "<input>"
    appendText "你的名字是: " div
    append input div
    append div body

    -- Create a paragraph to display a greeting
    greeting <- create "<p>"
    css { color: "red" } greeting
    append greeting body

    -- Listen for change events on the text box
    on "change" (handleChange input greeting) input
    on "click" (handleClick input div) input
  where
    handleChange
      :: JQuery
      -> JQuery
      -> JQueryEvent
      -> JQuery
      -> Eff ( dom :: DOM
             , console :: CONSOLE
             | eff
             ) Unit
    handleChange input greeting _ _ = unsafePartial do
      val <- getValue input
      for_ (runExcept (readString val)) \name -> do
        log $ "Name changed to " <> name
        setText ("Hello, " <> name) greeting

    handleClick
      :: JQuery
      -> JQuery
      -> JQueryEvent
      -> JQuery
      -> Eff ( dom :: DOM
             , console :: CONSOLE
             | eff
             ) Unit
    handleClick input div _ _ = unsafePartial do
      val <- getValue input
      setText ("Hello, " <> "clicked") div


