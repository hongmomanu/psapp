module Layout.Main  where

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

mainlayout :: forall eff.  Eff (dom :: DOM, ref :: R.REF , timer :: T.TIMER , console :: CONSOLE | eff ) Unit
mainlayout = do
    mycounts <- R.newRef 0
    body <- body
    -- Create a text box
    div   <- create "<div>"
    addbtn <- create "<button>"
    setText "+" addbtn
    miubtn <- create "<button>"
    setText "-" miubtn
    append addbtn div
    append miubtn div
    append div body

    -- Create a paragraph to display a counter
    counter <- create "<p>"
    css { color: "red" } counter
    append counter body

    -- Listen for change events on the text box
    on "click" (addClick addbtn counter mycounts) addbtn
    on "click" (miuClick miubtn counter mycounts) miubtn
  where
    addClick
      :: JQuery
      -> JQuery
      -> (R.Ref Int)
      -> JQueryEvent
      -> JQuery
      -> Eff (dom :: DOM, ref :: R.REF , timer :: T.TIMER , console :: CONSOLE | eff ) Unit
    addClick addbtn counter mycounts _ _ = unsafePartial do
      -- mycount <- counts
      R.modifyRef mycounts (_ + 4)
      n <- R.readRef mycounts
      setText (show n) counter

    miuClick
      :: JQuery
      -> JQuery
      -> (R.Ref Int)
      -> JQueryEvent
      -> JQuery
      -> Eff ( dom :: DOM, ref :: R.REF , timer :: T.TIMER , console :: CONSOLE | eff ) Unit
    miuClick miubtn counter mycounts _ _ = unsafePartial do
      R.modifyRef mycounts (_ - 4)
      n <- R.readRef mycounts
      setText (show n) counter
      -- void $ T.setInterval 10 do
      --   log "timeout increment counter"
      -- val <- getValue input
      -- setText ("Hello, " <> "clicked") div

