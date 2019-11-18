module Main where

import Data.Foldable (traverse_)
import Data.Int (toNumber)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay, launchAff_)
import Effect.Class (liftEffect)
import React.Basic (JSX, Self, createComponent, make, readState)
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture_)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

import Prelude

type State = { current :: Int, high :: Int } 

foreign import sizeBar :: State -> Effect Unit

onClick :: Self Unit State -> Effect Unit
onClick self =
  self.setState \s -> updateHigh (s { current = s.current + 1 })

updateHigh :: State -> State
updateHigh state =
  if state.current > state.high
  then state { high = state.current }
  else state

render :: Self Unit State -> JSX
render self =
  R.div_ [ R.div_ [ R.text (show self.state.current) ]
         , R.div_ [ R.text (show self.state.high) ]
         , R.button { onClick: capture_ (onClick self)
                    , children: [ R.text "Click me" ]
                    }
         ]

counter :: JSX
counter =
  make (createComponent "Counter") {
    render
  , initialState: { current: 0, high: 0 }
  , didMount: \self -> launchAff_ (didMount self)
  , didUpdate: \self _ -> sizeBar self.state
  } unit

didMount :: Self Unit State -> Aff Unit
didMount self = do
  s <- liftEffect (readState self)
  delay $ Milliseconds (toNumber (300 - (4 * s.current)))
  liftEffect (self.setState (\s' -> s' { current = max 0 (s'.current - 1) }))
  didMount self

main :: Effect Unit
main = do
  sizeBar { current: 10, high: 100 }
  d <- window >>= document
  container <- getElementById "app" (toNonElementParentNode d)
  traverse_ (R.render counter) container
