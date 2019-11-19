module Main where

import Effect (Effect)
import Effect.Console (log)
import Effect.Class (liftEffect)
import Effect.Aff (Aff, delay, launchAff_)
import Effect.Aff.Compat
import Data.Time.Duration (Milliseconds (..))
import Data.Int (round, toNumber)
import Web.HTML (window)
import Web.HTML.Window (document)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.DOM.NonElementParentNode (getElementById)
import Data.Maybe (maybe)
import React.Basic (JSX, Self, createComponent, make, readState)
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture_)
import Prelude



foreign import setSize :: Int -> Effect Unit

type State = { current :: Int, high :: Int } 

incrementCurrent :: State -> State
incrementCurrent =
  \o -> o { current = o.current + 1
          , high = max o.high (o.current + 1)
          }

decrementCurrent :: State -> State
decrementCurrent =
  \o -> o { current = max (o.current - 1) 0
          }

component :: JSX
component =
  make (createComponent "App") {
    initialState: { current: 0, high: 0 }
  , render: \o -> 
      R.div_ [ R.div_ [ R.text ("Score: " <> show o.state.current) ]
             , R.div_ [ R.text ("High: " <> show o.state.high) ]
             , R.button { children: [ R.text "Click me" ] 
                        , onClick: capture_ (o.setState incrementCurrent)
                        }
             ]
  , didMount: \o -> launchAff_ (didMount o)
  , didUpdate: \o _ -> setSize (barPercent o.state)
  } unit

barPercent :: State -> Int
barPercent =
  \s -> round ((toNumber s.current / toNumber s.high) * 100.0)

didMount :: Self Unit State -> Aff Unit
didMount =
  \o -> do
    s <- liftEffect (readState o)
    delay (Milliseconds (300.0 - toNumber (4 * s.current)))
    liftEffect (o.setState decrementCurrent)
    didMount o

main :: Effect Unit
main = do
  log (getOrElse "empty" (Full "Brian"))
  log (getOrElse "empty" Empty)
  
  -- orElse (Full 1) (Full 2)
  -- > Full 2
  log (optionalToString show (orElse (Full 1) Empty))
  -- > Full 1
  log (optionalToString show (orElse Empty (Full 2)))
  -- > Full 2

  log (showList (twiceList (+) (Cons 1 (Cons 2 (Cons 3 Nil))) (Cons 4 (Cons 5 (Cons 6 Nil)))))
  log (showList (bindList (\x -> Cons x (Cons x Nil)) (Cons 1 (Cons 2 (Cons 3 Nil)))))

  w <- window
  d <- document w
  e <- getElementById "app" (toNonElementParentNode d)
  maybe (log "#app not found") (R.render component) e

data Optional a
  = Empty
  | Full a

-- <A> A getOrElse(A a, Optional<A> o);
getOrElse :: forall a. a -> Optional a -> a
getOrElse =
  \a o ->
    case o of
      Empty -> a
      Full x -> x

orElse :: forall a. Optional a -> Optional a -> Optional a
orElse =
  \a b ->
    case b of
      Empty -> a
      Full x -> Full x

optionalToString :: forall a. (a -> String) -> Optional a -> String
optionalToString =
  \f o ->
    case o of
      Empty -> "Empty"
      Full a -> "Full " <> f a

-- mapOptional show (Full 1)
-- Full "1"
-- mapOptional (show :: Int -> String) Empty
-- Empty
mapOptional :: forall a b. (a -> b) -> Optional a -> Optional b
mapOptional =
  \f o ->
    case o of 
      Empty -> Empty
      Full x -> Full (f x)

-- twiceOptional (+) (Full 1) (Full 2)
-- > Full 3
twiceOptional ::
  forall a b c.
  (a -> b -> c)
  -> Optional a
  -> Optional b
  -> Optional c
twiceOptional =
  \f oa ob -> 
    case oa of
      Empty -> Empty
      Full x ->
        mapOptional (f x) ob
        -- case ob of
        --   Empty -> Empty
        --   Full y -> Full (f x y)

bindOptional ::
  forall a b.
  (a -> Optional b)
  -> Optional a
  -> Optional b
bindOptional =
  \f oa ->
    case oa of
      Empty -> Empty
      Full x -> f x


data List a
  = Nil
  | Cons a (List a)

oneTwoThree :: List Int
oneTwoThree =
  Cons 1 (Cons 2 (Cons 3 Nil))

-- headOr 9 Nil
-- > 9
-- headOr 9 oneTwoThree
-- > 1
headOr :: forall a. a -> List a -> a
headOr =
  foldRight const

-- copy down, we'll need this

-- maybe :: b -> (a -> b) -> Maybe a -> b
-- maybe x f Nothing = x
-- maybe x f (Just 10) = f 10

foldRight :: forall a b. (a -> b -> b) -> b -> List a -> b
foldRight =
  \f b l ->
    case l of
      Nil -> b
      Cons a r -> f a (foldRight f b r)

-- foldRight f x Nil = x
-- foldRight f x (Cons 1 Nil) = f 1 x

-- foldRight const 9 oneTwoThree
-- Cons 1 (Cons 2 (Cons 3 Nil))
-- const 1 (const 2 (const 3 9))
-- > 1

-- foldRight const 9 Nil
-- > 9

-- mapList (+1) (Cons 1 (Cons 2 (Cons 3 Nil)))
-- > Cons 2 (Cons 3 (Cons 4 Nil))

-- > Cons (f 2) (Cons (f 3) (Cons (f 4) Nil))

mapList :: forall a b. (a -> b) -> List a -> List b
mapList =
  \f ->
    foldRight (\a -> Cons (f a)) Nil

-- twiceList (+) (Cons 1 (Cons 2 (Cons 3 Nil))) (Cons 4 (Cons 5 (Cons 6 Nil)))
-- > Cons 5 (Cons 6 (Cons 7 (Cons 6 (Cons 7 (Cons 8 (Cons 7 (Cons 8 (Cons 9 Nil))))))))

-- Cons 1 Nil
-- Cons 2 Nil
twiceList :: forall a b c. (a -> b -> c) -> List a -> List b -> List c
twiceList =
  \f la lb ->
    foldRight (\a y -> foldRight (\b z -> Cons (f a b) z) y lb) Nil la

-- bindList (\x -> Cons x (Cons x Nil)) (Cons 1 (Cons 2 (Cons 3 Nil)))
-- > Cons 1 (Cons 1 (Cons 2 (Cons 2 (Cons 3 (Cons 3 Nil)))))
bindList :: forall a b. (a -> List b) -> List a -> List b
bindList =
  \f l ->
    foldRight (\a bs -> foldRight Cons bs (f a)) Nil l

showList :: forall a. Show a => List a -> String
showList =
  foldRight (\a b -> "(Cons " <> show a <> " " <> b <> ")") "Nil"

class Functor f where
  map :: forall a b. (a -> b) -> f a -> f b

-- map :: forall a b. (a -> b) -> List a -> List b
-- map :: forall a b. (a -> b) -> Optional a -> Optional b

instance optionalFunctor :: Functor Optional where
  map = mapOptional

instance listFunctor :: Functor List where
  map = mapList

class Functor f <= Applicative f where
  twice :: forall a b c. (a -> b -> c) -> f a -> f b -> f c

-- twice :: forall a b c. (a -> b -> c) -> List a -> List b -> List c
-- twice :: forall a b c. (a -> b -> c) -> Optional a -> Optional b -> Optional c

instance optionalApplicative :: Applicative Optional where
  twice = twiceOptional

instance listApplicative :: Applicative List where
  twice = twiceList

class Applicative f <= Monad f where
  binds :: forall a b. (a -> f b) -> f a -> f b

instance optionalMonad :: Monad Optional where
  binds = bindOptional

instance listMonad :: Monad List where
  binds = bindList

-- binds :: forall a b. (a -> List b) -> List a -> List b
-- binds :: forall a b. (a -> Optional b) -> Optional a -> Optional b

-- foreverEffect :: Effect Unit -> Effect Unit
-- foreverEffect = \fa -> binds (\_ -> fa) fa

-- foreverEffect :: Aff Unit -> Aff Unit
-- foreverEffect = \fa -> binds (\_ -> fa) fa

forever :: forall f. Monad f => f Unit -> f Unit
forever = \fa -> binds (\_ -> fa) fa

-- foreign import readPasswd :: EffectFnAff String

-- main = launchAff_ do
--   p <- fromEffectFnAff readPasswd
--   liftEffect (log p)
