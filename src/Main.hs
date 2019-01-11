{-# LANGUAGE OverloadedLists #-}

module Main where

import qualified GI.Gtk                             as Gtk
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple

type State = ()
data Event = Closed


main :: IO ()
main =
  run App { view = view', update = update', inputs = [], initialState = () }

view' :: State -> AppView Gtk.Window Event
view' s = bin 
  Gtk.Window 
  [#title := "Demo"
  , on #deleteEvent (const (True, Closed))
  ] 
  $ widget Gtk.Label [#label := "Hello, World!"]

update' :: State -> Event -> Transition State Event
update' _ Closed = Exit
