{-# LANGUAGE OverloadedLists #-}

module Main where

import qualified GI.Gtk                             as Gtk
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple
import qualified Data.Text                          as T
import           Data.Text                           ( Text )
import qualified Data.Vector                        as V
import           Data.Vector                         ( Vector )
import           Control.Monad                       ( void )

data State = State
  { todos :: Vector Item
  , textbox :: Text
  }

data Event
  = TextChanged Text
  | TodoAdded
  | Completed Int
  | Closed

data Item = Item
  { name :: Text
  }

main :: IO ()
main = void $ run App
  { view         = view'
  , update       = update'
  , inputs       = []
  , initialState = State { todos = mempty, textbox = mempty }
  }

view' :: State -> AppView Gtk.Window Event
view' s = bin
  Gtk.Window
  [#title := "TodoGTK+", on #deleteEvent (const (True, Closed))]
  (container Gtk.Box
             [#orientation := Gtk.OrientationVertical]
             [todoList, addTodo]
  )

 where
  todoList = container Gtk.Box
                       [#orientation := Gtk.OrientationVertical]
                       (fmap todoitem (todos s))
  todoitem item = widget Gtk.Label [#label := name item]
  addTodo = widget
    Gtk.Entry
    [ #text := textbox s
    , #placeholderText := "What do you need to do?"
    , onM #changed (fmap TextChanged . Gtk.entryGetText)
    , on #activate TodoAdded
    ]

update' :: State -> Event -> Transition State Event
update' s e = case e of
  Closed    -> Exit
  TodoAdded -> Transition
    s { todos   = V.snoc (todos s) (Item { name = textbox s })
      , textbox = T.empty
      }
    (pure Nothing)
  TextChanged t -> Transition s { textbox = t } (pure Nothing)
  _             -> Exit

