{-# LANGUAGE OverloadedLists #-}

module Main where

import qualified GI.Gtk                             as Gtk
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple
import           Data.Text                           ( Text )
import qualified Data.Vector                        as V
import           Data.Vector                         ( Vector )
import qualified Data.Vector.Mutable                as MVector
import           Control.Monad                       ( void )

data State = State
  { todos :: Vector Item
  , textbox :: Text
  }

data Event
  = TextChanged Text
  | TodoAdded
  | TodoCompleted Int
  | ClearCompleted
  | ClearAll
  | Closed

data Item = Item
  { name :: Text
  , completed :: Bool
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
             [todoList, bottomRow]
  )
 where
  todoList =
    BoxChild defaultBoxChildProperties { expand = True, fill = True }
      $ container Gtk.Box
                  [#orientation := Gtk.OrientationVertical]
                  (V.imap todoitem (todos s))
  todoitem i item =
    bin Gtk.CheckButton
        [#active := completed item, on #toggled (TodoCompleted i)]
      $ widget
          Gtk.Label
          [ #label := completedMarkup item
          , #useMarkup := True
          , #halign := Gtk.AlignStart
          ]
  bottomRow = container Gtk.Box
                        [#orientation := Gtk.OrientationHorizontal]
                        [clearCompleted, addTodo, clearAll]
  clearCompleted =
    widget Gtk.Button [#label := "Clear", on #clicked ClearCompleted]
  clearAll = widget Gtk.Button [#label := "Remove All", on #clicked ClearAll]
  addTodo =
    BoxChild defaultBoxChildProperties { expand = True, fill = True } $ widget
      Gtk.Entry
      [ #text := textbox s
      , #placeholderText := "What do you need to do?"
      , onM #changed (fmap TextChanged . Gtk.entryGetText)
      , on #activate TodoAdded
      ]
  completedMarkup :: Item -> Text
  completedMarkup item | completed item = "<s>" <> name item <> "</s>"
                       | otherwise      = name item

update' :: State -> Event -> Transition State Event
update' s e = case e of
  TodoAdded ->
    let newitem = Item { name = textbox s, completed = False }
    in  Transition s { todos = V.snoc (todos s) newitem, textbox = mempty }
                   (pure Nothing)
  TextChanged t -> Transition s { textbox = t } (pure Nothing)
  TodoCompleted i ->
    Transition s { todos = mapAt i toggleComplete (todos s) } (pure Nothing)
  ClearCompleted ->
    Transition s { todos = V.filter (not . completed) (todos s) } (pure Nothing)
  ClearAll -> Transition s { todos = mempty } (pure Nothing)
  Closed   -> Exit
 where
  mapAt :: Int -> (a -> a) -> Vector a -> Vector a
  mapAt i f = V.modify
    (\v -> do
      v' <- MVector.read v i
      MVector.write v i (f v')
    )
  toggleComplete :: Item -> Item
  toggleComplete item = item { completed = not (completed item) }

