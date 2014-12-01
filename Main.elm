import Html
import Html (..)
import Html.Events (..)
import Window
import Graphics.Input (..)
import Graphics.Input as Input
import Debug (log)
import String (split, indexes, dropLeft, left, show)
import Array (toIndexedList, fromList)

data Action
    = NoOp
    | NoteSelected Note
    | ChordSelected Chord

type Chord =
    { first: Int
    , second: Int
    , third: Int
    , fourth: Maybe Int
    , name: String }

type Note =
    { val: String
    , num: Int
    , selected: Bool }

type Strang =
    { notes: [Note]
    , leadNote: Note }

type State =
    { selectedNote: Note
    , chordSelectNotes: [Note]
    , chord: Chord }

baseNotes : [String]
baseNotes = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B", "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B", "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]

scale : [Note]
scale = map (\n -> newNote n) baseNotes

major : Chord
major = {first=0, second=4, third=7, fourth=Nothing, name="major"}
minor : Chord
minor = {first=0, second=3, third=7, fourth=Nothing, name="minor"}

chords = [major, minor]

strangs = [(newStrang 2), (newStrang 7), (newStrang 11), (newStrang 2)]

newNote : String -> Note
newNote value =
    { val = value
    , selected = False
    , num = findNoteIndex value }

newStrang : Int -> Strang
newStrang leadNoteIndex =
    let notes = take 12 (drop leadNoteIndex scale)
    in { notes = notes
       , leadNote = head notes }

findNoteIndex : String -> Int
findNoteIndex note =
    let indexedNotes = toIndexedList (fromList baseNotes)
        filteredNotes = filter (\x -> snd x == note) indexedNotes
    in fst (head filteredNotes)

findNote : String -> [String] -> [String]
findNote note listOfNotes =
    case listOfNotes of
      [] -> [""]
      x :: xs -> if note == x
                 then xs
                 else findNote note xs

getNoteClass : State -> Note -> String
getNoteClass state note =
    let selectedNote = state.selectedNote.num
        noteVal = note.num
        chord = state.chord
        first = head <| drop (selectedNote + chord.first) scale
        second = head <| drop (selectedNote + chord.second) scale
        third = head <| drop (selectedNote + chord.third) scale
    in if | noteVal == first.num -> " firstNote"
          | noteVal == second.num -> " secondNote"
          | noteVal == third.num -> " thirdNote"
          | otherwise -> ""

--
-- Views
--

fretNoteView : State -> Note -> Html
fretNoteView state note =
    let className = "fretNote"
        selected = getNoteClass state note
    in
    node "li"
         ["className" := className ++ selected]
         []
         [node "span" [] []
          [text note.val]
         ]

firstFretNoteView : State -> Note -> Html
firstFretNoteView state note =
    let className = "fretNote first"
        selected = getNoteClass state note
    in
    node "li"
         ["className" := className ++ selected]
         []
         [node "span" [] []
          [text note.val]
         ]

strangView : State -> Strang -> Html
strangView state strang =
    -- let firstNote = firstFretNoteView state (head strang.notes)
    let firstNote = firstFretNoteView state strang.leadNote
        notes = firstNote ::
                (map (\n -> fretNoteView state n) (drop 1 strang.notes))
    in
    node "li"
             ["className" := "string"] []
             [ node "ul" [] []
               notes
             ]

fretView : State -> Html
fretView state =
    node "div"
         [ "className" := "fretView" ] []
         [ node "ul" [] []
           (map (\n -> strangView state n) strangs)
         ]

chordNoteView : State -> Note -> Html
chordNoteView state note =
    let baseClass = "chordSelectNote"
        selected = if note.val == state.selectedNote.val
                   then " selected"
                   else ""
    in
    eventNode "div"
         [ "className" := baseClass ++ selected ]
         []
         [ onclick actions.handle (\_ -> NoteSelected note) ]
         -- wrapped in span so we can animate just the span
         -- and not the whole div (which was fucking with layout)
         [ node "span" [ "className" := selected ] []
           [ text note.val ]
         ]

chordTypeView : State -> Chord -> Html
chordTypeView state chord =
    let className =
            if | state.chord.name == chord.name -> " selected"
               | otherwise -> ""
    in
    eventNode "div"
              [ "className" := "chordTypeSelector" ++ className ]
              []
              [ onclick actions.handle
                (\_ -> ChordSelected chord) ]
              [ node "span" [ "className" := className ] []
                [ text chord.name ]
              ]

chordSelectView : State -> Html
chordSelectView state =
    node "div"
         [ "className" := "chordSelectNotes" ]
         []
         [ node "div" [] []
           (map (\n -> chordNoteView state n) state.chordSelectNotes)
         , node "div" [ "className" := "chordTypeSelectors" ] []
           (map (chordTypeView state) chords)
         ]

--
-- Tie application together
--

view : State -> Html
view state =
    node "div" [ "className" := "main" ] []
             [ chordSelectView state
             , fretView state ]

actions : Input Action
actions = Input.input NoOp

step : Action -> State -> State
step action state =
    case action of
      NoOp -> state
      NoteSelected note -> { state | selectedNote <- note }
      ChordSelected chord -> { state | chord <- chord }

state : Signal State
state = foldp step startingState actions.signal

startingState : State
startingState = { selectedNote = newNote "C"
                , chordSelectNotes = (newStrang 0).notes
                , chord = major }

scene : State -> (Int, Int) -> Element
scene state (w, h) =
    container w h bottomRight (Html.toElement w h (view state))

main : Signal Element
main = lift2 scene state Window.dimensions
