module Action exposing (Msg(..), RacingMsg(..), SettingMsg(..))

{-| Every way the state can change. Nothing mutates outside `update`, and the
compiler lists these for anyone adding a feature.
-}

import Core.App.Checkpoint as Checkpoint
import Core.App.Position exposing (Candidate)
import File exposing (File)
import Json.Encode as Encode
import State exposing (Field, Pixel)
import Time


type Msg
    = GotZone Time.Zone
    | Tick Time.Posix
    | SettingChanged SettingMsg
    | RacingChanged RacingMsg
    | GpsArrived Encode.Value
    | GpxArrived Encode.Value
    | ShowAbout
    | CloseDialog


type SettingMsg
    = PickGpxFile
    | GpxFileSelected File
    | GpxTextRead String
    | SeedFromWaypoints
      -- One of the bundled race plans, picked from the dropdown by its label.
    | SeedPlanChosen String
    | AddStation
    | RemoveStation Checkpoint.Id
    | MoveStationUp Checkpoint.Id
    | MoveStationDown Checkpoint.Id
    | EditName Checkpoint.Id String
      -- Raw text in a date, time, km or cutoff box, as typed.
    | Typed Field String
      -- The box lost focus: stop showing raw text and settle on the parsed value.
    | CommitTyping
    | UseToday
    | UseTomorrow
    | UseCurrentTime
      -- Horizontal position on the elevation chart, in CSS pixels, and the chart's width.
    | ScrubSetup Float Float
    | ReviewPlan
    | StartRace
    | ConfirmStartRace
    | ExportPlan
    | PickPlanFile
    | PlanFileSelected File
    | PlanTextRead String


type RacingMsg
    = SwitchTab Bool
    | RequestGps
    | ChoosePosition Candidate
    | KeepPosition
    | ToggleKmEntry
    | EditKmEntry String
    | SubmitKmEntry
    | ScrubProfile Float Float
      -- Pointer id, position in CSS pixels, and the map's width in CSS pixels.
    | MapPointerDown Int Pixel
    | MapPointerMove Int Pixel Float
    | MapPointerUp Int
    | MapWheel Float
    | ViewMe
    | ViewWhole
    | EditPhone String
    | SendSos
    | CopySos
    | OpenPlanEditor
    | RequestQuit
    | QuitToSetup
    | QuitAndErase
