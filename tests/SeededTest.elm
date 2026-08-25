module SeededTest exposing (suite)

{-| The bundled race plans are data, and data rots silently. The proposition:
every seeded plan decodes as a complete plan file — a course, a start line and
a finish line — so a broken bundle fails here instead of on a runner's phone.
-}

import Core.App.Checkpoint as Checkpoint
import Expect
import Json.Decode as Decode
import Storage.PlanFile as PlanFile
import Storage.Seeded as Seeded
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Seeded plans"
        (List.map decodes Seeded.plans)


decodes : { label : String, json : String } -> Test
decodes plan =
    test (plan.label ++ " decodes with a course, a start and a finish") <|
        \_ ->
            case Decode.decodeString PlanFile.decoder plan.json of
                Err error ->
                    Expect.fail (Decode.errorToString error)

                Ok draft ->
                    let
                        roles =
                            List.map .role draft.checkpoints
                    in
                    if draft.route == Nothing then
                        Expect.fail "no route in the bundled file"

                    else if not (List.member Checkpoint.StartLine roles) then
                        Expect.fail "no start line in the bundled file"

                    else if not (List.member Checkpoint.FinishLine roles) then
                        Expect.fail "no finish line in the bundled file"

                    else
                        Expect.pass
