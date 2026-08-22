# Trạm Kế

A pacing companion for trail runners who are new to the sport. It answers three
questions and nothing else:

- how far is the next checkpoint, and how much climbing and descending is left
  to reach it
- how long until that checkpoint closes
- am I still on the course, and if not, which way is back

It does not predict pace. How fast to run is the runner's business, and a wrong
estimate delivered at two in the morning is worse than none.

## Shape of the code

Written in Elm, following The Elm Architecture, and organised by **FTFC —
Function follows Type, Type follows File, File follows Context**.

    Core/Data   types reusable in any project    Distance, Clock, DateOnly, ...
    Core/App    types specific to this one       Route, Checkpoint, Plan, ...
    Storage     external formats, always decoded PlanFile, Snapshot
    Runtime     the port boundary, Elm side      Ports, Gps, Gpx
    runtime/    the port boundary, browser side  geolocation.ts, gpx.ts, ...
    State       the model
    Action      every way the model can change
    Page        one module per screen
    View        reusable rendering

A function that operates on a type lives in that type's file. Reaching for a new
`Utils.elm` is nearly always the wrong move here.

There is no `Core/Api` and no `Api/`: this app has no backend. The equivalent of
T2 is `Storage` (localStorage and the shareable plan file) and the equivalent of
T3 is `Runtime/Ports` (the contract with the browser).

## Rules

**Ports carry data, never decisions.** The JavaScript reads a sensor, parses
XML, writes to storage or hands off to another app. It never picks a milestone,
never judges whether a runner is off course, never builds a string a person
reads. Elm's no-runtime-exception guarantee stops at the port, so the port must
not be where the thinking happens.

**Everything inbound is decoded.** A `Value` arriving from a port or from
storage becomes a `Result` before it goes near the model.

**English for code, Vietnamese for people.** Identifiers, comments and doc
comments are English. Vietnamese appears only inside string literals that are
rendered to the user. `devops/check.py` enforces this.

**One file out.** `devops/build.sh` runs Vite, which compiles the Elm, compiles
`runtime/`, minifies both and inlines them with the stylesheet into
`dist/tram-ke.html`. The app is used on a mountain with no signal; a single file
that opens and runs is a hard requirement, not a convenience, so the script then
greps the output for any `src` or `href` that is not a data URI and fails the
build if it finds one. The requirement is checked, not merely intended.

**The browser half of the boundary is typed.** `runtime/ports.ts` names the shape
of every message crossing a port, mirroring `src/Runtime/Ports.elm` and the
decoders beside it. Elm generates none of this, so the mirror is maintained by
hand and `tests/wire/` is what keeps it honest. `save` takes `unknown` on
purpose: the snapshot belongs to `Storage.Snapshot`, and a type for it here
would invite this layer to read it.

## Working here

    nvm install       # Node, from .nvmrc
    npm install       # elm, elm-format, elm-test, at the pinned versions

    npm run start     # localhost, with hot reload on Elm and TypeScript
    npm run check     # type check      elm make … && tsc --noEmit
    npm run format    # formatting      elm-format --validate src/
    npm test          # propositions    elm-test
    npm run structure # structural      python3 devops/check.py
    npm run build     # dist/tram-ke.html, and asserts it is self-contained

`elm.json` says which compiler this code needs; `package.json` is what fetches
it, and `.nvmrc` pins the Node that `elm-test` runs on. Nothing here is a
JavaScript dependency of the app: the three tools are the whole reason the file
exists, and they are pinned exactly, because two machines building the same
commit should produce the same single file.

Run them through `npm` rather than directly. `npm` puts `node_modules/.bin` on
`PATH`, which is how `devops/build.sh` finds `elm` without a global install;
invoked straight from the shell it will not.

The first `elm make` downloads the dependencies in `elm.json` into `~/.elm`.
That is the only step that needs a network. After it, the build is as offline as
the app it produces.

`devops/check.py` exists because two real bugs in the JavaScript original passed
a syntax check and only failed at runtime: a deleted block left a function
undefined, and a variable was read above its own declaration. It catches
undefined module references, names that are not exposed, and non-English text
outside string literals.

## Illegal states this design removes

- `Screen = Setting Draft | Racing RaceState` — racing without a course or a
  start time cannot be represented
- `Plan.fromDraft : Draft -> Result (NonEmpty Issue) Plan` — the plan review is
  the constructor, not advice
- `Status = Pending | Passed Posix` — no nullable arrival time to leave stale
- `Source = FromGps Fix | FromRunner` — a declared position cannot carry a GPS
  accuracy
- `Resolution = Resolved { progress, nearest }` — the milestone used for
  distance and the point used to guide someone back are different fields, so
  they cannot be confused
- `Km` and `Distance` are different types — a milestone cannot be added to a
  milestone
- `Cutoff = NoCutoff | ClosesAt Clock` — an empty string cannot mean "no cutoff"

## Status

The port from the single-file JavaScript build is complete in structure. The
original is kept in `legacy/` as the oracle: the numbers this produces on the
fixture course must match it before `legacy/` is deleted.
