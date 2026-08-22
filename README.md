# Trạm Kế

A pacing companion for trail runners who are new to the sport. It answers three
questions and nothing else:

- how far is the next checkpoint, and how much climbing and descending is left
  to reach it
- how long until that checkpoint closes
- am I still on the course, and if not, which way is back

It does not predict pace. How fast to run is the runner's business, and a wrong
estimate delivered at two in the morning is worse than none.

Free, offline, and private: the app runs entirely on the phone, sends nothing
anywhere, and needs no account. It is served at
<https://programever.github.io/easy-pacer/>.

## Features

- Load the race's GPX file: distance, climb, descent and the elevation profile
  are read from it, and any checkpoints the file carries can be seeded in one
  tap.
- Type the cutoff table as the organiser publishes it. Stations without a
  cutoff are allowed; the countdown borrows the next one that has one.
- Plan review before the start: checkpoints past the end of the course,
  duplicates, cutoffs out of order, and legs that imply an impossible speed.
- During the race: the next checkpoint, how far, how much up and down, and
  how long until it closes, plus the full ledger of every station.
- The elevation profile with a draggable cursor, mirrored on the course map.
- GPS on demand only: one tap resolves your position onto the course,
  including out-and-back and looped sections, and tells you if you have left
  it and which way is back. Nothing runs in the background, so nothing drains
  the battery; to see new numbers the runner presses Lấy GPS or types the km.
- A reading less precise than 50 m is reported and not applied, so a bad fix
  under tree cover cannot move the milestone.
- Typed km as an alternative when GPS is not wanted.
- A ready-made help message with coordinates and the nearest course marker,
  sent by SMS or copied.
- The race survives a refresh or a closed tab.
- Plans can be saved to a file and opened again.

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

**Ports carry data, never decisions.** The TypeScript in `runtime/` reads a
sensor, parses XML, writes to storage or hands off to another app. It never
picks a milestone, never judges whether a runner is off course, never builds a
string a person reads. Elm's no-runtime-exception guarantee stops at the port,
so the port must not be where the thinking happens.

**The browser half of the boundary is typed.** `runtime/ports.ts` names the
shape of every message crossing a port, mirroring `src/Runtime/Ports.elm` and
the decoders beside it. `save` takes `unknown` on purpose: the snapshot belongs
to `Storage.Snapshot`, and a type for it here would invite this layer to read
it.

**Everything inbound is decoded.** A `Value` arriving from a port or from
storage becomes a `Result` before it goes near the model.

**English for code, Vietnamese for people.** Identifiers, comments and doc
comments are English. Vietnamese appears only inside string literals that are
rendered to the user. `devops/check.py` enforces this, in Elm and in
TypeScript, alongside its structural checks: module names match paths,
qualified names are imported, and exposed names exist.

**One file out.** `devops/build.sh` runs Vite, which compiles the Elm, compiles
`runtime/`, minifies both and inlines them with the stylesheet into
`dist/index.html`. The app is used on a mountain with no signal; a single file
that opens and runs is a hard requirement, so the script then asserts that the
output references nothing outside itself.

**Stylesheet over inline styles.** Elm names classes; `styles/app.css` owns the
look. Spacing between stacked controls lives there too.

## Illegal states this design removes

- `Screen = Setting SetupState | Racing RaceState` — racing without a course or
  a start time cannot be represented
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
- `Typing` on `SetupState` — the raw text in a focused box is separate from the
  parsed value, so a half-typed time is never rewritten under the cursor

## Working here

    nvm install       # Node, from .nvmrc
    npm install       # elm, elm-format, elm-test, vite, at the pinned versions

    npm run start     # localhost, with hot reload on Elm and TypeScript
    npm run check     # type check      elm make … && tsc --noEmit
    npm run format    # formatting      elm-format --validate src/ tests/
    npm test          # propositions    elm-test
    npm run structure # structural      python3 devops/check.py
    npm run build     # dist/index.html, and asserts it is self-contained

`elm.json` says which compiler this code needs; `package.json` is what fetches
it, and `.nvmrc` pins the Node that the tools run on. Nothing here is a
JavaScript dependency of the app; the tools are pinned exactly so that two
machines building the same commit produce the same file.

Run the commands through `npm`, which puts `node_modules/.bin` on `PATH`. The
first `elm make` downloads the dependencies in `elm.json` into `~/.elm`; that is
the only step that needs a network.

## Deploying

Pushing to `main` is the release. `.github/workflows/deploy.yml` runs every gate
above, then the build, then publishes `dist/` to GitHub Pages at
<https://programever.github.io/easy-pacer/>. If any gate fails, nothing is
published and the previous version stays live. Day to day work happens on
`development`; merging it into `main` is what ships.

The built file is `dist/index.html`, one self-contained file named so that the
folder can be published as-is. Serving it over https is what lets the browser
grant geolocation.

## Contributing

Issues, feedback and pull requests are welcome at
<https://github.com/programever/easy-pacer>. The rules above are the review
checklist; `npm run check && npm run format && npm test && npm run structure`
is what the deploy runs.
