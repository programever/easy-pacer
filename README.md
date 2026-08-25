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
- Stations are ordered by hand with up and down buttons; seeding from the GPX
  file sorts them by km once, and the start and finish stay pinned to the ends.
- Plan review before the start: checkpoints past the end of the course,
  duplicates, cutoffs out of order, and targets that disagree with the
  cutoffs. The start time anchors the timeline; races that cross midnight or
  run into a second day read correctly.
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
- Works with no signal once it has been opened: the page is kept on the phone
  and a refresh opens it from there. Add it to the home screen to open it like
  an app.
- Plans can be saved to a file and opened again; the file is named after the
  GPX it came from.

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

A function that operates on a type lives in that type's file.

There is no `Core/Api` and no `Api/`: this app has no backend. The equivalent of
T2 is `Storage` (localStorage and the shareable plan file) and the equivalent of
T3 is `Runtime/Ports` (the contract with the browser).

Coding conventions live in `CLAUDE.md`, and the checks in `devops/` enforce
them mechanically.

## The build

`devops/build.sh` runs Vite, which compiles the Elm, compiles `runtime/`,
minifies both and inlines them with the stylesheet into `dist/index.html`. The
app is used on a mountain with no signal; a single file that opens and runs is
a hard requirement, so the script then asserts that the output references
nothing outside itself. One more file sits beside it: `dist/sw.js`, the service
worker built from `runtime/sw.ts`, which keeps `index.html` on the phone so a
refresh with no signal still opens it. It has to be a separate file because a
browser fetches a service worker by URL; the app runs without it.

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
    npm install       # elm, elm-format, elm-test, elm-review, vite, pinned

    npm run start     # localhost, with hot reload on Elm and TypeScript
    npm run check     # elm make … && tsc --noEmit && elm-review
    npm run lint      # unused code     elm-review (config in review/)
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

The built app is `dist/index.html`, one self-contained file named so that the
folder can be published as-is, with `dist/sw.js` beside it. Serving over https
is what lets the browser grant geolocation and register the service worker.

## Contributing

Issues, feedback and pull requests are welcome at
<https://github.com/programever/easy-pacer>. The conventions in `CLAUDE.md` are
the review checklist; `npm run check && npm run format && npm test &&
npm run structure` is what the deploy runs.
