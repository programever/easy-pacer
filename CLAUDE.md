# CLAUDE.md

Guidance for Claude Code working in this repository.

## What this is

A trail-race pacing companion in Elm. `README.md` is the specification and is
kept current — read it first. This file adds what a README does not usually say.

## The rule that decides where code goes

**FTFC — Function follows Type, Type follows File, File follows Context.**

`ascentBetween` operates on a `Route`, so it lives in `Core/App/Route.elm`.
`Core/Data` is for types reusable in any project; `Core/App` is for types
specific to this one. A new `Utils.elm` is nearly always the wrong move.

## Coding style

**Exhaustive `case` over sum types — no `_ ->` catch-all on a union you own.**
Adding a variant should break compilation at every site that must now handle it.
A wildcard silently swallows the new case. Legitimate wildcards: matching on a
tuple where only one combination matters, and `Int` codes from outside.

**The stepdown rule.** A file reads top to bottom: the entry function first,
detail helpers below.

**Opaque types by default.** `Km`, `Distance`, `Route`, `Plan`, `Progress` and
`Candidate` are opaque. Exporting a record's internals invites callers to
recompute what the module already knows.

## Rules with teeth

- **Ports carry data, never decisions.** The TypeScript in `runtime/` reads a
  sensor, parses XML, writes to storage or hands off to another app. It never
  picks a milestone, never judges whether a runner is off course, never builds
  a string a person reads. Elm's no-runtime-exception guarantee stops at the
  port, so the port must not be where the thinking happens.
- **The browser half of the boundary is typed.** `runtime/ports.ts` names the
  shape of every message crossing a port, mirroring `src/Runtime/Ports.elm` and
  the decoders beside it. `save` takes `unknown` on purpose: the snapshot
  belongs to `Storage.Snapshot`, and a type for it here would invite this layer
  to read it.
- **Everything inbound is decoded.** A `Value` arriving from a port or from
  storage becomes a `Result` before it goes near the model. No `Decode.value`
  reaching the model.
- **English for code, Vietnamese for people.** Identifiers, comments and doc
  comments are English. Vietnamese appears only inside string literals that are
  rendered to the user.
- **Stylesheet over inline styles.** Elm names classes; `styles/app.css` owns
  the look. Spacing between stacked controls lives there too.

`python3 devops/check.py` enforces the port and language rules mechanically, in
Elm and in TypeScript, alongside its structural checks: module names match
paths, qualified names are imported, and exposed names exist. `elm-review`
(config in `review/`, part of `npm run check`) keeps unused code out.

## Why devops/check.py exists

The JavaScript original shipped two bugs that passed a syntax check and only
failed at runtime: a bulk edit deleted a block and left `renderEditor`
undefined, and a `const` was read one line above its own declaration. The
compiler catches both classes now, but the checker also runs where a compiler is
not available, and it enforces the language rule which no compiler knows about.

Before changing it, confirm it still catches all three: delete an import, rename
an exposed function at one call site, and put a Vietnamese word in a comment.

## Planning convention

Implementation plans are delivered for review BEFORE code, in the nine-section
shape used across haniker repos, in dependency order — the plan reads the way
the code compiles. In a type-first repo the type edit IS the plan.

1. Scope of work (and what is not in scope)
2. Solution approach, closing with the one-sentence invariant enforced
3. Types — real code, in dependency order
4. Functions — signatures and doc comments, FTFC-placed
5. Call-site deltas
6. Edit order — what the compiler will flag, in sequence
7. Illegal states removed
8. Tests as propositions
9. Gates and open items, including what has NOT been verified

Sub-number every item so review comments can name a point.

## iOS Safari lessons

- Safari on iOS ignores `touch-action` on SVG elements. The wrapper div
  (`.profile-card`, `.map-card`) carries `touch-action:none`, not just the SVG.
- The checkpoint editor list is rendered with `Html.Keyed`, keyed by
  checkpoint id, so a card that moves takes its DOM node — and any focused
  input — with it. Unkeyed, values swap between cards under the runner's
  finger. Keep it keyed.

## Keeping this file useful

If you learn something the next session would have wanted to know, add it here
in the same commit.
