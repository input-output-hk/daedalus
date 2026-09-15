# Finding: source and build config depend on packages nothing declares

**Status:** open, not scheduled
**Raised from:** the Storybook 6.4.22 to 8.6.18 upgrade
**Scope:** dependency management, repository-wide
**Severity:** low most of the time, high at the moment it fires. Nothing is at
risk while the tree is stable; the failure arrives during an unrelated upgrade,
in files the upgrade never touched.

---

## What happened

Removing four Storybook packages from `package.json` broke two things that have
nothing to do with Storybook.

`source/renderer/app/i18n/translations.ts:2` calls `require.context`, a webpack
construct whose TypeScript declaration comes from `@types/webpack-env`. That
package was never in `package.json`. It was in `yarn.lock`, pulled in by
Storybook 6.4.22, and `tsc` found it because Yarn 1 hoists flat. With Storybook
6 gone the types went, and the file that had compiled for years stopped
compiling.

`storybook/main.ts` calls `require.resolve('os-browserify/browser')` for its
`os` fallback. Same shape: never declared, supplied transitively, and the
webpack config stopped resolving the moment the supplier left.

Both were fixed by declaring them. That is the correct fix and it is not the
finding.

## Why it matters

The dependency a file needs and the dependency a manifest declares are two
different sets, and nothing in this repository compares them.

A flat-hoisted `node_modules` makes every transitive package importable from
everywhere. Anything reachable that way works, and keeps working, for exactly as
long as some other dependency happens to keep providing it. The connection
between the file that needs the package and the package that supplies it exists
nowhere: not in the manifest, not in the lockfile, not in a comment.

So the failure is displaced in three ways at once. In time, by however long it
takes for the supplying dependency to move, four years in this case. In space,
from the dependency that changed to a file nobody edited. And in appearance:
`Property 'context' does not exist on type 'NodeRequire'` reads as a defect in
the translations module rather than as a manifest that was always incomplete.

The same pattern is why the upgrade's first symptom was
`Can't resolve react-dom/client`, which reads as React 16 being unsupported. It
was not. Diagnosis cost more than the fix in every one of these cases.

## How many more there are

Unknown, and the two halves of the problem are not equally tractable.

Modules named in build configuration can be swept. Every `require.resolve` in
`storybook/main.ts` was resolved against `package.json` in one pass, which found
`os-browserify` and confirmed the other eleven were declared. That sweep took
minutes and would have found the second gap at the same time as the first,
rather than one rebuild later. It is worth running against any config file that
names modules by string.

Type packages have no equivalent. `@types/webpack-env` is not imported by name
anywhere: it is an ambient declaration that `tsc` picks up from
`node_modules/@types`, so there is no specifier to grep for and no import to
resolve. The dependency is real and invisible. Nothing here enumerates which
ambient type packages the program relies on, and removing one is only detectable
by compiling.

Nothing in CI checks either category. `yarn compile` catches a missing type
package only after it has already gone; it cannot say that one is
undeclared while something else still provides it.

## What a fix would have to decide

- **Whether to detect or to prevent.** Detection is a check that the declared
  set covers what is used, run in CI. Prevention is a resolution mode that
  refuses undeclared access, which for Yarn 1 means leaving Yarn 1.
- **Whether ambient types are in scope.** They are the harder half and the half
  that bit hardest here. A tool that only checks `import` specifiers would have
  missed `@types/webpack-env` entirely.
- **What to do with the existing set.** A first run will report more than these
  two. Some will be genuine, some will be tooling noise, and someone has to
  triage that before the check can be made blocking.
- **Where the cost lands.** Yarn 1 is pinned at `1.22.21` and the Nix build
  installs offline from a lockfile-derived cache, so any change to resolution
  semantics touches the build, not just the manifest.

## What is true today

- `@types/webpack-env@1.16.3` and `os-browserify@0.3.0` are now declared in
  `package.json`. `@storybook/preview-api@8.6.18` was declared for the same
  reason before it could become a third instance.
- No sweep exists for either category, and none was added by the work that
  turned this up.
- The build-config sweep is a few minutes by hand and is worth repeating
  whenever a build dependency is removed.
