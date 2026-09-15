Implementation: Iteration 1
Timestamp: 2026-09-15T08:05:30Z

Changes made:
- `package.json`: removed `@storybook/builder-webpack5`, `@storybook/manager-webpack5`,
  `@storybook/core` and `@storybook/addons`; moved `@storybook/react`, `@storybook/addon-actions`
  and `@storybook/addon-links` to 8.6.18; bumped `@storybook/addon-knobs` 6.4.0 to 8.0.1; added
  `storybook` and `@storybook/react-webpack5` at 8.6.18 and the four packages the knobs addon peers;
  declared `os-browserify` and `@types/webpack-env`; changed both script bodies to the new binaries
  and added `--disable-telemetry`.
- `storybook/main.ts`: added the `framework` field, removed `core.builder`, and carried the
  `config.resolve` merge fix.
- `storybook/preview.tsx`: converted to a default-export Preview object, keeping the frozen clock and
  both side-effect imports.
- `yarn.lock`: regenerated with the pinned yarn, 1330 insertions against 4385 deletions.

Verification run:
- `nix build .#internal.x86_64-linux.node_modules` succeeds, which is what establishes that the
  manifest and the lockfile agree under `--frozen-lockfile`.
- `@storybook/addons` appears nowhere in `package.json` or `yarn.lock`.
- All four legacy-decorator settings are present after the edit, checked rather than assumed:
  `experimentalDecorators` in `tsconfig.json`, and `decorators`, `legacyDecorator` and
  `useDefineForClassFields` in the `swc-loader` rule.
- `compile` is red with exactly 70 errors: 68 `TS2305` for `storiesOf` and 2 `TS2307` for
  `@storybook/addons` in the DaedalusMenu addon. Nothing outside those two categories, so every
  failure is owned by `task-016` or by `task-017` and the tranches.
- `storybook` is red, failing on `Could not resolve "@storybook/addons"` in
  `storybook/addons/DaedalusMenu/register.tsx`, which `task-016` removes.
- `lint` exit 0 at 5427 warnings and no errors; `i18n` and `stylelint` exit 0.

Three findings, all of which changed the change:

- **`yarn storybook:build` returned exit 0 on a failed build.** The first run of the check after the
  manifest edit reported success. Forcing a rebuild showed the manager build had failed with
  `Could not resolve "@storybook/addons"`, after which the 8.6 CLI tried to prompt
  `Would you like to help improve Storybook by sending anonymous crash reports?`; with no TTY the
  prompt resolves and the process exits 0, and the derivation's `installPhase` then ran. The
  required check would have been green on every failure for the rest of this phase. Adding
  `--disable-telemetry` makes the same tree exit 1. This is the second silent-success defect this
  plan has hit, after the codemod reporting `1 ok` while dropping registrations, and it is the more
  serious because it disables the check the whole phase leans on.
- **`os-browserify` was undeclared.** `storybook/main.ts` `require.resolve`s it for the `os`
  fallback, and the 6.4.22 tree happened to supply it transitively. Rather than fix it and rebuild,
  every module `webpackFinal` resolves was checked against the manifest in one pass; that and
  `@types/webpack-env` were the only two gaps.
- **`@types/webpack-env` was undeclared.** `source/renderer/app/i18n/translations.ts:2` uses
  `require.context`, whose typing came from a package the old lockfile carried transitively. It is
  application source depending on a devDependency's transitive types.

Deviations from the approved plan:
- The second acceptance criterion says `storybook:build` should fail only on unconverted `storiesOf`
  files and not on configuration. It fails on the local DaedalusMenu addon instead, in the manager
  build, before the preview is reached. That is not a defect in this task: the addon imports
  `@storybook/addons`, which has no 8.x release, and `task-016` exists to replace it. A probe with
  the addon entry temporarily removed confirmed the preview then builds far enough to reach the
  `os-browserify` gap, which is how that was found. The probe was reverted.

User interaction is now required:
- No, but the `--disable-telemetry` finding is worth propagating: any future Storybook CLI invocation
  in CI needs it, or the check reports green on failure.

Outcome: The hop is open, the manifest and lockfile agree, every remaining failure is owned by a
named later task, and the check that would have hidden them is fixed; ready for code review

Code Review: Iteration 1
Timestamp: 2026-09-15T08:11:15Z

Summary:
- Approved. The window is open with a counted, classified failure set and nothing unaccounted for.

Blocking findings:
- None.

Non-blocking observations:
- The telemetry finding is the most valuable thing in this task and it was nearly missed. The check
  went green, which is what a correct hop would also have looked like, and the only reason it was
  questioned is that green was the wrong answer for a manifest that had just deleted a package the
  addon imports. Expecting a specific failure, and treating an unexpected pass as a defect rather
  than as luck, is what caught it.
- Counting the compile errors and classifying all 70 turns the red window from a state into a
  measurement. `task-017` and the tranches now have a number to drive to zero rather than an
  instruction to make it compile.
- Checking every `require.resolve` in `webpackFinal` against the manifest in one pass, rather than
  fixing `os-browserify` and rebuilding, is the right response to a class of defect that arrives one
  build at a time.
- Both undeclared dependencies are latent defects the hop exposed rather than created. The manifest
  has been relying on a devDependency's transitive graph to supply things application source needs.
- The deviation on the second acceptance criterion is real and correctly attributed. Recording that
  the criterion cannot be met in the order the graph assumes, with the probe that establishes what
  lies behind it, is better than declaring it met.

Approval bar:
- Met. `task-015` is complete and `task-016` is unblocked.

Decision: approved
