# Node.js dependency security update

Branch: `nodejs-deps-update`
Base: upstream master `22440487d08fd1d2121394fa80c238852df5ea7d`

## Scope and existing work

Continue the completed dependency-hygiene work rather than repeating its
formatter, entropy, and seed-derivation changes. Reuse its crypto conformance
tests and advisory-ownership research. Address the current open Dependabot
baseline and the Electron exposure findings discussed with Sam.

## User stories

1. As a wallet user, I receive compatible patched dependencies without changes
   to recovery phrases, seed derivation, balances, or hardware signing semantics.
   Given the updated graph, crypto known-answer tests retain their outputs.
2. As a wallet user, remote news cannot inject executable HTML into the privileged
   renderer, and remote navigation/popups cannot acquire application privileges.
   Given hostile markup or navigation, active content is rejected and expected
   supported links continue to open safely in the external browser.
3. As a maintainer, I can reproduce the updated dependency graph and distinguish
   resolved advisories from unsupported updates and alerts outside the JS graph.
   Given an outstanding alert, its package, owner, blocker, and residual exposure
   are recorded; it is never silently dismissed or counted as resolved.

## Requirements

- Update direct dependencies and vulnerable transitive copies; remove obsolete
  vulnerable resolutions. Preserve compatibility and justify cross-major overrides.
- Keep Electron package, downloaded binary sources, and packaging assertions in sync.
- Attach popup and navigation controls to the main window before loading content.
- Remove raw remote HTML rendering; preserve ordinary Markdown and legitimate links.
- Review CSP and renderer privilege reduction against actual preload/IPC consumers;
  do not flip isolation flags without migrating dependent APIs and validating them.
- Preserve master and its untracked user files; all changes belong in this worktree.
- Do not publish, dismiss alerts, or claim hardware/installed-host certification.

## Acceptance

Compare a fresh audit and resolved versions with the baseline. Every baseline
alert must have an evidenced disposition. Run crypto regression tests, security
regressions, TypeScript, Jest, production bundles, formatting, and relevant Nix
contracts. Any failed approach or check is reported and discussed before changing
approach, per the user's working agreement.
