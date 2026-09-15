# Planning Brief

## The request

Bring the repository's `prettier` dependency up to parity with the prettier that
`nix fmt` runs, so the two stop producing conflicting output and the check set
stops failing as a result. Then, rather than shipping a branch for a one-line
version bump, take the runtime dependency work that is already waiting and can
be verified now.

## Framing

The formatter problem was reported as CI failing on formatting. The
investigation found the reverse of the obvious reading: the merge gate is clean
and the documented local pre-flight is what fails. `yarn check:all` reports 210
files with style issues on a clean checkout of `master`, and `nix fmt -- --ci`
reports none on the same checkout.

That ordering matters for the fix. The committed tree is already in the shape
the required check wants, so this is a dependency change, not a reformat. Acting
on the local failure by running `yarn prettier:format` is what puts the tree in
a state CI rejects.

Widening to the rest of the dependency work turned up two things worth having in
the same branch. `pbkdf2` and `lodash` are pinned in `resolutions` as well as
`dependencies`, so the advisories against them cannot be cleared by touching the
obvious line. And `source/renderer/app/utils/crypto.ts`, which imports `bip39`,
`blakejs`, `pbkdf2` and `lodash` together to derive mnemonics and seeds, has no
test that would notice if derivation output changed.

## Constraints

- The work lands on a fresh branch cut from `master`.
- No formatting option changes. Any style decision, including the open question
  about `arrowParens`, is separate work and must not ride along with a version
  bump, because a genuine style change and a version artefact become
  indistinguishable in the same diff.
- No reformat of the tree. If the change to `package.json` produces file
  changes anywhere else, something is wrong with the premise and the work stops
  for a re-measure.
- Prettier's scope stays where it is. Markdown, YAML and top-level JSON are not
  formatted today and are not brought into scope here.
- Dependency bumps stop at what the check set can verify. Nothing that needs
  physical hardware, a Storybook visual pass, or a packaging change.
- No dependency is removed. The set that looks unused is investigated and
  written down; acting on it is a separate branch.

## Source material gathered before planning

Formatter:

- `package.json`, `.prettierrc`, `.prettierignore`, `.editorconfig`
- `perSystem/formatter.nix`, `perSystem/checks.nix`, `flake.nix`
- `nix/internal/common.nix`
- `treefmt-nix/programs/prettier.nix` from the pinned flake input
- The generated treefmt config and prettier config from the built formatter

Dependencies:

- Every entry in `dependencies` resolved against the npm registry
- `source/renderer/app/utils/crypto.ts` and its Cucumber coverage in
  `tests/wallets/unit/steps/mnemonics.ts`
- `source/renderer/webpack.config.js` for the fallback and replacement wiring
- `scripts/rebuild-native-modules.sh` and `nix/internal/any-darwin.nix` for the
  native module constraint
- `hardware-wallet-tests/index.ts` for what hardware verification requires

Plan structure:

- `.agent/plans/readme.md`

Measurements are recorded in
[`research/01-measured-divergence.md`](./research/01-measured-divergence.md) and
[`research/02-dependency-inventory.md`](./research/02-dependency-inventory.md),
all taken at `50e6b84b3` on 2026-08-26.
