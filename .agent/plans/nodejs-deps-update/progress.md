# Dependency update progress — 2026-10-20

## Current scope and location

Worktree: `/home/sam/work/iohk/daedalus/nodejs-deps-update`; branch:
`nodejs-deps-update`; base: `22440487d`. Sam authorized committing the restored
work and completed dependency cleanup. Master's recovery backups remain.

Sam confirmed that the first restored Linux build worked, then requested that
all JS dependency advisories be addressed and the NixOS preview package remain
buildable. This pass covers the 294 npm alerts in the original 300-alert baseline.
The six Rust alerts are outside this JS pass; earlier Cargo updates and nine
passing Cargo tests are historical results, not a new Rust audit.

## Advisory evidence

`js-alert-disposition.json` records every original npm alert, its affected range,
all currently locked versions, and its disposition:

- 278 alerts fixed by dependency version updates.
- 12 alerts removed with dependencies, including the unused root WebdriverIO,
  node-forge, asar, and yamljs declarations. Spectron's own E2E dependency remains.
- Four original alerts addressed by tested source patches.
- Zero original npm alerts without a disposition.

`js-audit-final.json` retains the latest fresh audit's five distinct GitHub
advisory IDs. These remain visible to version-based scanners because patched
upstream releases do not exist; no audit ignore list or alert dismissal was used.
Do not describe the registry audit as clean or the GitHub alerts as closed.

| Advisory | Patch | Regression evidence |
|---|---|---|
| GHSA-848j-6mx2-7j84 | elliptic 6.6.1: preserve generated nonce byte length during RFC 6979 bits2int truncation | Two reporter-provided P-521 SHA-512 known-answer vectors that fail unpatched elliptic |
| GHSA-vfj7-8cjw-p6xm | braces 3.0.3: bound parser and AST-walker depth | Deep input strings and supplied ASTs; normal expansions retained |
| GHSA-jmr9-qjv8-65gv | extract-zip 2.0.1: contain symlink targets and existing parent chains | Outside and dangling symlinks rejected; valid internal forward links retained |
| GHSA-7pqw-9j4j-h8q3 | extract-zip 2.0.1: reject final-component symlink overwrites and use O_NOFOLLOW where available | Duplicate ZIP names and pre-existing symlinks cannot overwrite target files |
| GHSA-hp3w-g68c-fv3c | sprintf-js 1.1.3: bound e/f/g precision to supported ECMAScript limits | Excessive and zero precision do not cause RangeError; normal formatting retained |

The braces advisory was found by the fresh audit in addition to the original
baseline. Tests are in `tests/common/unit/dependency-security.spec.ts`: 18 passed.
The ZIP patch also preserves the callback API used by legacy Puppeteer callers.
A cucumber-html-reporter patch adapts its old UUID subpath import to UUID 11.

## Implemented compatibility work

Compatible transitive releases were refreshed. Gulp, Nodemon, Stylelint and its
SCSS parser, HTML loaders, Webpack, Faker, Recharts, UUID, and TypeScript were
updated. Faker callers use the current API. Recharts' reference-line customization
uses its public shape API. React type versions are aligned with React 16.
TypeScript compatibility changes preserve table rendering and existing status-map
alias overwrite order. The ESLint parser/plugin support TypeScript 5; unused
variables retain their original warning severity.

Electron remains 42.11.14 with matching platform archives, headers, and hashes;
Nixpkgs remains the selected 26.05 revision and Node 24 is retained. The approved
Moretea helper stays separately pinned. Build setup exposes the selected Node's
node-gyp and applies patch-package after patchShebangs. NAN is locked to 2.29.0.
The runtime-copy list has 473 entries and includes the external-package closure.

The expanded refresh exposed USB's C++14 flags against node-addon-api 8.9.2.
`patches/usb+2.15.0.patch` selects C++17 for Linux, Darwin, and MSVC before initial
native-module installation, instead of relying only on the later Electron script.
The renderer and Storybook also explicitly resolve querystring-es3 for browser
query serialization, which had previously depended on an implicit transitive
package. Representative wallet query cases match Node's querystring behavior.

## Checks

- TypeScript (`tsc --noEmit`): passed.
- Jest: 91 suites, 1,281 tests and six snapshots passed; three tests skipped.
  Use `--no-cache` after applying dependency patches to avoid stale Jest output.
- Storybook production build: passed. Webpack is aligned at 5.112.0.
- ESLint: passed with existing warnings; no errors.
- Stylelint with explicit SCSS syntax: passed.
- Translation extraction/check: passed, with no tracked translation changes.
- Formatting of changed source, test, manifest, and Nix files: passed.
- Fresh audit: five source-patched advisory IDs; zero critical advisories.
- Production renderer build: passed with the explicit querystring polyfill.
- Final `nix build .#nixos-package-preview -L`: passed with the USB and querystring fixes.
  Log: `/tmp/daedalus-js-nixos-final-4.log`.
  Output: `/nix/store/id63yr2pfa9b1dcqj06y95i99ch9zfcp-daedalus-nixos-preview`.
  This was the original JS-cleanup output; the Hydra follow-up below supersedes it.
- Packaged Electron smoke check: js-chain-libs-node, usb, node-hid,
  @trezor/connect, and pdfkit all loaded. All 473 runtime-copy entries exist.
  Both ECDSA regression vectors also passed using the packaged elliptic code.

The earlier output `/nix/store/1a2prdiibkjzsrhayxr9459a437dpx18-daedalus-nixos-preview`
predates the expanded JS cleanup and is superseded by the output above.

## Validation boundaries and restart

### Hydra follow-up

Hydra on `c5b70752d` exposed build paths the original NixOS preview build did
not exercise. Local Nix builds reproduced bundle-integrity, docs, i18n, and
Debian packaging failures. The Windows dependency failure was traced through
Hydra build 15227011 to msitools' failing upstream tests.

- Portable Electron: replace the actual absolute SQLite DT_NEEDED entry in
  libtinysparql rather than hard-coding a Nixpkgs SQLite version. Both the
  bundle-integrity self-test and real bundle check now pass.
- Translation tooling: preserve FormatJS N-API bindings during Linux and
  Darwin native-module cleanup. Correct two pre-existing optional-before-required
  parameter signatures that its parser rejected. The custom formatter now
  rejects empty extraction output. `--throws` was evaluated but is unsuitable
  for the existing duplicate-ID descriptions; normal extraction retains those
  warnings. The sandboxed Nix i18n check passes with all 1,634 messages across
  221 groups and no tracked translation changes.
- Docs: align the Electron version in AGENTS.md and .agent/readme.md. The Nix
  docs check passes over 65 files.
- Windows SDK tools: libgsf 1.14.59 rejected zero-length MSI streams because
  its zero-byte allocation returns NULL. Scope a nonzero backing allocation
  to the msitools dependency used for SDK extraction, retaining logical stream
  sizes and the complete msitools test suite. That suite passes; libgsf's own
  check phase completes with its existing 14 skipped tests. SDK extraction now
  succeeds. Fix the subsequent symlink cleanup to tolerate no matching links
  and preserve filenames with spaces. `nativeModules-x86_64-windows` passes:
  USB and HID outputs are verified as Windows x86-64 PE DLLs and archived.
- Windows app packaging: rcedit defaults to the removed `wine64` executable.
  Set its supported WINE_BINARY override to the Nix Wine package's `bin/wine`.
  All four Windows installer cross-builds then pass through complete NSIS
  assembly. These are unsigned installers; no signing or publication occurred.
- Fresh TypeScript, Jest (1,281 passed, three skipped), changed-file ESLint,
  formatter smoke checks, and treefmt checks pass.
- All four Debian installer variants (mainnet, mainnet-flight, preprod, preview)
  and `checks.x86_64-linux.linux-deb-package-contract` pass.
- `nix build .#nixos-package-preview` passes again. `result` now points to
  `/nix/store/i4imkkq7cwgnkr4c1ah87v61hvham11l-daedalus-nixos-preview`.

Validation logs are in `/tmp/daedalus-hydra-jschecks-final.log`,
`/tmp/daedalus-msitools-fixed-2.log`, `/tmp/daedalus-hydra-windows-fixed-2.log`,
`/tmp/daedalus-hydra-packages-final.log`, and
`/tmp/daedalus-hydra-wininstallers-final-4.log`. All final commands exit zero.
The installer run uses both NixOS and IOG caches; the IOG cache briefly had
connection errors. Using only the NixOS cache would have rebuilt the cached
Haskell backend closure, so that run was stopped and both caches restored.

No successful remote CI rerun, native macOS/Windows host execution, wallet launch,
or hardware-device certification is claimed. Sam assigned those checks to existing CI and team
manual testing. No push, PR, or alert dismissal has been performed.
The separate Electron hardening work still retains unsafe-eval pending assessment
of dynamic-code sites and has not enabled context isolation over the Node preload.

The current captured Nix development environment is
`/tmp/daedalus-nodejs-deps-dev-env-ready.sh` (temporary, excludes shellHook).
Jest needs the selected systemd library directory on LD_LIBRARY_PATH for libudev.
Use Nix to recreate the environment if those temporary files disappear.
Installs with ignored scripts must be followed by `yarn patch-package --error-on-fail`;
Nix sandboxes must fix executable shebangs first. Do not modify node_modules during
builds/tests. Run formatting before committing.

## Rebase compatibility follow-up — 2026-10-21

Rebased onto master `14d6c330c`, preserving its Ledger 8 integration, trusted
renderer navigation, IPC validation, and hardware service refactor. Master's
policies supersede the earlier window-security helpers in this branch.

Declare Ajv 6.15.0 directly for the existing schema API rather than depending on
transitive hoisting. Narrow numeric inputs and the JSON request body explicitly
for TypeScript 5. Live Trezor provenance checks follow the current manifest and
lockfile, verify installed source bytes against the integrity-checked tarball,
and retain the original archived evidence unchanged. Include master's new
`ipaddr.js` dependency in the packaged runtime list.

Validation: TypeScript passes; all 207 Jest suites pass (2,554 tests, three
skipped, six snapshots); the final provenance test rerun passes all 16 tests.
Changed-file ESLint passes with existing warnings, and formatting passes.
The NixOS preview derivation evaluates successfully; no full installer rebuild
or physical hardware certification was performed for this rebase.
