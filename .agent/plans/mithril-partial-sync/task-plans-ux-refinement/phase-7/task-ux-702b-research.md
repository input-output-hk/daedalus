# task-ux-702b — Research notes (durable findings from implementation)

> Durable, reusable findings surfaced while implementing task-ux-702b. Plan: `task-ux-702b.md`.
> Decisions: `task-ux-702b-decisions.md`. Implementation log: `task-ux-702b-impl-review.md`.

## 1. MobX strict-mode: observable mutations after `await` must run in an action context
`source/renderer/app/index.tsx:30` configures MobX with `enforceActions: 'observed'`. An `@action`
method only covers its **synchronous** span; any code that resumes **after an `await`** is outside that
span. Mutating an observable there (e.g. `this.isCompletedOverlayDismissed = true` in
`MithrilPartialSyncStore.dismissCompletedOverlay`) throws under strict mode — and if it sits inside a
`try { … } catch`, the throw is **swallowed** by the catch, so the mutation silently never happens (here:
the completed overlay would never hide). Fix pattern: wrap the post-await mutation in
`runInAction('<label>', () => { … })`, mirroring the store's existing post-await actions
(`_updateStatus` / `_applyAvailability`). This was the blocker the CAT-D code-review iteration caught and
resolved. Applies to any store method that mutates observables after an awaited IPC call.

## 2. `toEqual`/`toMatchObject` treats `undefined` and `null` differently — use a `!= null` conditional spread for optional result keys
Jest's `toEqual` (and `toMatchObject`) **ignore** object keys whose value is `undefined` but **do not**
ignore keys whose value is `null`. CAT-H's `getPartialSyncBehindness` adds an optional `certifiedEpoch`,
and `_getCachedCertifiedEpoch()` returns `null` (not `undefined`) when the beacon carries no epoch.
Returning `{ certifiedEpoch: null, … }` unconditionally would break the four pre-existing `toEqual` cases
that don't mention `certifiedEpoch`. The implemented guard is a conditional spread
`...(certifiedEpoch != null ? { certifiedEpoch } : {})`, which omits the key entirely when null/undefined.
Keep the unconditional `behindByImmutables: gap` literal as-is (do not refactor it into a spread).

## 3. Mithril certified-beacon epoch key (#16) — `['beacon','epoch']`, but live-aggregator confirmation is still a VERIFY-ONLY item
`extractCertifiedEpoch` (`mithrilSnapshotMetadata.ts`) extracts the beacon epoch via multi-path/undefined-safe
lookup (`getNestedValue` + `toPositiveInteger`), with `['beacon','epoch']` first (upstream `CardanoDbBeacon`
is snake_case with `required: [epoch, immutable_file_number]`); `cardano_db_beacon.epoch`,
`beacon.epoch_number`, `cardanoDbBeacon.epochNumber`, and a bare top-level `['epoch']` (LAST, so it cannot
shadow the beacon) follow as defense-in-depth. The repo pins a Mithril **fork branch** (`flake.nix:23`) and
no in-repo fixture proves the key against the pinned aggregator, so the early-sync fix degrades to today's
networkTip-only behavior until an operator confirms the live key with
`cardano-db snapshot show latest --json`. Until that operator verify passes, #16's early-sync un-suppression
is silently inert (safe, no regression) rather than live. This remains an open verify-only item, not a build gate.

## 4. `checkDiskSpace` reaches the 30s availability probe via `getManagedChainPath → getConfig`, NOT the `start()` preflight
The per-probe CPU cost behind the availability poll is NOT the obvious `start()` preflight
`_assertSufficientDiskSpace`. It is `getPartialSyncBehindness → getManagedChainPath() →
chainStorageManager.getConfig() → getDefaultStorageConfig() → checkDiskSpace(...)`
(`chainStorageManagerConfig.ts:23`), which forks df/PowerShell and then discards the free-bytes figure
(`getManagedChainPath` only needs `config.customPath`). Grepping `checkDiskSpace` call sites alone misses
this transitive path — this is what the 702a review under-traced for #9. CAT-H's
`_getCachedLocalImmutableNumber()` caches the local read so a cache hit skips both this fork and the
`immutable/` readdir. (Matches the `mithril-availability-probe-cpu-cost` memory note.)

## 5. `prettier:check` does not pass on a pristine checkout of this repo
Under the installed prettier 2.1.2, `yarn prettier:check` flags ~226 files that are untouched by any
recent task (e.g. `source/main/cardano/CardanoNode.ts`, `source/common/ipc/api.ts`, `declaration.d.ts`,
plus 15+ pre-existing mithril files) — the repo is pervasively prettier-non-compliant at HEAD/develop. So
a `prettier:check` FAIL is largely environmental/baseline; to attribute nits to a task, compare HEAD vs
working per-file with `prettier --stdin-filepath <file>` (which bypasses the `.prettierignore` `*.*` / `/*`
rules that otherwise yield a false "clean" on a `.cache` copy). Pair with the Node v24 verify-env caveat
(`mithril-ux-renderer-verify-env` memory: regen `.scss.d.ts` via typed-scss-modules + the gitignored
identity-obj-proxy jest sidecar) when triaging tsc/jest/format failures as regressions.
