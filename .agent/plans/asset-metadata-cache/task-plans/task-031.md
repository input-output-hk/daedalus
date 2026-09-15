## Task ID and Title

`task-031` — Probe a candidate URL against `/tip`, map a stored URL back to its
preset, and persist the selection.

## Why Chosen Now

`task-030` declared the presets and the validator. A pattern match is not a
check: a URL that matches the pattern and answers nothing is a channel that
silently returns no rows, and there is no surface on which that looks different
from an asset the index has never heard of. The probe is what makes a
user-supplied URL safe to store, and `task-032` renders the selection this task
holds.

## Interaction Mode

`agent_execution`.

## Scope

The live probe, the mapping from a stored URL back to a preset id, the per
profile persistence, and the store state the settings page will bind to.

## Non-Goals

- No settings surface. `task-032` renders it; this task is everything behind it.
- No client and no request composed from the selection. `task-033` does that,
  and it is also where the selected URL crosses into the main process.
- No change to the registry endpoint. Settled below rather than left implied.
- No `getAssetMetadataSourceNameFromUrl`. `getSmashServerNameFromUrl` exists at
  `utils/staking.ts:8-16` and has one caller; the asset settings page renders
  the preset name from the id, so a second reduction would have no reader.

## Dependencies

`task-030`.

## Research Consulted

- `asset-metadata-cache-prd.md:846-876`, the health probe, the preset mapping
  and the persistence.
- `asset-metadata-cache-prd.md:655-666`, the registry endpoint resolution order,
  which is the sentence settled below.
- `research/01-koios-pointer-option.md`, for `/tip` and its response shape.

## Docs, Workflows, and Skills Consulted

- `.agent/skills/i18n-messaging/SKILL.md`, for the two new error messages.
- `.agent/system/state-management.md` is **not** relied on for the store shape.
  The `Store` constructor signature it gives does not exist in this repository,
  and `setupStores` is really `setUpStores` with four parameters.
  `stores/StakingStore.ts` is read instead.

## Live Repo Findings Verified For Planning

1. **`checkSmashServerHealth` is not a direct probe, and the PRD says it is.**
   `api/staking/requests/checkSmashServerHealth.ts` calls `request` with
   `path: '/v2/smash/health'` and `{ url }` as the **second** argument, which
   `api/utils/request.ts:87-89` turns into a query string. It is a request to
   **cardano-wallet**, at its own hostname and port and with its own client
   certificate, asking cardano-wallet to check the SMASH server. The PRD at
   `:846-849` describes it as issuing the request against the candidate URL.
   There is no cardano-wallet endpoint that will probe a Koios instance, so "the
   same six lines" is not available and the asset probe issues its own request.
2. **Reusing `request` for a third-party host would be wrong in two ways.**
   `api/utils/request.ts:20` reads `isSelfnode` once at module load and `:129`
   sends the whole request over **plain HTTP** when it is true, and `:81` merges
   in an agent carrying the wallet's TLS options. A custom source URL on
   selfnode would go out unencrypted, and a probe has no business presenting the
   wallet's client certificate to a third party. The probe is its own small
   `global.https.request`, and the validator has already guaranteed `https://`.
3. **`global.https` is what the renderer has.** `source/main/preload.ts:17-22`
   exposes `Agent` and `request`. There is no `fetch` polyfill in the renderer
   and `api/utils/request.ts` is the only HTTP caller in `source/renderer`.
4. **Koios `/tip` answers a one-element array.** Confirmed live on 2026-09-14
   against `https://preprod.koios.rest/api/v1`: `GET /tip` returns `200` and a
   JSON array whose element carries `abs_slot`, `block_no`, `block_time`,
   `epoch_no`, `epoch_slot` and `hash`. A bare `200` with any other body is not
   an instance.
5. **The local tip is on the network status store.**
   `stores/NetworkStatusStore.ts:76-77` is
   `@observable localTip: TipInfo | null | undefined`, and `TipInfo` carries
   `absoluteSlotNumber` (`api/network/types.ts:4`). It is null until the first
   network status arrives, which is the case the staleness rule has to handle.
6. **`checkSmashServerIsValid` is the shape to mirror, including its short
   circuit.** `api/api.ts:2169-2194`: `:2177-2179` returns true for
   `SMASH_SERVERS_LIST.direct.url` with no request, and every other URL must
   answer. It returns a boolean and throws `ApiError` when the request itself
   fails; the caller at `:2205-2213` turns `false` into
   `ApiError({ code: 'invalid_smash_server' })`.
7. **An `ApiError` code needs three edits.** `domains/ApiError.ts:49` lists the
   codes, `api/errors.ts:106-110` holds the message, and `ApiError`'s
   constructor camel-cases the code to look the message up. So a new code is a
   union member, a `defineMessages` entry and a regenerated locale artifact.
8. **The persistence trio is three lines and two constants.**
   `api/utils/localStorage.ts:328-333` for the trio,
   `common/config/electron-store.config.ts:29` for the key and
   `common/types/electron-store.types.ts:20` for its type.
9. **`StakingStore` writes twice on start and this store writes once.**
   `stores/StakingStore.ts:199-224` reads the wallet's setting, reads local
   storage, and pushes a default back to the wallet when the two disagree. There
   is no server-side counterpart for the metadata source, so the stored value is
   the only one.
10. **`AssetsStore` already has a `setup()` that starts two reads.**
    `stores/AssetsStore.ts:106-108` runs `_setUpFavorites` and
    `_setUpLocalDecimals`, both `async` and neither awaited, and
    `_setUpLocalDecimals` wraps its post-await assignment in `runInAction`.
    `configure({ enforceActions: 'observed' })` is live, so that is a
    requirement rather than a habit.
11. **`AssetsStore.spec.ts` exists and builds the store against fakes.** It is
    the file the new cases go in.

## The endpoint ordering, settled

The PRD at `:664` resolves the registry fetcher's endpoint as
"`launcherConfig.metadataUrl`, then the configured source setting, then the mock
when `network === 'selfnode'`, then the mainnet literal". As the task graph
notes, `metadataUrl` is present on every network except selfnode
(`nix/internal/launcher-config.nix:448-450`), so a source setting sitting second
in that order could never apply.

**Settled: the metadata source setting does not feed the registry fetcher at
all, and `assetRegistryEndpoint` is unchanged.** The two are different endpoints
speaking different protocols. The registry answers `POST /metadata/query` with
signed properties; a Koios instance answers `POST /asset_info` and `POST
/tx_cbor` with an index of the chain. Pointing one at the other produces 404s,
not a fallback. The PRD sentence is the "source" collision its own open question
6 records: the schema column `source`, the user-facing setting, and the registry
endpoint are three things sharing one word.

So the setting is not decorative, because it is the only thing that supplies the
pointer channel's base URL, which `task-033` reads and which has no launcher
default of its own beyond the preset. The ordering that stands is:

| Endpoint | Order |
|---|---|
| Registry | explicit override, then `launcherConfig.metadataUrl`, then the selfnode mock, then `https://tokens.cardano.org` |
| Pointer | the stored selection, then the `koios` preset for this network, and nothing when neither exists |

The explicit override on the registry side stays. It is
`AssetMetadataChannelOptions.endpoint`, it is supplied by specs and by nothing
else, and removing it would take the registry client's tests offline.

## Files Expected To Change

- `source/renderer/app/api/assets/requests/checkAssetMetadataSourceHealth.ts` (new)
- `source/renderer/app/api/assets/requests/checkAssetMetadataSourceHealth.spec.ts` (new)
- `source/renderer/app/api/assets/types.ts`
- `source/renderer/app/api/api.ts`
- `source/renderer/app/api/errors.ts`
- `source/renderer/app/domains/ApiError.ts`
- `source/renderer/app/config/assetsConfig.ts`
- `source/renderer/app/utils/assets.ts`
- `source/renderer/app/utils/assets.spec.ts`
- `source/renderer/app/api/utils/localStorage.ts`
- `source/common/config/electron-store.config.ts`
- `source/common/types/electron-store.types.ts`
- `source/renderer/app/actions/assets-actions.ts`
- `source/renderer/app/stores/AssetsStore.ts`
- `source/renderer/app/stores/AssetsStore.spec.ts`
- the four translation artifacts, regenerated
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`
- the three review-log files for this task.

## Implementation Approach

**The probe is its own request, for the reasons in findings 1 and 2.**
`checkAssetMetadataSourceHealth(url)` composes `{url}/tip` with the trailing
slash normalised away, issues a `GET` through `global.https.request`, caps the
response, applies one timeout to the whole call, and resolves the parsed tip or
rejects. It parses rather than trusting the status: a `200` whose body is not an
array carrying a numeric `abs_slot` is not an instance, which is what catches a
URL that answers everything with a login page.

**The acceptance decision is three-valued, not two.** `checkSmashServerIsValid`
returns a boolean because there is one way to fail. Here there are two that mean
different things to the user, and both are worth saying: the URL does not answer
as an instance, and the instance is behind. `AdaApi.checkAssetMetadataSourceIsValid`
returns `{ valid: true }` or `{ valid: false, reason: 'unreachable' | 'stale' }`,
and the store turns the reason into one of two `ApiError` codes. `direct`
returns valid with no request, exactly as `api.ts:2177-2179` does for SMASH.

**Staleness is measured against the user's own node, and only downwards.** An
instance whose `abs_slot` is more than `ASSET_METADATA_SOURCE_MAX_TIP_LAG_SLOTS`
below `networkStatus.localTip.absoluteSlotNumber` is refused. The constant is
43,200, which is twelve hours at one slot per second, chosen to be the same
order as the volatile window `task-034`'s local confirmation cannot see into: an
index further behind than that cannot answer for anything the local check could
confirm anyway. An instance **ahead** of the local tip is not refused, because a
node that is still syncing is behind everything. When `localTip` is null the
comparison is skipped and a well-formed answer is accepted, because refusing
during sync would make the setting unusable for exactly as long as a first sync
takes.

**`getAssetMetadataSourceIdFromUrl` is the same reduce as
`getSmashServerIdFromUrl`,** over `ASSET_METADATA_SERVERS_LIST` and falling back
to `ASSET_METADATA_SOURCE_TYPES.CUSTOM`. It carries no `@ts-ignore` where the
SMASH one does, because the fallback is typed and the list's keys are typed by
`task-030`.

**Persistence and startup.** One key, `ASSET-METADATA-SOURCE`, and the trio
beside `getSmashServer`. `AssetsStore.setup()` starts a third read:
the stored value, falling back to the `koios` preset URL for this network, and
`null` when there is no preset either, which is selfnode. The assignment is
inside `runInAction`.

**Selecting.** `_selectAssetMetadataSourceUrl` mirrors
`StakingStore._selectSmashServerUrl` minus the parts that exist only because
SMASH has a server-side setting: no request reset, no fetch tracker. Probe,
then store, then write, then a settings analytics event. The error path sets the
error observable so the input can render it.

## Acceptance Criteria

1. `getAssetMetadataSourceIdFromUrl` returns `koios` for the default URL,
   `direct` for the literal, and `custom` for an unrelated URL and for the empty
   string.
2. The `direct` option is accepted with no request issued, asserted on the
   absence of a call rather than on the return value alone.
3. A URL that does not answer `/tip`, and a URL that answers `200` with a body
   that is not a tip, are both refused and the reason reaches the caller.
4. A URL whose tip is more than the lag bound behind the local tip is refused as
   stale; one within it is accepted; one ahead of the local tip is accepted; and
   one probed while `localTip` is null is accepted.
5. A selection survives a restart: a store reading what a previous store wrote
   comes up with that URL selected.
6. A profile with nothing stored comes up with the preset for its network.
7. `compile`, `lint`, `stylelint`, `jest`, `i18n` and `cucumber-unit` pass from
   `nix build`.
8. No new `@ts-ignore` and no new `@ts-expect-error`; `package.json` and
   `yarn.lock` unchanged.

## Verification Plan

- The probe spec replaces `global.https` with a fake whose `request` returns a
  scripted response, so the cases drive the parsing and the timeout without a
  socket. Cases: a well-formed tip, a `200` carrying an object rather than an
  array, a `200` carrying an array whose element has no `abs_slot`, a `503`, a
  socket error, a timeout, and a body over the cap.
- Criterion 2 is asserted by counting calls on the fake: selecting `direct`
  must leave the call count at zero. A test that only asserted the return value
  would pass against an implementation that probed and ignored the answer.
- Criterion 4 is four cases against the same fake with the local tip varied,
  because the interesting part is the comparison and not the transport. The
  boundary is driven at exactly the bound and one slot beyond it.
- Criteria 5 and 6 are driven on the store against a fake local storage: write
  through one store, build a second, and assert what it comes up with; then the
  same with nothing stored.
- `utils/assets.spec.ts` gains the mapping cases. The default URL has to be
  reachable there, and under jsdom `global.koiosUrl` is undefined, so the spec
  sets it and loads the config module through `jest.isolateModules` before
  reducing over it. That is the mechanism `task-030`'s finding 10 recorded and
  its own spec did not need.
- All six Nix checks. `i18n` is a result rather than a guard: two messages are
  added, so the regenerated artifacts belong in this commit.

## Risks and Open Questions

- **The probe is a network call from the renderer to a third party.** It happens
  only when the user submits a URL in the settings page, it carries no wallet
  identifier, and it is the same disclosure the channel itself makes once
  selected. Named because it is the first request this branch makes from the
  renderer to anything but cardano-wallet.
- **The staleness bound is chosen, not measured.** Nothing says twelve hours is
  the right lag to refuse at. It is argued from the volatile window rather than
  from data about how far Koios instances actually drift.
- **A stored URL is not re-probed on start.** An instance that was healthy when
  selected and is dead now comes up selected and answers nothing. Re-probing
  every start would put a third-party request on every launch, which is a worse
  trade for a channel whose failure mode is already "no rows". Named, not fixed.
- Nothing here needs a decision from the project owner.

## Required Docs, Research, and Tracking Updates

- `asset-metadata-cache-tasks.json`: `task-031.targetPaths` widened to the error
  code, the storage key, the actions and the two specs; the endpoint-ordering
  settlement recorded in its implementation notes; `task-031.status` to
  `completed`.
- The PRD's `:664` sentence and its `:846-849` description of
  `checkSmashServerHealth` are both wrong. Both are recorded in the PRD's Status
  Log at the end of the phase, in the append-only form the plan readme requires,
  rather than edited into the body.

## Review-Log Paths

- `.agent/plans/asset-metadata-cache/task-plans/task-031-plan-review.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-031-impl-review.md`

## Planning Status

approved

## Build Status

completed

## Current Outcome

A URL is stored only after the instance behind it has answered, and it comes
back selected on the next start.

## Final Outcome

Complete.

## Self-Review

The plan the task graph describes is six lines copied from SMASH. Reading the
SMASH request found that it is not a probe at all: it asks cardano-wallet to
check the server, over the wallet's own connection, with the candidate URL as a
query parameter. Copying its shape without reading it would have produced a
request to cardano-wallet for an endpoint it does not serve, and the failure
would have looked like an unreachable Koios instance.
