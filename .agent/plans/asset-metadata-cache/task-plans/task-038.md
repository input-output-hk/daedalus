## Task ID and Title

`task-038` — Manual QA across the three source options.

## Why Chosen Now

Everything phase 7 builds is in place and every automated check is green. What
no automated check reaches is a live instance, a real chain database, the
process boundary, and what a person sees when a token they have just acquired
has no name yet. CI runs the suites on Linux and aarch64-darwin and the static
checks on `x86_64-linux` alone, so no job executes on Windows at all, and no job
anywhere has a chain.

## Interaction Mode

`manual_execution`.

This environment is a WSL2 machine with no display, no macOS, no Windows, no
mainnet wallet and no funded selfnode. **No step below has been executed.** The
deliverable is the procedure, the evidence each step must produce, and a
checklist an operator signs.

One thing here **was** exercised, and it is recorded so nobody re-runs it
looking for it: the chain confirmation was run once against the real 17 GiB
preprod immutable database on the development machine with a live `tx_cbor`
response, on 2026-09-16, and confirmed the pointer. That is one platform, one
network, one asset, and outside the application.

## Scope

Eight scenarios across the settings surface and the chain channel, on Linux,
macOS and Windows, plus two that need one platform only.

## Non-Goals

- No screenshots as the primary evidence. Four of these scenarios are about what
  does **not** happen, and a screenshot is weak evidence for an absence.
- No new automation. The e2e suite cannot execute: `spectron@14` resolves
  `electron-chromedriver@12` against Electron 41.3.0.
- No source change. A scenario that fails opens a defect; it does not get fixed
  inside this task.
- Not phases 1 to 6. `task-027` covers those and is separately blocked.

## Dependencies

`task-032`, `task-036`, `task-037`.

## Research Consulted

- `asset-metadata-cache-prd.md:1482-1517`, the Manual QA list and the platform
  note.
- `asset-metadata-cache-prd.md:1560-1580`, open question 1, whose user-visible
  consequence scenario 6 is the only check of.

## Docs, Workflows, and Skills Consulted

- `task-027`'s procedure, as the format: a pass condition per step that a person
  who did not write it can apply, and an artifact per step.

## Live Repo Findings Verified For Planning

1. **The three preset URLs, per network.**
   `nix/internal/launcher-config.nix` assigns `koiosUrl` for `mainnet`,
   `mainnet_flight`, `preprod` and `preview`:
   `https://api.koios.rest/api/v1`, `https://preprod.koios.rest/api/v1` and
   `https://preview.koios.rest/api/v1`. **Selfnode has none.** The preset then
   has no URL, so nothing matches it and the settings page resolves the
   selection to "Custom index" with an empty address field. Checked by running
   `getAssetMetadataSourceIdFromUrl('')`, which answers `custom`. The chain
   channel is inert there. That is scenario 10, not a defect.
2. **The settings route is `/settings/asset-metadata`** and the menu item is
   labeled "Tokens", between Wallets and Stake Pools.
3. **The chain database the confirmation reads is
   `<stateDir>/chain/immutable`**, or `<custom>/chain/immutable` when the user
   has moved it. It is resolved once, when the IPC handlers are built
   (`source/main/ipc/index.ts`), so moving the chain takes effect on the next
   start.
4. **A pointer newer than the immutable database resolves late by design.** The
   window is the last k blocks, about twelve hours on mainnet, and the retry is
   one hour.
5. **A wallet with no chain database gets no chain rows and no errors.** The
   reader answers `unreadable` and the resolver writes nothing, which is
   scenario 8.
6. **The refusals a user can see are two and they read differently**:
   "This URL did not answer as a metadata source" and "This metadata source is
   too far behind your node to be used".
7. **A stale instance is refusable but hard to arrange.** The bound is 43,200
   slots, about twelve hours, and no public instance is that far behind. The
   scenario needs a local stand-in that answers `/tip` with an old slot, so it is
   written as a scripted step rather than as a hope.

## Files Expected To Change

- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-prd.md`, the status log
- the three review-log files for this task.

No source file changes.

## Implementation Approach

The procedure below is the deliverable. An operator runs it, fills in the
checklist, and attaches the artifacts each step names.

### Preparation, once per platform

Install the build under test on a profile whose asset metadata cache has been
deleted, so the first launch is a cold cache:

| Platform | Delete |
|---|---|
| Linux | `~/.local/share/Daedalus/<network>/asset-metadata-cache/` |
| macOS | `~/Library/Application Support/<spacedName>/asset-metadata-cache/` |
| Windows | `%APPDATA%\<spacedName>\asset-metadata-cache\` |

Let the node sync fully before starting, except in scenario 9. The log to read
evidence from is `<stateDir>/Logs/pub/Daedalus.json`, which is written at
`debug`.

A local stand-in instance is needed twice, in scenarios 5 and 7. Any HTTP server
that answers `GET /tip` and `POST /asset_info` will do; both scenarios say what
it must answer.

### 1. The preset is selected on a profile that has never chosen

Open Settings, then Tokens.

**Pass:** the selection reads "Koios (recommended)", no address field is shown,
and the description below names Koios. **Not on selfnode**, which has no
instance and behaves differently; that is scenario 10.

**Evidence:** a screenshot of the page.

### 2. The unavailable option is visible and cannot be chosen

Open the selection.

**Pass:** three options are listed and the third reads "From my own chain data
(not available yet)". Clicking it changes nothing: the selection stays where it
was and no address field appears.

**Evidence:** a screenshot of the open selection, and a second of the page after
clicking the third option, showing the selection unchanged.

### 3. A custom address that answers is accepted, without a restart

Choose "Custom index", type the preset URL for this network into the address
field, and submit.

**Pass:** the field accepts it, no error appears, and **without restarting**,
opening a wallet holding an NFT resolves names as before. Re-opening Settings
shows the selection back on "Koios (recommended)", because the stored URL is the
preset's.

**Evidence:** the settings page after submitting, and the token list afterward.

### 4. A custom address that does not answer is refused

Choose "Custom index" and submit `https://example.invalid/api/v1`.

**Pass:** the message "This URL did not answer as a metadata source" appears
under the field, and the stored selection does not change: leaving Settings and
returning shows the previous selection.

**Evidence:** a screenshot of the error, and a second of the page after
returning, showing the old selection.

### 5. An address that is behind is refused differently

Point the setting at a local stand-in answering
`GET /tip` with `[{"abs_slot": <the node's current slot minus 100000>}]`.

**Pass:** the message reads "This metadata source is too far behind your node to
be used", which is **not** the message from scenario 4.

**Evidence:** a screenshot of the error, and the stand-in's access log showing
one request to `/tip`.

### 6. An NFT the registry has never heard of

With the preset selected, open a wallet holding an NFT that carries a CIP-25
record and is not in the token registry. Watch the token list from a cold cache.

**Pass, in this order:** the row first shows its fingerprint and, if its asset
name bytes are printable, a decoded name marked as minter-chosen; then, without
a restart, it shows the CIP-25 name and that marking is gone. Its amount is in
raw units throughout, before and after, and the settings dialog for it offers
decimal places rather than applying any.

**Evidence:** two screenshots of the same row, before and after, and the
`asset_metadata` row from the cache, which must show `source` `chain`,
`decimals` NULL and `verified` 0:

```
sqlite3 <cache>/assets.sqlite \
  "select subject, source, decimals, verified, slot from asset_metadata where source='chain';"
```

### 7. An index that lies is refused and writes nothing

Point the setting at a local stand-in that answers `GET /tip` with the node's
current slot, and `POST /asset_info` with a real subject the wallet holds and a
`minting_tx_hash` of a transaction that is not the one that minted it. Any other
real transaction hash will do.

**Pass:** no row appears for that subject. The token keeps its fingerprint, the
log carries `Asset metadata: pointer refused by the local check`, and the
`asset_metadata` table has no row with that subject and `source` `chain`.

**Evidence:** the log line, and the query from scenario 6 showing no such row.

**This is the scenario worth doing by hand.** It is the one that proves the index
is untrusted by construction, and nothing else in this plan checks it end to end.

### 8. Offline, with a populated cache

Complete scenario 6, then disconnect the network and restart.

**Pass:** the token list shows the same names it showed online. No error dialog,
no toast, and nothing in the interface refers to a metadata source. The
`Daedalus.json` log may carry `Koios: batch abandoned`, which is the correct
record of an offline attempt.

**Evidence:** a screenshot of the token list offline, and the absence of any
dialog.

### 9. A freshly minted NFT resolves late

On preprod or preview, mint an NFT with a CIP-25 record, or acquire one minted
within the last hour, and open the wallet holding it.

**Pass:** the row shows its fingerprint and its decoded name, and **no** CIP-25
name. Within the hour after the block passes out of the volatile window, and
without a restart, the CIP-25 name appears. The `asset_resolution` row for that
subject reads `pending` in the meantime:

```
sqlite3 <cache>/assets.sqlite \
  "select subject, state, datetime(retry_after/1000,'unixepoch') from asset_resolution;"
```

**Pass condition on the wait:** `retry_after` is about an hour ahead of the last
attempt, which is the retry interval, and in no case more than twelve hours
ahead, which is the window it has to fall inside. The scenario is not a failure
while the name is missing; it is a failure if the name never arrives after the
block settles, or if `retry_after` is beyond twelve hours, which would mean the
subject has fallen into the failure backoff rather than the pending one.

**One platform is enough.** This is chain timing, not platform behavior.

### 10. Selfnode has no instance and says nothing about it

Start a selfnode cluster and open Settings, then Tokens.

**Pass:** the page renders and the selection reads **"Custom index"** with an
empty address field. That is correct rather than a defect: the launcher
configures no instance for selfnode, so the preset has no URL and nothing can
match it. Nothing is stored and no request is issued to any host, which the log
confirms by carrying no `Koios:` line at all.

Typing an address here is accepted if it answers, which is the only way to use
the channel on selfnode and is not part of this scenario.

**One platform is enough.**

## Acceptance Criteria

1. Scenarios 1 to 8 are executed on Linux, macOS and Windows.
2. Scenarios 9 and 10 are executed once, on any platform, and the platform is
   recorded.
3. The checklist below is completed and signed, with the build identifier and
   the date, and the evidence each step names attached.
4. Any failure is opened as a defect with its scenario number, before the
   checklist is signed.

## Verification Plan

The checklist is the verification. It is filled in by the operator and is what
makes the claim checkable afterward by someone who was not there.

```
Asset metadata source and chain channel — manual QA
Build:            ____________________   Date: ____________
Operator:         ____________________
Network:          ____________________

                                        Linux    macOS    Windows
1  Preset selected on a fresh profile    [  ]     [  ]     [  ]
2  Unavailable option visible, inert     [  ]     [  ]     [  ]
3  Custom address accepted, no restart   [  ]     [  ]     [  ]
4  Unreachable address refused           [  ]     [  ]     [  ]
5  Behind address refused, differently   [  ]     [  ]     [  ]
6  NFT: fingerprint, then CIP-25 name    [  ]     [  ]     [  ]
7  Lying index writes no row             [  ]     [  ]     [  ]
8  Offline with a populated cache        [  ]     [  ]     [  ]

9  Fresh mint resolves late              [  ]  (once, platform: __________ )
10 Selfnode has no instance              [  ]  (once, platform: __________ )

Chain row observed in scenario 6
  subject   ______________________________________________
  source    ____________  decimals ________  verified ________

Defects opened (scenario number and issue reference)
  ____________________________________________________________

Signed: ____________________
```

## Risks and Open Questions

- **This task cannot be completed by an agent and must not be recorded as
  passing.** Its status in the task graph is `blocked`, with the reason stated,
  until an operator returns a signed checklist.
- **Scenario 9 needs a mint and a wait.** It is the longest step and the one most
  likely to be skipped, and it is the only check of the behavior the PRD's open
  question 1 accepts as the common case for NFTs.
- **Scenarios 5 and 7 need a local stand-in instance.** That is a few lines of
  any HTTP server, and both scenarios say exactly what it must answer, but it is
  a developer setup rather than a QA one.
- **Scenario 7 needs a real subject and a wrong transaction hash.** Getting the
  stand-in to answer plausibly for a subject the wallet actually holds is the
  fiddly part; the pass condition does not depend on which wrong hash is used.
- Nothing here needs a decision from the project owner beyond assigning an
  operator.

## Required Docs, Research, and Tracking Updates

- `asset-metadata-cache-tasks.json`: `task-038.status` to `blocked` with the
  reason, and its implementation notes extended with the two scenarios the task
  did not name.
- The PRD's status log gains an entry for phase 7, recording what was built and
  the four corrections this phase made to the document: the endpoint ordering,
  the description of `checkSmashServerHealth`, the CBOR decoder settlement that
  closes open question 2, and the source of the policy script for closure.

## Review-Log Paths

- `.agent/plans/asset-metadata-cache/task-plans/task-038-plan-review.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-038-impl-review.md`

## Planning Status

approved

## Build Status

blocked — the procedure is complete and no step has been executed. An operator
with Linux, macOS and Windows returns the signed checklist.

## Current Outcome

A procedure a person can follow, with a pass condition for each step that does
not depend on having written it.

## Final Outcome

Pending an operator.

## Self-Review

The temptation in a task like this is to run what can be run here and leave the
rest implied. One thing was run here, once, outside the application, and it is
recorded as exactly that rather than folded into a scenario. The scenario worth
defending is the seventh: it is not on the PRD's list, it is the only end-to-end
check that the index is untrusted, and every other scenario would pass against a
build that believed whatever the index said.
