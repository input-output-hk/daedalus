# Task task-010: Resolver: sequence cache read, fetch, verify and write

## Task ID and Title

- ID: `task-010`
- Title: `Resolver: sequence cache read, fetch, verify and write`

## Why Chosen Now

All four of this task's dependencies are complete: `task-005` at `1df195e96`, `task-006` at
`084854a71`, `task-007` at `49a0ed3b3` and `task-009` at `41f98a5d6`. It is the module that turns
four independent pieces into a cache, and phase 3 has nothing to call until it exists.

It is also the module that decides what `verified` means in the one column the schema has, which is
the last interpretive question in the phase and the one everything in phase 4 turns on.

## Interaction Mode

- Mode: `agent_execution`

A stubbed transport and a temporary database file cover every acceptance criterion.

## Scope

- A new `source/main/assets/assetMetadataResolver.ts`: the read that answers from disk, the rule that
  decides which subjects are due, the fetch, the per-property verification, the row construction and
  the write, and the callback that emits what resolved.
- A colocated `source/main/assets/assetMetadataResolver.realfs.spec.ts`.

## Non-Goals

- No IPC. The resolver takes a callback and does not know what a channel is.
- No timer of any kind. Resolution is demand-driven, which is the whole point of removing the poll in
  `task-018`.
- No image fetch. `task-011` owns the logo, which the bulk query never requests.
- No user-facing error. Offline is a state, not a failure: the fetch fails, the cache answers from
  disk, absent rows stay absent, and nothing is surfaced.
- No enumeration. The resolver is always handed the subjects it is asked about.

## Dependencies

- `task-005`, `task-006`, `task-007` and `task-009`, all complete.

## Research Consulted

- `.agent/plans/asset-metadata-cache/asset-metadata-cache-prd.md`: the decimals resolution order at
  `:1134-1141`, the corpus table under it at `:1145-1155`, the freshness argument at `:1019-1047`,
  and the advisory at `:1213-1232`.
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`: the `task-010` entry, plus
  `task-014`, `task-016` and `task-019`, which consume it.
- The three completed phase-2 plans, for the row types, the client's result shape and the
  verification verdict.

## Docs, Workflows, and Skills Consulted

- Docs: `.agent/plans/asset-metadata-cache/task-plans/readme.md`, `CLAUDE.md`.
- Workflows: `.agent/workflows/test.md` for the Jest invocation.
- Skills: none apply.

## Live Repo Findings Verified For Planning

Verified at `41f98a5d6` on branch `docs/asset-metadata-cache-plan`, 2026-09-14.

**The three modules this one sequences, and the exact surface each offers.**

- `assetMetadataDb.ts` exports `openAssetMetadataDatabase(filePath?)`, and the instance offers
  `readMetadata`, `writeMetadata`, `readResolutions`, `writeResolutions` and `close`. Both writes
  take an optional explicit timestamp, which exists so the resolver and the registry client can
  agree on one clock.
- `assetRegistryClient.ts` exports `queryAssetRegistry(subjects, options)` returning
  `{ entries, resolutions }`, where `resolutions` is one `AssetResolutionWrite` per distinct subject
  asked for and `retryAfter` is an absolute instant computed as `now + backoff(failureCount)`.
- `assetVerification.ts` exports `verifyRegistryProperty(subject, policy, propertyName, property)`
  returning `{ bound, satisfied, attested, verified }`, where `verified` is the conjunction.

**`retry_after` is an instant, so the due rule is one comparison.** `task-007` set it that way and
recorded the handoff: the resolver passes the same `now` to `queryAssetRegistry` and to
`writeResolutions`, so the stamped `attempted_at` and the computed `retryAfter` come from one clock.

**What `verified` means in the one column the schema has.** The schema carries a single `verified`
column and the PRD asks for per-property verification, so the two have to be reconciled and the PRD
says how. `:1134-1141` gives the decimals resolution order as an explicit user setting, then "the
cached registry value, if and only if `verified = 1`", then none. `:1145-1155` bins the whole corpus
by the verification outcome of the `decimals` property specifically. `:1213-1216` words the advisory
as "the issuer's published decimal places for this token could not be verified against its minting
policy". Every one of those is about `decimals`. So the column is the verdict for the `decimals`
property of that row, and this plan writes it that way and says so.

Names and tickers are not gated on it. Goal one at `:1093-1131` asks for them with no verification
condition, and the registry is already the source Daedalus trusts for a display name today. Nothing
in the design behaves differently for an unverified name, and a column nothing reads is a column that
goes stale.

**An unverified decimals value is still stored.** `:1140-1141`: "An unverified registry value never
formats anything. It stays available to the asset settings dialog as the recommended value, which is
exactly what it is today." So the value is written and `verified` is 0, rather than the value being
dropped.

**Decimals above 20 cannot be stored at all.** `asset_metadata` carries
`CHECK (decimals IS NULL OR (decimals >= 0 AND decimals <= 20))`, from
`source/renderer/app/config/assetsConfig.ts:1`. cardano-wallet accepts 0 to 255, so a registry value
above 20 is possible in principle. Writing one would make the engine refuse the whole row, losing
the ticker and the name with it, so an out-of-range value is stored as null and the rest of the row
survives.

**The refresh window and why the miss path is the one that matters.** `:1019-1047`: of 7,976
registry mapping files at commit `c9cf09f4`, 7,464 sit at sequence number 0 on every property, and
in the last 365 days 375 commits touched `mappings/` while only 38 files were modified rather than
added. The live event is a user acquiring a token the cache has never seen, not a record changing
under one they already hold.

**Nothing collides.** `grep -rn "assetMetadataResolver" source tests` returns nothing.

**The two traps carried forward both apply.** `blake2b`'s realm-sensitive input check is handled
inside `assetVerification`, and `strict: false` narrowing is handled with `===` on every
discriminated result.

## Files Expected To Change

- `source/main/assets/assetMetadataResolver.ts` — new.
- `source/main/assets/assetMetadataResolver.realfs.spec.ts` — new. The task graph names
  `assetMetadataResolver.spec.ts`; this spec drives a real database file, so it takes the
  `<Unit>.realfs.spec.ts` name for the same reason `task-006`'s did.
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json` — `task-010` status.
- `.agent/plans/asset-metadata-cache/task-plans/task-010*.md` — this plan and its two review logs.

## Implementation Approach

1. **Two primitives and one convenience over them.**

   ```ts
   readCached(subjects: Array<string>): Array<AssetMetadataRow>
   resolve(subjects: Array<string>): Promise<Array<AssetMetadataRow>>
   request(subjects: Array<string>): Array<AssetMetadataRow>
   ```

   `readCached` is synchronous and touches only the database. `resolve` fetches what is due, verifies,
   writes and emits. `request` is `readCached` plus a background `resolve` for whatever is due, and
   is what the IPC handler in `task-014` will call: a read never waits on the network, and a miss
   schedules the work that will fill it. The background promise is kept and exposed as `pending()`,
   so a rejection has an owner and a test can wait for the work it started.

2. **The due rule, three clauses.** A subject is due when it has no metadata row, or when its row's
   `updated_at` is older than the refresh window, and in both cases only when it has no
   `asset_resolution` row whose `retry_after` is still in the future. Both reads are one call each,
   so deciding what to fetch costs two statements however many subjects are asked about.

3. **In-flight subjects are not asked for twice.** A set of subjects currently being resolved is
   subtracted before batching. Two renders in quick succession over the same token list are the
   ordinary case, not an edge one.

4. **The database is not touched across the network call.** Read what is due, release, `await` the
   query, then verify and write. There is no database call inside the awaited section and no
   transaction open across it.

5. **A row is built from an entry and its per-property verdicts.**

   - `policy_id` and `asset_name` are the subject split at 56 characters, which is where the primary
     key's `CHECK` requires them to come from.
   - `ticker` and `name` are taken when the property is present and its value is a string.
   - `decimals` is taken when the property is present, its value is an integer, and it is within 0 to
     20. Out of range stores null rather than losing the row.
   - `verified` is `verifyRegistryProperty(subject, entry.policy, 'decimals', decimalsProperty).verified`,
     and is false when there is no decimals property to verify.
   - `metadata` is the JSON of `url` and `description` where present, or null.
   - `source` is `registry` and `slot` is null, which the two `CHECK` constraints on `source`
     require.
   - `sequence_number` is the maximum across the properties the row stores.

6. **`verified` is computed here from the bytes and never carried from the response.** The only input
   to it is `verifyRegistryProperty`, which takes the subject, the policy field and the property, and
   returns the conjunction of three independently computed facts. No field of a registry response can
   set it.

7. **The refresh rewrites only when a sequence number has risen, and two cases are rules rather than
   comparisons.** On a re-read the maximum sequence number in the response is compared against the
   stored one. Higher, and the new row is written. Otherwise the stored row is rewritten unchanged
   with a new `updated_at`, which is what stops a never-updated subject being re-read on every
   render. Both go through `writeMetadata`, so there is no second write path to keep consistent.

   A stored `sequence_number` of null means nothing is known about the stored row's version, not that
   it is at zero, so a response always rewrites it. In JavaScript a null would otherwise coerce to
   zero and compare equal to a response at sequence 0. And a stored row whose `source` is not
   `registry` is always rewritten by a registry response, because the two sources carry different
   version facts and the PRD's precedence rule is that the registry wins where both could answer.

8. **Resolutions are written for every subject asked for.** The client returns them; the resolver
   writes them with the same `now` it passed to the client, so `attempted_at` and `retry_after` come
   from one clock. A subject that resolved gets `resolved` with no retry, one the registry omitted
   gets `unregistered`, and one whose batch failed gets `failed`, each with the client's backoff.

9. **Emission carries what changed, not what was written.** A timestamp-only touch on an unchanged
   row is written, because that is what stops the subject being re-read on every render, and is not
   emitted, because nothing downstream has anything to do with it. That is the common case rather
   than an edge one: 7,464 of 7,976 registry subjects sit at sequence number 0 on every property. The
   callback is called once per resolve with the rows whose content is new or changed, and not at all
   when there are none. Errors thrown by the callback are caught and logged, because a consumer that
   throws must not take the resolver down.

10. **Offline is not an error.** A failed fetch produces no entries, resolution rows in state
    `failed`, and a debug log line without the subject list. `readCached` keeps answering.

## Acceptance Criteria

Carried from the task graph, with the check that settles each.

1. **`yarn test:jest` passes against a stubbed transport and a temporary database.**
2. **No `setInterval` or `setTimeout` schedules unsolicited work.** The module contains neither.

Eight this task adds to its own closure:

3. A read for a cached subject returns without touching the transport.
4. A read for an uncached subject returns absent and schedules a fetch.
5. A subject inside its retry-after window is not refetched, and one whose window has passed is.
6. A response that fails verification is written with `verified = 0` and is not retried immediately.
7. With the transport unavailable, reads still answer from disk.
8. A row younger than the refresh window is answered from disk without a fetch; one older is re-read
   on demand and is rewritten only when a sequence number has risen.
9. `verified` is the verdict for the `decimals` property, asserted with a subject whose decimals
   verifies and one whose decimals is tampered while its ticker is untouched.
10. `compile`, `lint` and `i18n` are green from `nix build`, `package.json` and `yarn.lock` are
    unchanged, and there are no new `@ts-ignore` or `@ts-expect-error`.

## Verification Plan

**Reading.**
- A subject with a row is returned by `readCached` with the transport stub asserted to have no calls.
- A subject with no row returns nothing from `readCached`, and `request` returns nothing and starts a
  fetch.
- `readCached` for an empty list returns an empty array and makes no call.

**The due rule.**
- A subject with a fresh row is not fetched by `request`.
- A subject whose row's `updated_at` is one millisecond older than the window is fetched; one
  millisecond newer is not. Driven by writing the row with an explicit timestamp, which `task-006`
  exposed for this.
- A subject with no row but a resolution row whose `retry_after` is in the future is not fetched.
- The same subject with `retry_after` in the past is fetched.
- A subject in state `unregistered` behaves the same way, which is the case that stops an
  NFT-holding wallet re-asking forever.

**Verification and the column.**
- An entry whose `decimals` verifies is written with `verified` true and the decimals value present.
- The same entry with its `decimals` signature tampered is written with `verified` false and the
  decimals value still present, because an unverified value is still the recommended one.
- The same entry with its `ticker` signature tampered but its `decimals` intact is written with
  `verified` true, which is what proves the column is the decimals verdict rather than an average.
- An entry with no `decimals` property is written with `verified` false and decimals null.
- An entry whose `decimals` value is 21 is written with decimals null and the ticker intact, rather
  than the whole row being refused.
- An entry with no policy field is written with `verified` false and is not logged as an error.

**Fetching, writing and emitting.**
- A successful resolve writes the rows, writes a `resolved` resolution for each subject, and calls
  the callback once with the rows that are new or changed.
- A resolve that writes nothing does not call the callback.
- A refresh that changes nothing writes a new `updated_at` and does not call the callback, asserted
  on both halves.
- A callback that throws does not propagate.
- With the transport failing, `resolve` writes no metadata, writes `failed` resolutions, and
  `readCached` still answers from disk.
- Two overlapping `request` calls for the same subject produce one query.

**Refresh.**
- A stale row whose response carries a higher sequence number is rewritten with the new values.
- A stale row whose response carries the same sequence numbers keeps its values and gets a new
  `updated_at`, asserted on both the content and the timestamp.
- A stale row whose response carries a lower sequence number keeps its values, which is the
  downgrade case.
- A stored row whose `sequence_number` is null is rewritten by a response at sequence 0, because null
  means nothing is known rather than zero.
- A stored row whose `source` is `chain` is rewritten by a registry response regardless of sequence
  numbers.

**Timers.**
- A resolve is run to completion under fake timers, the clock is advanced by a day, and the transport
  is asserted to have received no further call. That states the property, where asserting the absence
  of the string `setInterval` in the module's own source would only assert the absence of a string
  and would say nothing about the modules it imports.

**Commands.**
- `nix build '.#checks.x86_64-linux.jest' --no-link`
- `nix build '.#checks.x86_64-linux.compile' --no-link`
- `nix build '.#checks.x86_64-linux.lint' --no-link`
- `nix build '.#checks.x86_64-linux.i18n' --no-link`

## Risks and Open Questions

1. **The one `verified` column is given one meaning, and that meaning is a reading of the PRD rather
   than a sentence it contains.** The PRD asks for per-property verification and specifies a schema
   with one column. Every behaviour it describes for that column is about `decimals`. The reading is
   recorded here and in the module so that a later reader who expects "everything in this row is
   verified" finds out from the code rather than from a bug. If the project owner wants an unverified
   ticker suppressed rather than displayed, that is a different decision and a different column.
2. **The refresh window is seven days and it is an average.** The PRD says so: assets a real user
   holds skew toward active projects and are likelier than average to update, and nobody has measured
   how far above. Seven days is not derived from the held-asset rate and is not presented as if it
   were.
3. **A sequence number that does not rise freezes a changed value.** That is the specified rule, and
   it is what makes the refresh path cheap. A publisher who edits a value without bumping its
   sequence number is invisible to this cache until something else about the subject changes. The
   registry's own tooling bumps it.
4. **"Do not hold the database open across a network call" is read as holding no lock and making no
   database call inside the awaited section.** The handle itself stays open for the resolver's
   lifetime, because opening and closing a WAL database per operation would be slower and would churn
   the sibling files. Recorded because the wording admits a stricter reading.
5. No open questions beyond item 1, which is for the project owner and does not block anything.

## Required Docs, Research, and Tracking Updates

- Update `task-010`'s `status` in `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`
  to `completed` when the implementation review reads `approved`.
- Append to `task-010-plan-review.md` and `task-010-impl-review.md` as the cycle requires.
- No PRD change. The reading of the `verified` column is recorded here rather than by editing the
  PRD, because it is an interpretation of what the PRD already says and the PRD is the document it
  interprets.
- `task-019` inherits the column's meaning directly: it resolves decimal places by provenance and
  must test `verified` and nothing else.

## Review-Log Paths

- Planning review log: `.agent/plans/asset-metadata-cache/task-plans/task-010-plan-review.md`
- Implementation review log: `.agent/plans/asset-metadata-cache/task-plans/task-010-impl-review.md`

## Planning Status

`approved`

## Build Status

`completed`

## Current Outcome

- Plan drafted, critiqued once and revised. Approved for build execution.

## Final Outcome

- `task-010` complete. Reviewed and approved in `task-010-impl-review.md`.
- A read answers from disk and never waits on the network. A miss schedules exactly one fetch however
  many callers asked for the same subject, because subjects are claimed synchronously at schedule
  time rather than when the fetch begins.
- The single `verified` column is the verdict for the `decimals` property, which is what every
  behaviour the PRD describes for it is about. The case that carries the argument is that a failed
  ticker signature leaves the row verified.
- The refresh path writes `updated_at` on every successful re-read and emits only rows whose content
  changed, which matters because 7,464 of 7,976 registry subjects have never been edited.
- Checks, all from `nix build`: `compile` exit 0, `lint` exit 0, `i18n` exit 0, `jest` 80 suites and
  1164 tests with 1161 passed and 3 skipped. `package.json` and `yarn.lock` are unchanged.
- Handoff to `task-014`: push resolved rows from the `onResolved` callback rather than awaiting
  `pending()`, which would put the network back in front of a render.

## Self-Review

- The one genuinely interpretive decision in this task, what the single `verified` column means, is
  argued from four separate places in the PRD and then stated in one sentence.
- The due rule is tested at its boundaries rather than in the middle, on both the freshness window
  and the retry window.
- The refresh cases cover a sequence number that rose, one that stayed and one that fell, because the
  middle one is the common case and the last one is the one an implementation gets wrong.
