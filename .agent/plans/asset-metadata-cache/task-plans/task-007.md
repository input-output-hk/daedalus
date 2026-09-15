# Task task-007: Registry client: batch query, timeout, retry and backoff

## Task ID and Title

- ID: `task-007`
- Title: `Registry client: batch query, timeout, retry and backoff`

## Why Chosen Now

`task-007` has no dependencies in the task graph, and three tasks name it as one: `task-008` verifies
the entries it returns, `task-010` sequences it, and `task-011` fetches a logo through the same
transport. It is the last phase-2 module that can be built without a verification chain in place, so
building it now leaves `task-008` and `task-009` with a real response shape to verify against rather
than a hand-written fixture.

## Interaction Mode

- Mode: `agent_execution`

Every acceptance criterion but one is assertable against a stubbed transport. The exception is the
selfnode endpoint, which the task graph itself assigns to manual QA (`task-027`); what this task
owes it is a resolution function whose selfnode branch is unit-tested and an operator procedure.

## Scope

- A new `source/main/assets/assetRegistryClient.ts`: endpoint resolution, batch sizing in request
  bytes, the HTTP transport with its timeout and response cap, the retry and split rules, response
  normalisation, and the `asset_resolution` rows that describe what happened to every subject asked
  for.
- A colocated `source/main/assets/assetRegistryClient.spec.ts` driving all of it against a stubbed
  transport.

Revertible on its own. Nothing imports the module yet.

## Non-Goals

- No database. The client returns the resolution rows it would have written; `task-010` writes them.
  The task graph gives this task no dependency on `task-006`, and `task-010` is described there as
  the one module that owns the order of operations, so a client that opened the database would take
  that ownership back. See Risks for how the task graph's wording is met.
- No verification. `verified` is `task-008` and `task-009`'s output, and no field of a response read
  here may set it.
- No logo. The bulk query does not request that property, which is what bounds the volume at the
  transport rather than only in the store.
- No interval timer and no scheduling. Every request this module makes is one a caller asked for.
- No address-range filtering. `AnchorFetchService` performs it because a DRep anchor URL is
  attacker-supplied; the registry host is configuration, so the same code here would be ceremony.
- No caching. A second call for the same subject issues a second request; deciding not to ask is
  `task-010`'s.

## Dependencies

- None in the task graph. `task-007` has `"dependencies": []`.
- A type-only import of `AssetResolutionWrite` from `task-006`'s module, so that the rows this
  client produces cannot name a state the schema refuses. No runtime dependency on that module.

## Research Consulted

- `.agent/plans/asset-metadata-cache/asset-metadata-cache-prd.md`: the endpoint and the selfnode case
  at `:655-668`, the request body at `:670-682`, batching at `:684-700`, timeout and retry at
  `:702-706`, the 4xx rule at `:708-714`, the unregistered rule at `:716-724`, and offline at
  `:728-730`.
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`: the `task-007` entry, plus
  `task-008`, `task-010` and `task-011`, which consume it.
- `.agent/plans/asset-metadata-cache/task-plans/task-006.md`, for the row types and the handoff.

## Docs, Workflows, and Skills Consulted

- Docs:
  - `.agent/plans/asset-metadata-cache/task-plans/readme.md` for the cycle and the section list.
  - `CLAUDE.md` for the spec conventions and the `@ts-ignore` rule.
- Workflows:
  - `.agent/workflows/test.md` for the Jest invocation, read against the `CLAUDE.md` trust map.
- Skills: none apply.

## Live Repo Findings Verified For Planning

Verified at `084854a71` on branch `docs/asset-metadata-cache-plan`, 2026-09-14.

**The endpoint, and where each candidate comes from.** `source/main/config.ts:71` declares
`metadataUrl?: string` on `LauncherConfig`, and `nix/internal/launcher-config.nix:36-40` holds the
per-network values: `https://tokens.cardano.org` for mainnet and
`https://metadata.world.dev.cardano.org` for preprod and preview, assigned at `:76`.
`nix/internal/launcher-config.nix:448-450` assigns `metadataUrl` under
`lib.optionalAttrs (network != "selfnode")`, so on selfnode the key is absent.
`source/main/index.ts:216-219` is the existing literal fallback,
`metadataUrl ?? 'https://tokens.cardano.org'`, which on selfnode would point the fetch path at
mainnet.

**The selfnode case has constants already, and nothing uses them.** `source/main/config.ts:169-172`
declares `MOCK_TOKEN_METADATA_SERVER_URL = 'http://127.0.0.1'` and
`MOCK_TOKEN_METADATA_SERVER_PORT = process.env.MOCK_TOKEN_METADATA_SERVER_PORT || 0`, with a comment
recording that `localhost` breaks under the current Electron because it prefers IPv6.
`grep -rn "MOCK_TOKEN_METADATA_SERVER" source --include=*.ts` returns those three lines and nothing
else, so the constants exist and no code reads them. They are what the selfnode branch composes its
URL from, and the port arriving from the environment is why the branch cannot be a literal.
`source/main/environment.ts:111` exports `isSelfnode` on `environment`, which is how the branch is
taken.

**Batching, re-measured live today rather than carried from the PRD.** Against
`https://tokens.cardano.org/metadata/query`, with the five properties this client requests and
subjects drawn from the registry's own mapping list:

| subjects | request bytes | HTTP | response bytes |
|--:|--:|---|--:|
| 45 | 3,397 | 200 | 77,712 |
| 50 | 3,762 | 200 | 85,226 |
| 69 | 6,099 | 200 | 94,772 |
| 85 | 8,067 | 200 | 102,812 |
| 86 | 8,190 | 200 | 103,314 |
| 90 | 8,682 | **413** | 0 |
| 100 | 9,912 | **413** | 0 |
| 110 | 11,142 | **413** | 0 |

The cap sits between 8,190 and 8,682 request bytes, consistent with 8,192. The PRD's table records
90 subjects at 7,156 bytes returning 200, and 90 subjects here is 8,682 bytes and returns 413. Both
are true: the subject count is not the variable, which is the finding, and a client that sized by
count would fail on asset name length alone. The 6 KB ceiling leaves about 2 KB of headroom at the
worst case measured.

**Response size, which sets the read cap.** The largest response measured was 103,314 bytes for 86
subjects with no logo requested. `ANCHOR_MAX_BYTES` at
`source/main/governance/AnchorFetchService.ts:8` is 1 MiB, which is about ten times the largest
measured response, and is the number this module reuses rather than inventing a second one.

**The response shape, from a live capture.** Each entry carries `subject` and `policy`, and each
property carries `{ value, sequenceNumber, signatures: [{ signature, publicKey }] }`. `policy` comes
back whether or not it is requested, which is what makes verification possible from the bulk query
alone. A captured mainnet response for subject
`c76ef5451f551f3c06d48c46b153cb35221b507683b2e413122661b942544544` carries `name`, `description`,
`url`, `ticker` and `decimals` in exactly that shape, with `additionalProperties` present and empty.

**The shape to copy for the transport, and the part not to copy.**
`AnchorFetchService.ts:232-292` is the request: a wall-clock budget, a status check, a content-type
check, a declared-length check, a running byte count that destroys the response when it is exceeded,
and a `settled` flag so a timeout and an error cannot both resolve. `:48-157` is the address-range
filtering, which exists because a DRep anchor URL comes from the chain. The registry host comes from
configuration, so that part is not copied, per the task graph.

**Logging.** `source/main/utils/logging.ts:34-39` exports `logger` with `debug`, `info`, `warn` and
`error`, each taking a message and a context object.

**Nothing collides.** `grep -rn "assetRegistryClient\|metadata/query" source tests` returns nothing.

## Files Expected To Change

- `source/main/assets/assetRegistryClient.ts` — new.
- `source/main/assets/assetRegistryClient.spec.ts` — new.
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json` — `task-007` status.
- `.agent/plans/asset-metadata-cache/task-plans/task-007*.md` — this plan and its two review logs.

No deviation from the task graph's `targetPaths`. This spec uses no real filesystem, so the plain
`.spec.ts` name is the right one.

## Implementation Approach

1. **Endpoint resolution, four candidates in order.**

   ```ts
   export const assetRegistryEndpoint = (override?: string | null): string
   ```

   An explicit override, then `launcherConfig.metadataUrl`, then
   `${MOCK_TOKEN_METADATA_SERVER_URL}:${MOCK_TOKEN_METADATA_SERVER_PORT}` when
   `environment.isSelfnode`, then the mainnet literal. The override parameter is the slot the
   metadata source setting fills in `task-031`; nothing supplies it today, so every network resolves
   exactly as the task graph specifies. Which of the override and the launcher value should win is
   a question for `task-031` and is recorded under Risks rather than decided here.

2. **Batch sizing in request bytes.** The body is
   `{"subjects":[...],"properties":["name","ticker","decimals","url","description"]}`. The builder
   starts from the byte length of that body with no subjects and adds
   `Buffer.byteLength(JSON.stringify(subject))` per subject plus one byte for the separating comma,
   closing the batch when the total would exceed 6,144 bytes. Measuring each subject through
   `JSON.stringify` rather than by character count is what keeps the arithmetic exact if a subject
   ever contains a character JSON escapes. A single subject that exceeds the ceiling on its own is
   sent alone rather than dropped.

3. **Batches are issued one after another.** A `for` loop with an `await`, so a wallet with many
   tokens opens one socket at a time.

4. **The transport is an interface with one implementation.**

   ```ts
   export type RegistryTransportResult =
     | { ok: true; status: number; body: string }
     | { ok: false; reason: 'timeout' | 'network' | 'too-large' };

   export interface RegistryTransport {
     post(url: string, body: string, timeoutMs: number): Promise<RegistryTransportResult>;
   }
   ```

   The default posts over `https:` or `http:` chosen by the URL's protocol, because the selfnode mock
   is plain HTTP on the loopback address. It carries a 10-second wall-clock budget matching
   `ANCHOR_TIMEOUT_MS`, sends `content-type` and `accept` of `application/json`, refuses a declared
   `content-length` over the cap before reading a byte, counts bytes as they arrive and destroys the
   response at the cap, and resolves exactly once.

5. **Status handling, one rule per class.** 200 is parsed. Any 4xx other than 413 is a client-side
   defect: logged at warn with the body size and the subject count and never scheduled for retry. A
   5xx, a timeout or a network error gets one retry after a short backoff, then the batch is
   abandoned. A response over the byte cap is discarded and not retried, since the same request would
   produce the same oversized response.

6. **413 splits, and the split terminates.** A batch of more than one subject is divided by count,
   any remainder going to the second half, and each half is sent as an ordinary request. A half
   inherits every rule except splitting: its own retry on a timeout or a 5xx, its own status
   handling, and its own subjects' outcomes, so a half that returns 200 resolves its subjects while
   the other half fails its own. A batch of one subject that returns 413 is recorded `failed` and is
   not split, because half of one subject is either the identical request or an empty one, and the
   endpoint that refused it at that size will refuse it at every size.

7. **The incoming subject list is deduplicated, first occurrence kept.** Several wallets holding one
   token is the case this whole design is shaped around, so a duplicated subject is the expected
   input rather than a defensive hypothetical. Without this it would occupy space in two batches and
   produce two resolution rows for one primary key.

8. **Every subject asked for comes back with a resolution, and a resolution belongs to a subject
   rather than to a batch.** The result carries the normalised entries and one
   `AssetResolutionWrite` per distinct subject in the call:

   - present in a 200 response: `resolved`, failure count 0, `retryAfter` 0.
   - absent from a 200 response: `unregistered`, failure count incremented, `retryAfter` set from
     the backoff. The registry omits what it does not know rather than returning a negative, so
     without this row every demand re-schedules the same subject and the poll this plan deletes comes
     back, worst for the NFT-holding wallets the registry essentially never covers.
   - batch abandoned or refused: `failed`, failure count incremented, `retryAfter` set from the
     backoff.

9. **`retry_after` is an instant, not a duration.** It is `now + backoff(failureCount)` in epoch
   milliseconds, so `task-010`'s rule reads as one comparison against the clock rather than as
   arithmetic over two columns. The client and the database must not disagree about `now`: `task-006`'s
   `writeResolutions` takes an explicit timestamp for exactly this, and `task-010` passes the same
   one it handed the client. That is recorded here as the handoff.

10. **The backoff doubles from five minutes to a day.** `backoff(n) = min(5 min * 2^(n-1), 24 h)`, so
   a subject that keeps failing settles at one attempt a day rather than never being asked again.
   Five minutes is short enough that a wallet opened during a brief outage recovers within the
   session, and a day is long enough that several hundred permanently unregistered NFT subjects cost
   one batch a day between them.

11. **Normalisation is a filter, not a cast, and it is scoped to one request.** An entry whose
    subject is not in the subject list of the request that produced the response is dropped and
    logged at debug: a server that answers a question nobody asked must not be able to create a row,
    and after a split there are more requests than batches, so the call's whole subject list is the
    wrong thing to check against. A
    property without a numeric `sequenceNumber`, or without a `signatures` array of objects carrying
    two strings, is dropped rather than carried forward as a shape later code has to re-check.
    `additionalProperties` is not requested, is empty in the live capture, and is not carried.

12. **Failures are logged without the subject list.** At debug for the ordinary offline case, at warn
    for a 4xx, which is a defect in this code rather than a condition of the network. The warn line
    carries the body size and the subject count, which is what a reader needs to diagnose a sizing
    error, and no subject.

## Acceptance Criteria

Carried from the task graph, with the check that settles each.

1. **A batch is sized by serialized request body bytes with a ceiling of 6 KB, and a unit test with
   100 maximum-length subjects produces more than one batch.** Driven with 100 subjects of 56 hex
   characters of policy id plus 64 of asset name.
2. **A 413 response splits the batch once and re-sends; a second 413 is recorded without a retry.**
   Driven with a stub that counts calls.
3. **No 4xx is ever scheduled for retry, asserted per status code.** Driven over 400, 401, 403, 404,
   413 and 429, each asserting the call count and the resolution state.
4. **Subjects present in a request but absent from a 200 response are written to `asset_resolution`
   in state `unregistered`, asserted against a stub that omits one of two subjects.** The rows are
   produced here and written by `task-010`; see Risks.
5. **No timer anywhere in the module schedules unsolicited work.** The only `setTimeout` is the
   backoff between the two attempts of a batch a caller asked for, and the request budget inside the
   transport. There is no `setInterval` and no module-scope timer.
6. **`yarn test:jest` passes against a stubbed transport.**
7. **On selfnode the client resolves to the bundled mock and not to `tokens.cardano.org`, asserted
   by a manual QA case.** The resolution function's selfnode branch is unit-tested here; the
   end-to-end case belongs to `task-027` and the procedure is under Verification Plan.

Four this task adds to its own closure:

8. `compile`, `lint` and `i18n` are green from `nix build`.
9. No new `@ts-ignore` and no new `@ts-expect-error`, and `package.json` and `yarn.lock` unchanged.
10. A response larger than the cap is discarded and the subjects are recorded rather than returned
    half-parsed.
11. The `logo` property is absent from the request body, asserted against the serialized body rather
    than against the constant.

## Verification Plan

**Endpoint resolution.** Four cases, each with `../config` and `../environment` mocked: an override
wins; the launcher value is used when there is no override; the selfnode branch composes
`http://127.0.0.1:<port>` from the two constants when the launcher value is absent; and the mainnet
literal is used when nothing else applies. The selfnode case asserts the resolved URL is not
`https://tokens.cardano.org`, which is the failure the PRD says is silent.

**Batch sizing.**
- 100 subjects of maximum length produce more than one batch, and every batch's serialized body is
  at most 6,144 bytes.
- A single subject that exceeds the ceiling alone is still sent, in a batch of one.
- An empty subject list issues no request at all.
- The serialized body carries exactly the five properties and does not contain `logo`.

**Status handling, against a stub that records every call.**
- 200: entries are returned and every requested subject is `resolved`.
- 200 omitting one of two subjects: the present one is `resolved` and the absent one is
  `unregistered`, which is the criterion stated in the task graph.
- 413: the batch is split in half and both halves are sent, so the call count is three. A stub that
  returns 413 to everything produces three calls and no more, and every subject is `failed`.
- 413 on a batch of one subject: one call, the subject is `failed`, and nothing is split.
- 413 on the first request with one half then returning 200 and the other timing out twice: the
  first half's subjects are `resolved` and the second half's are `failed`, which is what proves a
  resolution belongs to a subject rather than to the original batch.
- 400, 401, 403, 404 and 429: one call each, every subject `failed`, and no retry.
- 500 and 503: two calls, then abandoned.
- Timeout: two calls, then abandoned.
- Network error: two calls, then abandoned.
- A body over the response cap: reported by the transport as `too-large`, one call, subjects
  `failed`.
- Malformed JSON in a 200: one call, subjects `failed`, nothing thrown.

**Backoff.**
- `backoff(1)` through `backoff(10)` double until the ceiling and then stay there, asserted at the
  first value that reaches the ceiling and at the one after it.
- A subject whose incoming failure count is 3 gets a longer `retryAfter` than one whose count is 1,
  from the same call.
- `retryAfter` is an instant: it is greater than the `now` handed in, by exactly the backoff.

**Normalisation.**
- An entry for a subject that was not requested is dropped.
- After a split, an entry belonging to the other half is dropped from the half that did not ask for
  it.
- A subject supplied twice is requested once and produces one resolution row.
- A property with a missing or non-numeric `sequenceNumber` is dropped and the rest of the entry
  survives.
- A property whose `signatures` is not an array of two-string objects is dropped.
- An entry with no `policy` field is returned with `policy` null rather than dropped, because that is
  the ordinary state of a large share of the registry and `task-008` writes it unverified.

**Sequencing.**
- With three batches' worth of subjects, the stub records that no request starts before the previous
  one resolves.

**Manual QA, handed to `task-027`.** Start a selfnode cluster with
`MOCK_TOKEN_METADATA_SERVER_PORT` set, hold a token the mock knows, and confirm from the main
process log that the request went to `http://127.0.0.1:<port>/metadata/query`. The expected evidence
is that log line and a resolved row; the failure it catches is silent, because a mainnet registry
answers a selfnode query plausibly.

**Commands.**
- `nix build '.#checks.x86_64-linux.jest' --no-link`, with both new files staged.
- `nix build '.#checks.x86_64-linux.compile' --no-link`
- `nix build '.#checks.x86_64-linux.lint' --no-link`
- `nix build '.#checks.x86_64-linux.i18n' --no-link`
- `git diff -- package.json yarn.lock` must be empty.

## Risks and Open Questions

1. **The task graph's fourth acceptance criterion says the client writes to `asset_resolution`, and
   this client does not.** It produces the rows, typed as `task-006`'s `AssetResolutionWrite`, so
   they cannot name a state the schema refuses, and `task-010` writes them. Two things in the task
   graph say this is the intended division: `task-007` has no dependency on `task-006`, and
   `task-010` is described there as the one module that owns the order of operations, with the
   instruction not to hold the database open across a network call. A client that opened the database
   would contradict both. The substance of the criterion, that a subject omitted from a 200 response
   is recorded as `unregistered` rather than re-asked forever, is asserted here against a stub that
   omits one of two subjects.
2. **The endpoint ordering in the PRD would make the metadata source setting unreachable.**
   `launcherConfig.metadataUrl` is populated on every network except selfnode, so a resolution order
   that consults it before the configured setting means the setting never applies where a user would
   set it. This task does not resolve that: nothing supplies an override yet, so behaviour today is
   identical either way. It is named here so `task-031` decides it deliberately rather than
   discovering it.
3. **The response cap is a read-side bound and not a rate limit.** A host that answers every request
   with exactly one byte under the cap can still cost a wallet one megabyte per batch. That is the
   same exposure `AnchorFetchService` accepts for anchors, the host is configuration rather than
   attacker-supplied, and batches are sequential, so the ceiling is one outstanding response.
4. **No address filtering, which becomes a question when the endpoint becomes user-settable.** Today
   the host is configuration. In `task-031` it becomes a URL a user types, at which point the
   loopback and private-range question is worth asking on its own terms rather than inheriting a
   decision made here.
5. **The five-minute and one-day backoff bounds are chosen, not measured.** There is no corpus that
   says what the right retry interval for an unregistered NFT subject is. They are recorded here as
   choices with their reasoning so that a later measurement can move them rather than rediscover
   them.
6. No open questions for the project owner.

## Required Docs, Research, and Tracking Updates

- Update `task-007`'s `status` in `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`
  to `completed` when the implementation review reads `approved`.
- Append to `task-007-plan-review.md` and `task-007-impl-review.md` as the cycle requires.
- No PRD change. The live re-measurement agrees with the PRD's conclusion, that batches are sized in
  bytes and not in subjects, and differs only in which sample of subjects was used.
- `task-027` gains the selfnode manual QA case described under Verification Plan.
- `task-031` inherits the endpoint ordering question under Risks.

## Review-Log Paths

- Planning review log: `.agent/plans/asset-metadata-cache/task-plans/task-007-plan-review.md`
- Implementation review log: `.agent/plans/asset-metadata-cache/task-plans/task-007-impl-review.md`

## Planning Status

`approved`

## Build Status

`completed`

## Current Outcome

- Plan drafted, critiqued once and revised. Approved for build execution.

## Final Outcome

- `task-007` complete. Reviewed and approved in `task-007-impl-review.md`.
- Batches are sized in request bytes with a 6 KB ceiling, re-measured live rather than carried from
  the PRD: 8,190 request bytes returned 200 and 8,682 returned 413, and the 90-subject request that
  the PRD recorded at 7,156 bytes was 8,682 here, which is the same conclusion reached from a
  different sample.
- Every subject handed in comes back with an outcome, including the one a 200 response silently
  omits, which is what stops an NFT-holding wallet re-asking forever.
- One finding for every later module in this phase: under `strict: false` a discriminated union does
  not narrow by truthiness of a boolean-literal discriminant, only by equality. It fails at compile
  rather than at runtime.
- Checks, all from `nix build`: `compile` exit 0, `lint` exit 0, `i18n` exit 0, `jest` 78 suites and
  1078 tests with 1075 passed and 3 skipped. `package.json` and `yarn.lock` are unchanged.
- Two questions handed forward: `task-027` gains the selfnode endpoint case, and `task-031` inherits
  the ordering between the launcher value and the metadata source setting.

## Self-Review

- The batch sizing was re-measured against the live endpoint today rather than carried from the PRD,
  and the measurement disagrees with the PRD's table in a way that strengthens rather than weakens
  its conclusion: the same subject count produced a different byte count and a different status.
- The one place this task does not do what its acceptance criterion literally says, writing to the
  database, is argued from two other entries in the same task graph rather than from convenience.
- The selfnode branch is the one failure in this module that is silent when it is wrong, so it gets
  both a unit case and a manual procedure with its expected evidence.
