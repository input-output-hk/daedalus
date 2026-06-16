# Slice-1 Final Pass — Review Distillation

> **Status:** CODE-COMPLETE (automated gates green) · manual live-node verification LARGELY DONE on preprod (FP-8 partial, FP-11 done) · interactive refresh + error-path checks PENDING
> **Date:** 2026-06-15 · **Manual-gate update:** 2026-06-16 (live preprod run)
> **Cross-references:** [slice-1-final-pass-PRD.md](./slice-1-final-pass-PRD.md) · [slice-1-code-review.md](./slice-1-code-review.md) · [research/slice-1-final-pass-findings.md](../research/slice-1-final-pass-findings.md) · [research/drep-state-preprod-epoch295-sample.json](../research/drep-state-preprod-epoch295-sample.json)

This is the surviving deliverable of the slice-1 final pass. It distills the impl → review → fix sprint outcomes and the full per-dimension review into one durable record. It supersedes the disposable sprint plan (`slice-1-final-pass-sprint.yaml`), which should be removed now that the final pass is complete.

---

## Executive Summary

Slice-1 (DRep Discovery walking skeleton + log/analytics sanitization floor) is **CODE-COMPLETE and verified by automated gates**: the FP-7 authoritative gate is **64/64 focused governance Jest tests across 5 suites, 0 skipped** (GovernanceQueryService, GovernanceStore, governance-sanitization, Governance.spec, DRepDirectory), and **`tsc --noEmit` exits 0**. The pre-final-pass baseline was 52 tests; the final pass added 12.

The final pass landed five code fixes (FP-1, FP-2, FP-3, FP-9, FP-10) closing the acceptance-critical gaps found in the slice-1 code review — most importantly a real-node-breaking cardano-cli network-flag placement bug and the missing surfacing of actionable CLI error text in the renderer. Three bookkeeping tasks (FP-4, FP-5, FP-6) finalized the audit trail and plan-tasks ledger. **All five review dimensions report acceptanceMet: true. Zero blocker/major findings were confirmed.**

The two manual live-node items have now been **largely executed on a synced preprod node (2026-06-16)**: **FP-11 is fully resolved** (versions recorded, UTxO-HD backend confirmed `V2InMemory`, real query run, real fixture captured) and **FP-8 is partially confirmed** (directory loads end-to-end with real on-chain data; interactive refresh-reload and renderer-console checks remain, and the error paths are deferred by user decision). The standing version-gap risk (#3) is resolved for the live preprod app. See the updated "Manual Verification — Results" section for what was confirmed, what is pending, and the reproducible capture procedure.

---

## Review Results

**FP-7 authoritative gate (orchestrator-run):** 64/64 focused governance Jest across 5 suites, 0 skipped; `tsc --noEmit` exit 0. Baseline before the final pass was 52 tests.

### Per-task fix outcomes

| Task | Title | Outcome | Key files |
|---|---|---|---|
| **FP-1** | Add network flag to CLI queries | Impl initially **prepended** the flag before the era token — broken on a real node. The review subagent caught the blocker by running the real bundled `cardano-cli 10.15.0.0`; the fix **appends** the flag after the subcommand args. 26/26 service tests; orchestrator independently re-validated the corrected argv against the real binary (passes the network parser, only complains about the runtime-supplied `--socket-path`). | `GovernanceQueryService.ts`, `CardanoNode.ts`, `GovernanceQueryService.spec.ts` |
| **FP-2** | CLI error details to UI + harden IPC transport | Renders `error.details` in **both** the Failed state and the retained-list banner (the acceptance-critical gap). `governanceChannel.ts` now throws a marked plain object `{__governanceError, type, message, details}` (verified safe: `IpcChannel.onRequest` forwards the raw thrown value, plain objects survive structured clone). `_normalizeError` extended to handle marker + JSON-wrapped + `queryErrorType` + Error-instance paths. Review added an Error-instance fallback test and correctly **rejected** a trap test that would not hit the target branch. | `governanceChannel.ts`, `GovernanceStore.ts`, `DRepDirectory.tsx`, `DRepDirectory.spec.tsx`, `GovernanceStore.spec.ts` |
| **FP-3** | Guard duplicate hash-history push | Added the pathname guard to the `Governance.tsx` sub-tab handler. Review independently verified the sidebar path is **already** guarded by `AppStore._updateRouteLocation` (`currentRoute` is the live pathname), so the sprint YAML `root_cause` was overstated and no second code change was needed. New `Governance.spec.tsx` with negative + positive control. | `Governance.tsx`, `Governance.spec.tsx` |
| **FP-9** | Rename DRepStatus `expired` → `inactive` | Renamed across types/service/store/badge/scss/locales/specs/storybook. Review caught a stale generated `translations/messages.json` still carrying `status.expired`; fix regenerated it via the canonical formatjs extract. Zero stray `expired` literal remains; en-US `!!!` preliminary-copy convention preserved (`!!!Inactive`), ja-JP translated. | `governance.types.ts`, `GovernanceQueryService.ts`, `GovernanceStore.ts`, `DRepStatusBadge.tsx/.scss`, i18n locales, specs, storybook |
| **FP-10** | Remove `GovernanceStore.setup()` auto-fetch | Removed `fetchDRepList()` from `setup()`; the query now fires only on Governance-route entry (`DRepDirectoryPage.componentDidMount`) and explicit refresh. In-flight dedup intact; 2 new tests assert no fetch on setup and a single deduplicated fetch on route entry/refresh. | `GovernanceStore.ts`, `GovernanceStore.spec.ts` |
| **FP-4** | Bookkeeping | Verified `slice-1-code-review.md` is finalized and cross-references the Final Pass PRD (no edit). | — |
| **FP-5** | Bookkeeping | Moved `task-103` partial → complete (network-flag + active/inactive gaps closed; only residual is the design constraint that `drep-state` exposes just expiry). `auditSummary.statusCounts` now complete:8 / partial:0 / verified:3. | `governance-drep-discovery-plan-tasks.json` |
| **FP-6** | Bookkeeping | Removed 58 dead `origin` properties + cleaned 23 stale parentheticals, bumped metadata to v1.8, JSON validated. | `governance-drep-discovery-plan-tasks.json` |

> FP-8 (live preprod smoke) and FP-11 (LSM-backend verification) are not code fixes; they are manual live-node items — see the dedicated section below.

### Per-dimension acceptance

| Dimension | Acceptance met? | Summary |
|---|---|---|
| **Correctness & logic of FP-1/2/3/9/10 + integration** | ✅ Yes | All five fixes correctly implemented and integrated end-to-end. Network flag derived solely from `this._config.cluster`; appended after the subcommand per cardano-cli's per-subcommand parser; null-flag rejects before spawn; conway era-fallback re-appends the flag on retry. Error transport, history guard, status rename, and route-scoped fetch all verified. json-bigint precision and BigNumber rehydration confirmed lossless. |
| **Security & log/analytics sanitization floor** (never-thinned) | ✅ Yes | Floor holds. Vote-target redaction (drepId / dRepId / vote / voting + abstain/no_confidence sentinels) enforced via `filterLogData()` at the api.ts vote/delegation call sites; regression suite 17/17. No governance call site logs a vote target. Socket path is set in child env, never argv; spawn uses an args array with no shell (no injection). Renderer IPC request type is `void`. Anchor URL/hash parsed but never rendered to a navigable/fetch sink in slice-1. No analytics/sendEvent on the directory read path. |
| **IPC contract integrity & shared types** | ✅ Yes | DRep-list channel constant + request (`void`) / response (`DRepListQueryPayload`) declared once in `common/ipc/api.ts`, imported by both clients — success path cannot desync. Lovelace decimal-string → BigNumber boundary respected (json-bigint `storeAsString:true`; only the renderer rehydrates to BigNumber). Structured-error transport matches the D2/FP-2 decision. Shared types defined once in `common/types/governance.types.ts`. |
| **Test coverage & quality** | ✅ Yes | 64/64 across 5 suites. Genuinely meaningful: `GovernanceQueryService.spec` exercises the **real** @cardano-sdk/core CIP-129 derivation and **real** json-bigint (only spawn mocked); parse-failure suite is broad and adversarial; the conway era-fallback test asserts exact argv with the appended `--mainnet` (catches the FP-1 prepend regression); `_normalizeError` has all four transport branches; `DRepDirectory.spec` asserts `error.details` renders in both blocking and banner states. |
| **Design-token / vocabulary alignment & error UX** | ✅ Yes | `active \| inactive` vocabulary consistent end-to-end; repo-wide grep for `expired/expiring/retired` in governance source returns zero hits. Badge maps to `--badge-success-*` / `--badge-neutral-*` and always pairs color with a textual label. i18n consistent across en-US / ja-JP / defaultMessages (18 governance keys). Error UX renders generic heading + `error.message` + `error.details` (conditionally) in both Failed and banner states without duplication. |

---

## Potential Issues & Findings

**Confirmed blocker/major findings: 0.** No blocker or major finding survived adversarial verification. (Note: the test-coverage dimension prose self-flagged the route-entry automation gap as "major," but it was **not** escalated to a confirmed major in adversarial review and is carried below as the highest-value advisory.)

The items below are minors/advisories — none breaks the build, the gate, or an acceptance criterion. They are recorded so future slices do not re-walk the same ground.

### Standing technical risks (known and accepted)

1. **Unit tests mock `spawn` and cannot catch CLI grammar errors.** *Where:* `tests/jest/governance/GovernanceQueryService.spec.ts`. *Severity:* advisory (inherent mock-boundary limitation). The argv-shape assertions confirm the impl matches the developer's belief about cardano-cli grammar but cannot validate the grammar itself. The specific FP-1 prepend regression **is** caught by the exact-array + tail-flag assertions, but a *novel* flag/ordering the real CLI rejects is structurally unclosable at the unit level. *Action:* delegated to the manual FP-8 live-node smoke test; any future change to argv composition must be re-checked against the real binary (parse-only, `--socket-path` unset suffices). A real-binary parse-only smoke test for CLI argv composition remains an open advisory cleanup (tracked as **task-169**, ux-refinement phase).

2. **Conway-retry heuristic keys off the substring `"latest"`.** *Where:* `GovernanceQueryService._shouldRetryWithConway`. *Severity:* minor (fragility). The retry fires when failure text contains `latest` plus an era-ish keyword; any future `QueryFailed` message containing the word `latest` would trigger a spurious conway retry. The "network not set" message was intentionally worded to avoid `latest`. There is also **no negative test** proving the matcher *suppresses* retry on an ordinary non-era `QueryFailed` (the single highest-value missing service unit test). *Action:* add a case mocking both queries to close exit-1 with a non-era stderr and assert `spawn` called exactly twice (no conway retry); long-term, gate the retry on a structured marker instead of substring matching. Both halves are tracked as **task-169** (ux-refinement phase).

3. **~~Bundled `cardano-cli` is 10.15.0.0 locally; flake/release pins 11.0.1.~~ — RESOLVED (2026-06-16).** *Where:* dev bundle vs flake (PRD E1). *Severity:* ~~advisory (version gap)~~ closed. The FP-11 live run confirmed the **running** preprod app uses `cardano-cli 11.0.0.0` / `cardano-node 11.0.1` (git rev `97036a66b`); the 10.15.0.0 bridge (`/nix/store/kxfdg2w…`) is merely *also present* in the store, not what executes. FP-1 / era-fallback are thus validated on the real 11.x grammar, and the UTxO-HD assessment is keyed to the actual running 11.0.1 build (backend confirmed `V2InMemory`). No residual version gap for the live preprod app; a mainnet-build re-check is still worthwhile when mainnet testing begins.

### Notable minors / advisories

4. **Route-entry fetch is only partially covered by automation.** *Where:* test-coverage dimension. *Severity:* advisory (the dimension's self-flagged "major," not escalated). The store-level dedup and no-fetch-on-setup are unit-tested, but "query fires on route entry" via `DRepDirectoryPage.componentDidMount` is only partially exercised by automation. *Action:* fold into the FP-8 smoke walkthrough; optionally add a focused component test asserting the mount triggers the fetch on Idle/Failed.

5. **`development` → `--testnet-magic 42` is an unverified hard-coded magic.** *Where:* `GovernanceQueryService.ts` `setNetwork`. *Severity:* minor. Value 42 taken from the PRD with no test/cross-check; not in the mainnet/preprod acceptance targets. *Action:* source it from node config or add a unit test pinning `setNetwork('development')`.

6. **`__governanceError` wire contract enforced by convention, not types; channel-level mapping untested.** *Where:* `source/main/ipc/governanceChannel.ts`. *Severity:* minor. Producer/consumer bind via two matching string literals with no shared constant; `tsc` cannot catch a marker rename or a `queryErrorType`→`type` drift, and no test exercises the channel's actual wrapping. *Action:* define a `GovernanceWireError` interface in `common/types/governance.types.ts`, type the thrown object with it, and add one channel-level mapping test.

7. **FP-2 structured-clone survival verified by inspection, not by an integration test.** *Where:* `tests/jest/governance/GovernanceStore.spec.ts`. *Severity:* minor (test-quality gap). The marker-object-survives-clone claim is asserted only by code-reading; the test feeds `_normalizeError` an in-memory literal. *Action:* add a structured-clone simulation (Node `structuredClone`) or IPC-level test contrasting a marked object (keeps `details`) vs an Error instance (loses it). Compensated by FP-8.

8. **Sanitization floor's "never-thinned" guarantee rests on call-site discipline.** *Where:* `tests/jest/security/governance-sanitization.spec.ts`; governance error-logging sites (`governanceChannel.ts`, `GovernanceStore.ts`, `GovernanceQueryService.ts`). *Severity:* minor. `filterLogData()` is opt-in per call-site, not applied inside the logger; the floor holds only because no governance logger call currently passes a vote target, and nothing guards the query/error-logging path against a future regression. *Action:* add a regression assertion that governance error-logging payloads contain no `drepId`/vote field (or route those sites through `filterLogData`), and document in shared-design-tokens.md that the logger does not auto-sanitize.

9. **CLI stderr (error.details) may echo the node socket path into logs/UI.** *Where:* `GovernanceQueryService.ts`. *Severity:* minor (defense-in-depth, not a leak). Rendering stderr is a PRD acceptance requirement (FP-2 Part B) and React auto-escapes it; the socket path is not a secret. *Action:* optional hardening — cap `details` length and/or strip absolute filesystem paths before storing.

10. **`as any` cast on the credential passed to `Cardano.DRepID.cip129FromCredential`.** *Where:* `GovernanceQueryService.ts`. *Severity:* minor. Wrapped in try/catch that degrades to a typed `ParseFailed`, but the cast suppresses the SDK's compile-time guarantee. *Action:* use the SDK's `Cardano.Credential` type instead of `as any`.

11. **`DRepListQueryPayload.epoch` and `DRepDirectoryEntry.drepActivity` typed `number | null` but runtime never produces null.** *Where:* `common/types/governance.types.ts`. *Severity:* minor (type-accuracy). *Action:* narrow to `number`, or document that null is reserved for future compatibility.

12. **Component display logic exercised but not asserted.** *Where:* `DRepCard.tsx` `formatVotingPower` (M/K/null `—`) and `DRepIdDisplay` truncation/clipboard. *Severity:* minor. *Action:* add focused tests for the ₳-glyph thresholds, the `—` null fallback, and the truncate/tooltip aria-label.

13. **FP-9 `inactive` badge label is never asserted in rendered output in any locale.** *Where:* `DRepDirectory.spec.tsx`. *Severity:* minor. The data-layer `status==='inactive'` derivation is covered, but no rendered/translated assertion of the `!!!Inactive` / ja-JP label exists. *Action:* render at least one `status:'inactive'` entry (en-US + ja-JP) and assert the label text — guards both the FP-9 rename and locale-catalog completeness.

14. **ja-JP non-locked governance copy drops the `!!!` preliminary marker.** *Where:* `source/renderer/app/i18n/locales/ja-JP.json`. *Severity:* minor (convention/process nit). Per shared-design-tokens §9 the preliminary translations should carry `!!!` until the final copy pass; the locked status-badge labels are correctly exempt. *Action:* either prefix non-locked ja-JP strings with `!!!`, or amend §9 to exempt ja-JP placeholders.

15. **Source-label i18n key diverges from the design's documented detail-view key.** *Where:* `DRepSourceLabel.tsx` uses `governance.drepDirectory.source.onChain`; §9 lists `governance.drepDetail.sourceLabel.onchain`. *Severity:* minor (vocab/namespace drift; not a broken reference today). *Action:* reconcile the namespace before slice-4 lands its detail-view source labels, or document the shipped key in §9 so slice-4 reuses it.

16. **FP-9 regenerated the repo-root `translations/messages.json` globally (expected, but watch at commit time).** *Where:* `translations/messages.json` (outside the `source/` tree). *Severity:* advisory (working-tree hygiene). FP-9's fix ran the canonical `yarn i18n:extract` (`formatjs extract 'source/**/*.{ts,tsx}' …`), which rewrites the entire extracted catalog. **Verified:** the working-tree file is now byte-identical to a fresh canonical extract, contains the intended `governance.drepDirectory.status.inactive` rename, and incidentally **removed a pre-existing *duplicate* `mithrilPartialSync*` block** (those messages are defined once in `MithrilPartialSyncRecommendation.tsx`; the committed catalog had them twice). This is a correct canonical regeneration, not collateral loss — but the diff touches non-governance entries (mithril dedup + scan-order reordering), so a reviewer staging the branch should expect it. *Action:* none required; leave the file canonical (reverting would re-introduce the duplicate and diverge from `yarn i18n:extract`).

---

## Manual Verification — Results (2026-06-16 live preprod run)

Executed manually against a synced preprod node (Daedalus 11.0.0#dev, sync 100%, epoch 295). Evidence: user screenshots of the working DRep Directory + Daedalus diagnostics, and a real `drep-state` capture run against the live node socket.

### FP-11 — cardano-node 11 / UTxO-HD backend — ✅ DONE (all sub-items resolved)

| Sub-item | Result |
|---|---|
| 1. Exact versions | **`cardano-cli 11.0.0.0`** (git rev `97036a66bcf8c89f687ae57a048eecc0389977ef`) and **`cardano-node 11.0.1`** (same git rev), as used by the **running** preprod app bundle (`/nix/store/gandzyj3…-daedalus-cardano-bridge/bin`). Diagnostics screen corroborates node `11.0.1-97036a66b`. Note the **cli is 11.0.0.0, not 11.0.1** — record both exact strings. |
| 2. Active UTxO-HD backend | **`V2InMemory`** — read directly from the running node config (`LedgerDB.Backend: "V2InMemory"`, `QueryBatchSize: 100000`, `NumOfDiskSnapshots: 2`). **Not** OnDisk/LSM. The plan's "in-memory snapshot" framing is therefore **exact**, and the `liburing`/`snappy-c`/`protobuf-compiler` LSM runtime-dep concern is **not applicable** to this build. |
| 3. Real query parses cleanly | ✅ Ran `latest query drep-state --all-dreps --include-stake --output-json --testnet-magic 1` against the live socket — exit 0, no stderr, valid JSON, **258 DReps**. `latest query tip` also clean (epoch 295, era Conway). |
| 4. Fixture captured | ✅ Saved to the Daedalus state dir and copied to [research/drep-state-preprod-epoch295-sample.json](../research/drep-state-preprod-epoch295-sample.json) (90 KB). Profiled against the parser (see below). Tracked under task-166 (status → partial). |
| 5. >4096-item LSM block bug | **Moot.** Backend is InMemory (not LSM) and the directory has only 258 entries — well under 4096. No sync-stall risk on this build. |

**Real-data validation of the parser (`_parseDRepState` / `_parseAnchor` / `_credentialToDRepId`):** the 258-entry capture exercises every branch — both credential shapes (`keyHash` ×212, `scriptHash` ×46), the active/inactive split at epoch 295 (**68 active / 190 inactive**, matching the screenshots' badge mix), nullable stake (**83 null → "—"**, 175 with stake), and anchors present on 116 entries using the `dataHash` + `url` key shape that `_parseAnchor` already falls back to. Max stake ≈ 400.5 T lovelace (within `Number.MAX_SAFE_INTEGER`, but json-bigint `storeAsString` keeps it exact). This is the empirical CLI-grammar validation the unit-level mocks structurally cannot provide (standing risk #1).

> **Real-data display nuance (design confirmation needed, not a bug):** at least one DRep has `stake: 0`. The code maps absent stake to `votingPower: null` → `—`, but `stake: 0` to `votingPower: "0"`. Because a `BigNumber(0)` is truthy, `formatVotingPower`'s `if (!value) return '—'` does **not** catch it, so a real zero-stake DRep renders **`₳ 0`** (registered, zero voting power) — distinct from `—` (no stake data). Confirm this is the intended display before calling the card display logic fully validated.

### FP-8 — Live preprod smoke — 🟡 PARTIAL

| Step | Status |
|---|---|
| 1. Directory loads | ✅ **Confirmed** (screenshot). The list paints with DRep IDs, voting-power values (`₳ 16.5K` etc. and `—`), active/inactive badges, on-chain source labels, and pagination (Page 1 of 11). |
| 2. Refresh reloads | ⏳ **Pending (interactive).** The Refresh button + "Last updated a minute ago" are visible, but a reload-on-click is a behavior a static screenshot can't confirm. |
| 4. No duplicate-push console warning (FP-3) | ⏳ **Pending (interactive console check).** *This is NOT an error-state test* — it is a normal-path console assertion while switching sub-tabs, and is **not** covered by the deferred error-path bucket below. |
| 3 & 5. Actionable error on node-down / no empty `{}` error objects (FP-2) | ⛔ **Deferred by user decision** — error states are postponed to later in the DRep Discovery manual-testing process. |
| 6. `stake: 0` renders `₳ 0` (not `—`) — *design confirmation* | ⏳ **Pending (design decision).** Confirm in the live UI that a real zero-stake DRep should show `₳ 0` (registered, zero voting power) vs `—` (no stake data). See the display-nuance callout under FP-11. Not a bug — an intended-display question. |

> **Reproducible capture procedure** (answers "where do I run `cardano-cli`?" — it is not on the bare WSL PATH; it ships inside the app's Nix bundle). Two ways:
>
> **A — inside the flake shell (simplest):**
> ```bash
> yarn nix:preprod                 # cardano-cli/cardano-node are on PATH here
> export CARDANO_NODE_SOCKET_PATH=~/.local/share/Daedalus/preprod/cardano-node.socket
> cardano-cli latest query drep-state --all-dreps --include-stake --output-json --testnet-magic 1 \
>   > ~/.local/share/Daedalus/preprod/drep-state-fixture.json
> ```
>
> **B — using the running app's bundled binary directly (no shell needed; the app must be running so the node socket is live):**
> ```bash
> CLI=$(readlink -f /proc/$(pgrep -f 'cardano-node run' | head -1)/exe | sed 's,/cardano-node$,/cardano-cli,')
> export CARDANO_NODE_SOCKET_PATH=~/.local/share/Daedalus/preprod/cardano-node.socket
> "$CLI" latest query drep-state --all-dreps --include-stake --output-json --testnet-magic 1 \
>   > ~/.local/share/Daedalus/preprod/drep-state-fixture.json
> ```
> `--testnet-magic 1` = preprod (mainnet uses `--mainnet`). This is the exact argv the app's `GovernanceQueryService` builds, with the network flag appended after the subcommand (the FP-1 fix) and the socket supplied via env, never argv.

> The node-11 backend assessment (UTxO-HD moves only the UTxO set to disk; `drep-state`/stake distribution are served from in-memory ledger tables, so `drep-state --include-stake` is unaffected by the OnDisk backend) is consolidated in [research note §7](../research/slice-1-final-pass-findings.md) — and is now **empirically moot** for this build since the active backend is `V2InMemory`.

### Persisting the directory snapshot (user follow-up → task-168)

Per a 2026-06-16 grilling decision, the captured fixture is kept as a **local support/manual-testing artifact** (state dir + the research copy above), and a snapshot-log feature is **tracked, not yet implemented** (slice-1 stays code-frozen). The design is idiomatic: mirror `logStateSnapshot` in [setupLogging.ts](../../../../source/main/utils/setupLogging.ts) to write the latest successful payload to `Logs/pub/DRep-state-snapshot.json` on each successful `fetchDRepList()`, and register the filename in `ALLOWED_LOGS` ([config.ts](../../../../source/main/config.ts)) so support log bundles include it. **Sanitization boundary:** this snapshot is **public on-chain directory data**, not a vote target — it must **bypass** `filterLogData` (which would strip every `drepId`) and must **never** include the user's own delegation/vote. The snapshot also doubles as an on-chain anchor-**pointer** cache (`url` + `dataHash` per DRep) to seed/cross-check slice-4's metadata fetch — but is **not** a substitute for fetching the off-chain JSON-LD and hash-verifying it (CIP-100/119), which stays a slice-4 concern. Captured as **task-168** (ux-refinement phase).
