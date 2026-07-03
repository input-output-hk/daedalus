# task-ux-703 — PR #3337 comment checklist

Per DD-703-8: task-ux-703 makes **no GitHub writes**. This checklist gives the author, for every
review thread, the original comment, a detailed response, and a brief response to paste when
replying and resolving manually. Replace `<commit>` with the task-ux-703 commit SHA once it exists.
Thread ids T1–T32 match `task-ux-703.md`'s traceability table. All threads were unresolved as of
2026-07-03; T1 and T2 are marked *outdated* by GitHub (code moved; findings persisted).

Reviewers: `david-profrontsolutions` (self multi-agent review), `Copilot`, `AndrewWestberg`
(external human — answer T2/T31/T32 first).

---

## Priority replies (external reviewer)

### T2 — `nix/internal/launcher-config.nix:~410` · AndrewWestberg (reply on author question) · comments 3513439139 / 3514407458 · design question

- [ ] replied · [ ] resolved

**Comment:** Author asked whether the threshold constant is still required now that behind-ness uses
the network tip, and whether there's a better place/structure for it. AndrewWestberg replied that a
percentage-behind-tip is not a useful metric (the denominator grows forever) and proposed "X blocks
from tip" — roughly, allow partial sync when more than ~30 minutes from tip.

**Detailed response:** Agreed on the unit argument — and the shipped code already agrees: the
constant is `mithrilPartialSyncThresholdImmutables = 20`, an absolute measure (one immutable chunk
≈ 6 h of chain, so 20 ≈ 5 days), not a percentage; the percentage framing was stale. On "allow at
~30 minutes": task-ux-703 splits the two surfaces this constant was conflating. (1) The **manual
offer** — the Diagnostics "Mithril Sync" section and its trigger — now renders whenever the feature
flag is on, with no behind-ness gate at all, so a user any distance behind tip can start a Mithril
Sync deliberately. (2) The **proactive prompt** keeps the conservative ~5-day gate, so we don't
push a full snapshot restore at users whose ordinary sync would catch up in minutes. The constant
stays in launcher config (backend-owned threshold, per the sprint's locked boundary that the
renderer computes no thresholds) and is flagged "calibrate in QA" — happy to revisit the number
with real data.

**Brief response:** The constant is already absolute (immutable chunks ≈ 6 h each; 20 ≈ 5 days),
not a percentage. In `<commit>` the manual trigger in Diagnostics becomes available whenever the
feature is enabled (no behind-ness gate), and the constant only gates the proactive prompt —
keeping that conservative while letting mildly-behind users opt in manually. Number stays
QA-calibratable in launcher config.

### T31 — `source/renderer/app/stores/MithrilPartialSyncStore.ts:413` · AndrewWestberg · comment 3514615187 · duplicate of T12

- [ ] replied · [ ] resolved

**Comment:** Asks to avoid swallowing start rejections when the status resync returns a non-error
state: if main rejects before publishing `failed`, `syncStatus()` restores `idle`, and since
`mithrilAttemptStartedThisSession` was already set, the prompt disappears with no visible error or
retry path. Suggests only swallowing once the backend has published an error/terminal state, and
rethrowing + resetting the session attempt guard when the resynced status is `idle`.

**Detailed response:** Implemented exactly as suggested in `<commit>`: `startPartialSync` now
rethrows any start rejection after the resync unless the resynced status is a backend-published
error/terminal state (which then owns the error surface); when the resynced status is `idle` it
also resets `mithrilAttemptStartedThisSession` so the proactive prompt re-offers. Preflight
rejections now surface in both the prompt inline-confirm and the Diagnostics confirmation, with
unit tests for each rejection path.

**Brief response:** Fixed in `<commit>` per your suggestion — rejections rethrow unless the backend
published an error/terminal status, and the session attempt guard resets when the resync lands on
`idle`, so the prompt re-offers and errors are visible.

### T32 — `source/renderer/app/components/loading/syncing-connecting/SyncingConnectingMithrilPrompt.tsx:22` · AndrewWestberg · comment 3514615192 · duplicate of T20

- [ ] replied · [ ] resolved

**Comment:** `computeBehindByEpochs` can return 1 but the string always renders plural
("1 epochs behind"); the Diagnostics confirmation has the same pattern. Asks for ICU plural
(`{epochs, plural, one {# epoch} other {# epochs}}`) on both surfaces plus locale/spec updates.

**Detailed response:** Done in `<commit>`: both the syncing-screen prompt and
`MithrilPartialSyncConfirmation` now use `{epochs, plural, one {# epoch} other {# epochs}}`;
`en-US`/`ja-JP` locale entries and the copy-asserting specs are regenerated (JA collapses to the
`other` form, per Japanese plural rules).

**Brief response:** Fixed in `<commit>` — ICU plural on both surfaces, locales and specs updated.

---

## CAT-A — Cross-session recovery & startup correctness

### T5 — `source/main/mithril/MithrilPartialSyncService.ts:822` · self-review · comment 3514323643 · **CRITICAL**

- [ ] replied · [ ] resolved

**Comment:** On a cross-session `starting-node` failure the `failed` snapshot with
`allowedRecoveryActions: ['wipe-and-full-sync']` is emitted only via `MithrilController` while the
service's `_status` stays `idle`/`[]` — so every click of the sole recovery button throws in
`_assertRecoveryActionAllowed`; the user is permanently stuck.

**Detailed response:** Fixed in `<commit>`. The recovery-action IPC wirings now seed the service's
status from the last broadcast snapshot (`getMithrilPartialSyncStatus()`) via a new
`adoptRecoverySnapshot` before delegating — so `_assertRecoveryActionAllowed` validates against
the same `allowedRecoveryActions` the user is shown, including the cross-session snapshots emitted
from `startInstalledNode`. Regression spec reproduces the `installed-awaiting-node-start` failure
and asserts `wipeAndFullSync` succeeds.

**Brief response:** Fixed in `<commit>` — recovery actions seed service status from the broadcast
snapshot, so the wipe button validates against what the user sees. Repro spec added.

### T6 — `source/main/mithril/mithrilPartialSyncNodeStartup.ts:67` · self-review · comment 3514323650 · major

- [ ] replied · [ ] resolved

**Comment:** Unguarded `await fs.remove(marker.stagingRootPath)` in the node-start-verified reclaim
branch runs before `clearMithrilPartialSyncMarker()`; a Windows EBUSY/EPERM converts to a
managed-chain-layout error and Daedalus refuses to start a verified-good node, repeating every boot.

**Detailed response:** Fixed in `<commit>`: the staging removal is now try/caught (warn + continue)
and the marker is cleared regardless, so a locked staging dir can never block startup on a verified
DB. Unit test simulates an EBUSY rejection and asserts node start proceeds and the marker clears.

**Brief response:** Fixed in `<commit>` — reclaim removal is best-effort; marker always clears;
EBUSY spec added.

### T8 — `source/main/mithril/mithrilPartialSyncNodeStartup.ts:77` · self-review · comment 3514323659 · major

- [ ] replied · [ ] resolved

**Comment:** The interrupted-cutover "Quit" dialog branch neither calls `app.quit()` nor emits any
status; the gate goes `partialSyncCutoverUnsafe`, the node never starts, and the renderer sits on
the connecting screen with no recovery option.

**Detailed response:** Fixed in `<commit>`: the Quit branch now exits via `safeExitWithCode`
instead of dead-ending. On re-verification the other branch was already sound — it wipes and
returns `false`, resuming a healthy full-sync boot — so it's unchanged (re-emitting `failed` there
would show a false failure overlay). Both branches unit-tested.

**Brief response:** Fixed in `<commit>` — Quit actually quits (`safeExitWithCode`); the continue
branch was verified already correct and is unchanged.

### T9 — `source/main/mithril/MithrilPartialSyncService.ts:438` (and `:421`) · self-review · comment 3514323664 · major

- [ ] replied · [ ] resolved

**Comment:** `allowedRecoveryActions` was narrowed to `['retry','restart-normal']` on both the
cancelled-success and failed-cancel-cleanup paths; with a Windows file lock on the staging dir,
both remaining actions loop on the same `fs.remove` failure — the wipe escape hatch is gone.

**Detailed response:** The narrowing itself is a recorded decision (ADR D-702a-2: cancel is
pre-cutover only, the chain DB is intact, so wipe is destructive and unnecessary there) and stands
— restoring wipe was also unlikely to help, since the wipe path would attempt the same locked
staging removal. The trap is fixed differently in `<commit>` (DD-703-9): cancel-path staging
removal is now best-effort — on removal failure the flow still lands on `cancelled`, so
"Restart Node Sync" proceeds on the intact DB and the orphaned staging dir is reclaimed at the next
partial-sync start (its staging prepare removes the staging root; cancel is pre-cutover, so no
marker exists for startup reclaim to see). Specs cover the locked-staging case.

**Brief response:** Wipe stays off per ADR D-702a-2 (pre-cutover DB is intact; wipe would hit the
same lock). The trap itself is fixed in `<commit>` — cleanup is best-effort, restart-normal always
proceeds, leftover staging is reclaimed at the next partial-sync start.

### T10 — `source/main/mithril/MithrilPartialSyncService.ts:861` · self-review · comment 3514323669 · major

- [ ] replied · [ ] resolved

**Comment:** `_getStagingRootPath()` is session-local; `wipeAndFullSync`, `restartNormal`,
`_cleanupPartialSyncArtifacts` and the startup cutover-in-progress wipe never use
`marker.stagingRootPath` — with a custom chain dir on another volume, a crash after the cutover
marker leaves a multi-GB staged snapshot permanently orphaned.

**Detailed response:** Fixed in `<commit>`: every cleanup/recovery path now resolves the staging
root from the marker when one exists, falling back to the session-local resolver only when it
doesn't; the startup cutover wipe removes `marker.stagingRootPath`. Unit test uses a custom-volume
marker path and asserts wipe removes it.

**Brief response:** Fixed in `<commit>` — marker-first staging resolution across all
cleanup/recovery paths; custom-volume spec added.

### T14 — `source/main/mithril/mithrilCommandRunner.ts:162` · self-review · comment 3514323691 · minor

- [ ] replied · [ ] resolved

**Comment:** Detached POSIX spawn removes the guarantee that mithril-client dies with Daedalus on
group-delivered signals; on crash/OOM the child keeps downloading a multi-GB snapshot. Suggests not
detaching unless required, or persisting the child PID for startup reaping.

**Detailed response:** Answered without a code change (DD-703-6 as revised): `detached` is
load-bearing here. `killProcessTree` performs the POSIX kill as `process.kill(-pid, signal)` — a
process-group kill that requires the child to be a group leader, which is exactly what `detached`
provides (the dependency is documented in `killProcessTree`'s docstring); the conversion path also
spawns helper binaries that group-kill reaps. Removing `detached` would degrade every POSIX
cancel/force-kill/shutdown-reap to a fallback single-process kill without fixing the stated
problem: on POSIX, children survive a parent crash regardless of `detached` (they reparent to
init). Graceful shutdowns already reap the child via the shutdown hook. If hard-crash orphaning
ever becomes a practical concern, the recorded follow-up is a persisted-PID startup reaper.

**Brief response:** Intentionally detached — `killProcessTree`'s group kill (`kill(-pid)`) depends
on the child being a group leader, and `detached` doesn't cause crash-orphaning on POSIX (children
reparent to init either way). No change; persisted-PID reaper recorded as a future option.

---

## CAT-B — Start flow & availability

### T11 — `source/renderer/app/components/status/MithrilPartialSyncSection.tsx:130` · self-review · comment 3514323677 · major

- [ ] replied · [ ] resolved

**Comment:** The Diagnostics section returns `null` unless `enabled && isSignificantlyBehind`, and
probe failures degrade to `isSignificantlyBehind: false` — a user weeks behind with an unreachable
aggregator loses the manual trigger for at least the 5-minute cache TTL.

**Detailed response:** Fixed in `<commit>` (design decision DD-703-2): the section renders whenever
the feature is enabled. Copy adapts to probe state — significantly behind → recommendation; near
tip → informational; probe failed → "availability unknown" (the availability payload now carries an
explicit probe-failed flag instead of collapsing failures into "not behind") — and the manual
trigger stays enabled in all three. This also addresses the availability half of the threshold
discussion (see T2 reply).

**Brief response:** Fixed in `<commit>` — section always renders when the feature is enabled, with
state-adaptive copy; the manual trigger is never hidden by behind-ness or probe failures.

### T12 — `source/renderer/app/stores/MithrilPartialSyncStore.ts:413` · self-review · comment 3514323684 · major

- [ ] replied · [ ] resolved

**Comment:** `startPartialSync` rethrows only when status is still the renderer-optimistic
`'stopping-node'` — which main never emits — so every preflight rejection resyncs to `idle` and is
swallowed; error displays are unreachable and `mithrilAttemptStartedThisSession` (never reset)
suppresses the prompt for the rest of the session.

**Detailed response:** Fixed in `<commit>` together with the duplicate report (T31, same line):
rejections rethrow unless the resynced status is a backend-published error/terminal state; on
`idle` resync the session flag resets so the prompt re-offers. Unit tests cover each preflight
rejection surface.

**Brief response:** Fixed in `<commit>` (with the duplicate thread) — rejections surface, session
flag resets on idle resync, prompt re-offers.

### T15 — `source/renderer/app/stores/MithrilPartialSyncStore.ts:438` · self-review · comment 3514323692 · minor

- [ ] replied · [ ] resolved

**Comment:** `cancelPartialSync` rethrows after finally-resync, and `restartNormally` /
`wipeAndFullSync` are bare awaits, all wired to Button `onClick` — main-process rejections become
unhandled promise rejections and silent button no-ops.

**Detailed response:** Fixed in `<commit>`: all three actions catch rejections, resync status, and
surface the failure through the overlay's error state (code-keyed copy, consistent with the error
i18n rework in this wave). No Mithril button can produce an unhandled rejection; each action has a
rejecting-IPC spec.

**Brief response:** Fixed in `<commit>` — recovery actions catch, resync, and surface errors in the
overlay; unhandled-rejection specs added.

### T21 — `source/main/mithril/MithrilPartialSyncService.ts:965` · self-review · comment 3514323721 · minor

- [ ] replied · [ ] resolved

**Comment:** Behind-ness caches survive a chain-directory change — `_invalidateBehindnessCaches` is
only called at start/cancel/reset, and directory-change notifications only reach the controller's
startup gate; up to 5 minutes of probes serve the old directory's immutable count.

**Detailed response:** Fixed in `<commit>`: the directory-change notification now also invalidates
the service's behind-ness caches (wired through the controller's directory-change handler). Unit
test: after a directory change the next probe re-reads instead of serving cache.

**Brief response:** Fixed in `<commit>` — behind-ness caches invalidate on chain-directory change.

### T28 — `source/main/mithril/MithrilPartialSyncService.ts:983` · self-review · comment 3514323756 · minor

- [ ] replied · [ ] resolved

**Comment:** `getPartialSyncBehindness` awaits the aggregator metadata fetch and the local immutable
readdir sequentially though they're independent; cold-cache serial time can exceed the renderer's
10 s availability timeout.

**Detailed response:** Fixed in `<commit>`: the two reads run under `Promise.all`.

**Brief response:** Fixed in `<commit>` — parallelized with `Promise.all`.

---

## CAT-C — Completed-overlay finalize

### T13 — `source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.tsx:85` · self-review · comment 3514323688 · major

- [ ] replied · [ ] resolved

**Comment:** The completed overlay lost its "Continue to Daedalus" button in favor of a one-shot
4-second auto-finalize that never retries; if finalize fails while status stays `completed`, the
effect never re-fires and the overlay has no interactive element — only exit is force-quit.

**Detailed response:** The auto-finalize itself is a recorded decision (ADR D-702a-1, amending
locked #16/D9), so the button stays gone — but the failure path is fixed in `<commit>`: on finalize
rejection the overlay retries once, and on a second rejection it renders the existing
`MithrilErrorView` with a `retry` recovery action that re-invokes finalize. The silent
`.catch(() => {})` is gone; specs cover fail-twice → recoverable error view and fail-then-succeed →
normal dismiss.

**Brief response:** Auto-finalize stays per ADR D-702a-1; fixed the dead-end in `<commit>` — one
retry, then the error view with a working retry action. No more swallowed rejection.

---

## CAT-D — Disk preflight

### T7 — `source/main/mithril/MithrilPartialSyncService.ts:1102` · self-review · comment 3514323652 · major

- [ ] replied · [ ] resolved

**Comment:** The preflight requires `max(snapshot.size × 1.2, 4 GB)` using the whole-chain snapshot
size even when the partial range is a few hundred MB — false-blocking exactly the target users
whose disk is mostly full of the existing chain.

**Detailed response:** Fixed in `<commit>` with a delta-based estimate (design decision DD-703-5):
`requiredBytes = max(snapshotTotalUncompressed − chainDirSize, 0) + 0.2 × snapshotTotalUncompressed`,
floored at the existing 4 GB constant; if the chain-dir size can't be measured the old
whole-snapshot bound applies. The margin is deliberately proportional to the full snapshot (covers
chain growth and conversion slack), while the base term shrinks to roughly the data actually
missing. Unit tests cover near-tip, measurement-failure, and large-delta cases.

**Brief response:** Fixed in `<commit>` — preflight now estimates the missing range
(snapshot − chain dir) plus a 20 %-of-snapshot margin, floored at 4 GB; whole-snapshot bound only
as measurement-failure fallback.

### T18 — `source/common/types/mithril-partial-sync.types.ts:33` · self-review · comment 3514323704 · minor

- [ ] replied · [ ] resolved

**Comment:** The service throws `PARTIAL_SYNC_INSUFFICIENT_DISK_SPACE` but the shared error-code
union omits it, so the renderer copy map can't key on it — users get generic failure copy instead
of disk-space guidance.

**Detailed response:** Fixed in `<commit>`: the code joins `MithrilPartialSyncErrorCode` and
`COPY_BY_CODE` gains dedicated disk-space copy (needed vs available, freeing space; EN+JA).

**Brief response:** Fixed in `<commit>` — code added to the union with dedicated disk-space copy.

---

## CAT-E — i18n / copy / vocabulary

### T1 — `source/main/mithril/MithrilPartialSyncService.ts:1203` · self-review · comment 3513419359 · minor (outdated thread)

- [ ] replied · [ ] resolved

**Comment:** Progress rows show raw stage ids (`verifying`/`converting` lowercase, `installing`
flashes) because `_upsertProgressItem(id, id, 'active')` emits label = id and the renderer map only
knows bootstrap ids.

**Detailed response:** Fixed in `<commit>` on the renderer side (single source of label truth):
`ITEM_ID_TO_MESSAGE` (and the sub-id sets) now cover the real partial-sync stage ids with proper
intl messages (EN+JA); spec fixtures feed the ids the service actually emits. The service keeps
emitting stable ids, per the locked boundary that raw backend strings never become UI copy.

**Brief response:** Fixed in `<commit>` — renderer now maps `verifying`/`converting`/`installing`
to translated labels; fixtures use real service ids. (Same fix as the step-indicator thread.)

### T17 — `source/renderer/app/components/loading/mithril-bootstrap/MithrilStepIndicator.tsx:49` · self-review · comment 3514323700 · minor

- [ ] replied · [ ] resolved

**Comment:** Renderer-side twin of the above: `ITEM_ID_TO_MESSAGE` / `DOWNLOAD_SUB_IDS` /
`FINALIZE_SUB_IDS` only know bootstrap vocabulary; specs mask it with bootstrap-style ids the
partial-sync service never produces.

**Detailed response:** Fixed in `<commit>` — see T1: mappings + sub-id membership added for
`verifying`, `converting`, `installing` with EN+JA messages; specs now assert against real ids.

**Brief response:** Fixed in `<commit>` together with T1 — mappings and honest fixtures added.

### T4 — `source/renderer/app/components/status/MithrilSyncProcessSummary.messages.ts:18` · Copilot · comment 3513613266 · minor

- [ ] replied · [ ] resolved

**Comment:** "shutdown" (noun) used where the verb phrase "shut down" is needed; the string is
shared across several surfaces, so translations/tests must be regenerated.

**Detailed response:** Fixed in `<commit>`: message now reads "shut down"; locale files regenerated
and copy-asserting specs updated.

**Brief response:** Fixed in `<commit>` — "shut down", locales and specs regenerated.

### T16 — `source/main/mithril/MithrilPartialSyncService.ts:255` (also `:671-673`, `:1123`, `:567`) · self-review · comment 3514323695 · minor

- [ ] replied · [ ] resolved

**Comment:** Service-thrown user-facing messages are hardcoded English using banned "partial sync"
vocabulary and bypass i18n — ja-JP users see raw English.

**Detailed response:** Fixed in `<commit>`: service throws now carry stable error codes at every
user-reachable throw site — the sweep also covered sites beyond the ones flagged here (metadata
resolve, certified-immutable resolution, and the `_buildError` fallback, all of which land verbatim
in the error view via `status.error`). The union is extended (e.g. disabled / already-running /
layout-unsupported) and the renderer maps codes to intl copy in the error-copy module; the English
prose moved into log lines only.

**Brief response:** Fixed in `<commit>` — error codes over prose at all user-reachable throw sites;
renderer owns the words via i18n.

### T19 — `source/renderer/app/components/loading/mithril-bootstrap/MithrilErrorView.tsx:130` (also `:105`, `:159`) · self-review · comment 3514323709 · minor

- [ ] replied · [ ] resolved

**Comment:** Overlay body renders `error?.message` verbatim, so service-thrown English reaches
users untranslated; raw messages belong in collapsed details/logs at most.

**Detailed response:** Fixed in `<commit>`: body copy comes exclusively from the code/stage-keyed
intl maps; the raw message appears only inside the collapsed details section.

**Brief response:** Fixed in `<commit>` — intl-keyed body copy; raw message demoted to details.

### T20 — `source/renderer/app/components/loading/syncing-connecting/SyncingConnectingMithrilPrompt.tsx:22` · self-review · comment 3514323714 · minor

- [ ] replied · [ ] resolved

**Comment:** "Your node is about {epochs} epochs behind." has no ICU plural and
`computeBehindByEpochs` returns exactly 1 in the common near-tip case — both the prompt and the
Diagnostics confirmation render "about 1 epochs behind".

**Detailed response / Brief response:** Same fix as T32 (duplicate reporter) — see the T32 entry
above. Fixed in `<commit>`.

### T22 — `source/renderer/app/components/status/MithrilPartialSyncSection.tsx:115` (fallback at `:107`) · self-review · comment 3514323728 · minor

- [ ] replied · [ ] resolved

**Comment:** Hardcoded English fallback "Unable to start Mithril partial sync." — not react-intl,
banned vocabulary, drifted from the sibling fallback in the prompt component.

**Detailed response:** Fixed in `<commit>`: the fallback is an intl message with approved
vocabulary ("Unable to start Mithril Sync.") and all three fallback sites share one extraction
helper (see the duplication thread T23), eliminating the drift.

**Brief response:** Fixed in `<commit>` — intl fallback, approved vocabulary, single shared helper.

---

## CAT-F — Structure / duplication / dead code

### T3 — `source/renderer/app/components/loading/mithril-bootstrap/MithrilBootstrap.scss:13` · Copilot · comment 3513613222 · minor

- [ ] replied · [ ] resolved

**Comment:** `.backdrop` applies the `overlay-backrop` blur mixin but the backdrop is now fully
opaque — the blur is invisible while still costing GPU time.

**Detailed response:** Fixed in `<commit>`: mixin removed from the opaque `.backdrop` (the opaque
look shipped deliberately in the 702 wave; the blur was vestigial).

**Brief response:** Fixed in `<commit>` — dead blur removed.

### T23 — `source/renderer/app/components/loading/syncing-connecting/SyncingConnectingMithrilPrompt.tsx:151` · self-review · comment 3514323733 · minor

- [ ] replied · [ ] resolved

**Comment:** `error instanceof Error ? error.message : fallback` implemented three times with
divergent fallback copy.

**Detailed response:** Fixed in `<commit>`: one shared helper with a single intl-backed fallback,
used at all three sites (prompt, Diagnostics section, store).

**Brief response:** Fixed in `<commit>` — shared extraction helper, one fallback.

### T24 — `source/common/types/mithril-partial-sync.types.ts:123` · self-review · comment 3514323739 · minor

- [ ] replied · [ ] resolved

**Comment:** `isMithrilPartialSyncBlockingNodeStart` inlines an array byte-identical to
`MITHRIL_PARTIAL_SYNC_WORKING_STATUSES`; a missed update could silently stop blocking node start
during cutover.

**Detailed response:** Fixed in `<commit>`: delegates to `isMithrilPartialSyncWorkingStatus`.

**Brief response:** Fixed in `<commit>` — single source of truth for working statuses.

### T25 — `source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.tsx:42` · self-review · comment 3514323744 · minor

- [ ] replied · [ ] resolved

**Comment:** `PROGRESS_STATUSES` hand-copies the nine working statuses plus `completed`; an
upstream status addition would fall through to the error view for a healthy state.

**Detailed response:** Fixed in `<commit>`:
`isMithrilPartialSyncWorkingStatus(status) || status === 'completed'` replaces the copy.

**Brief response:** Fixed in `<commit>` — derived from the shared predicate.

### T26 — `source/main/mithril/mithrilCommandRunner.ts:258` · self-review · comment 3514323747 · minor

- [ ] replied · [ ] resolved

**Comment:** ~15-line spawn block duplicated verbatim between `runBinary` and `runCommand`;
one-sided edits would give the two children different process-group behavior.

**Detailed response:** Fixed in `<commit>`: one shared spawn helper used by both, with spawn
options — including the intentional `detached` (see the T14 reply) — defined in exactly one place,
which also resolves this comment's one-sided-edit concern.

**Brief response:** Fixed in `<commit>` — shared spawn helper; both children get identical
lifecycle semantics.

### T27 — `source/main/mithril/MithrilPartialSyncService.ts:1011` · self-review · comment 3514323753 · minor

- [ ] replied · [ ] resolved

**Comment:** Four methods spawn the same two CLI commands with copy-pasted args and JSON parsing,
differing only in the normalizer.

**Detailed response:** Fixed in `<commit>`: `listSnapshots` maps the raw-list result through the
normalizer and the show/list pipelines share one spawn+parse path.

**Brief response:** Fixed in `<commit>` — CLI pipelines deduplicated.

### T29 — `source/renderer/app/stores/MithrilPartialSyncStore.ts:89` · self-review · comment 3514323761 · minor

- [ ] replied · [ ] resolved

**Comment:** `behindByImmutables` observable is written every 30 s refresh but never read — and per
the epochs-only display rule must never be rendered.

**Detailed response:** Fixed in `<commit>`: observable and its write sites removed; the value stays
main-side only.

**Brief response:** Fixed in `<commit>` — dead observable removed.

### T30 — `source/renderer/app/stores/MithrilPartialSyncStore.ts:71` · self-review · comment 3514323765 · minor

- [ ] replied · [ ] resolved

**Comment:** `elapsedSeconds` observable is dead since the `startedAt` refactor; keeping two
elapsed-time representations invites reintroducing the jumpy backend timer.

**Detailed response:** Fixed in `<commit>`: observable and per-frame assignment removed;
`startedAt` is the single elapsed-time source.

**Brief response:** Fixed in `<commit>` — dead observable removed.

---

## Review-body nits (no threads — mention in a single summary comment if desired)

Eight of nine applied in `<commit>`; N1 declined with rationale:

- **N1** `killProcessTree.ts:55` / `CardanoNode.ts:1080` — declined: the two kills are
  intentionally different (`killProcessTree` tree-kills a live `ChildProcess` via a POSIX group
  kill; `CardanoNode._killProcessWithName` single-kills a raw pid — possibly a stored
  previous-session PID with no `ChildProcess` — and polls for death). Unifying would change
  CardanoNode shutdown semantics for a nit.
- **N2** `mithrilSnapshotMetadata.ts:105` — dead lookup paths and bare `['epoch']` fallback removed.
- **N3** `chainStorageCoordinator.ts:75` — fabricated availability shape replaced by
  `isPartialSyncEnabled()`.
- **N4** `MithrilPartialSyncOverlay.tsx:187` — intl-string prop threading replaced by a
  `'bootstrap' | 'partial-sync'` variant flag.
- **N5** idle-status literal ×3 — `makeIdlePartialSyncStatus()` factory.
- **N6** `MithrilPartialSyncService.ts:714` — redundant dynamic import removed.
- **N7** `DaedalusDiagnostics.tsx:723` — redundant enabled re-check removed (absorbed into the T11
  rework).
- **N8** `mithril-partial-sync.types.ts:95` — no-op `'idle'` filter removed.
- **N9** `SNAPSHOT_FILES_TOTAL` duplicated in stories — moved to `_support/mithrilFixtures`.

**Suggested summary comment for the PR:** "All 32 review threads addressed in `<commit>`
(task-ux-703; plan at `.agent/plans/mithril-partial-sync/task-plans-ux-refinement/phase-7/task-ux-703.md`),
plus eight of the nine nit-level cleanups from the review body (N1 declined — the two kill paths
are intentionally different; rationale above). Per-thread replies inline."
