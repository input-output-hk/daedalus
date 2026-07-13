# task-ux-706 — Pre-merge copy standardization + cleanup wave (JA review · code-review comments · extra punch-list)

> Plan-ahead task (not a retrospective). Consolidates the copy and cleanup work that must land on
> `feat/mithril-partial-sync-ux-refinement` before the PR merges, sourced from three inputs: the
> Japanese translator review (2026-07-03), the resolutions to it (2026-07-03), and the "extra"
> punch-list (code-review comments + manual-QA findings). Decisions were locked 2026-07-08 (see the
> Decisions section). Implementation is a separate approved pass — this doc + the per-CAT docs are the
> source of truth for that pass.

- Sprint: Mithril Partial Sync UX Refinement — phase-7
- Branch: `feat/mithril-partial-sync-ux-refinement`
- Planning status: `approved` (decisions locked 2026-07-08/09; plan-review pass complete 2026-07-10 —
  findings resolved per DD-706-9 and folded into this doc + the CAT docs)
- Build status: `completed` (2026-07-12 — all seven CATs landed, verified, code-review approved;
  see the Final outcome section)
- Interaction mode: `guided` — several copy items still need external sign-off (JA translator round 2
  and/or product; see the pending sign-off table) before their strings are final; the plan marks each.
  Everything else is implement-ready.
- Priority: high · Dependencies: task-ux-705 (completed)

## Why (what this wave closes)

The branch is copy-complete but inconsistent. The JA translator flagged terminology drift; it was
resolved by tightening the product vocabulary rather than patching individual strings; code review left
three code/test items; and manual QA surfaced a handful of UX fixes. None of these are new features —
they are the last consistency, correctness, and polish pass before merge.

Four source threads feed this task:

1. **JA translator review** — 4 consistency issues, 1 terminology issue, 2 questions (screenshots +
   the files-vs-size unit question).
2. **The resolutions** — accepted #2/#3/#4/#5 as-is; escalated #1 into a project-wide vocabulary
   decision; answered #6 with the "Ledger state" relabel; #7 surfaced the button-overflow bug.
3. **Extra punch-list** — `nix fmt`; two new diagnostic/prompt copy items; the moving-stage caution;
   the Windows storage-picker separator bug; snapshot-size on partial-sync progress; smoke-test
   coverage for the replay-screen "Use Mithril Sync instead" button; the handoff-note Ctrl/Cmd
   platform substitution; and the three code-review comment fixes (`mithrilSyncInterrupt` extraction,
   a broken service unit test, and the now-dead `PARTIAL_SYNC_NO_CERTIFIED_RANGE` code + i18n).
4. **PR #3337 follow-up review (validated 2026-07-10)** — the second reviewer's comments were checked
   against the branch; the one accepted finding is **N1** (disk-space preflight fails open when the
   snapshot size is unknown or malformed). Folded in as **CAT-F Step F4** under DD-706-8; every other
   comment was verified as already addressed, intentional-and-documented, or a declared non-issue.

## Decisions (locked 2026-07-08)

### DD-706-1 — Three-term sync vocabulary (supersedes the "standard sync" copy decision)

Replaces the earlier "Mithril Sync vs standard sync" naming. Three canonical names; JA renders each in
**established native vocabulary** — simple for Japanese users and consistent with the app's existing
translations. "Mithril" itself stays Latin as a proper noun (matching the existing JA copy, e.g.
「Mithrilを使えば…」):

| Term | JA rendering | Meaning | Replaces |
| --- | --- | --- | --- |
| **Mithril Sync** | 「Mithril同期」 | Fast verified-snapshot restore (partial or bootstrap) | — (already the JA rendering across 30 keys; one outlier button fixed, CAT-A A1) |
| **Blockchain Sync** | 「ブロックチェーン同期」 | Normal block-by-block sync / resume from current tip | "Standard Sync" / 「標準同期」 |
| **Blockchain Sync from Genesis** | 「ジェネシスからのブロックチェーン同期」 (name form; sentences use ジェネシスからブロックチェーン同期を実行…) | Full sync from block 0 (fresh install / after wipe) | "Sync from genesis" / 「ジェネシスから同期」 |

Rationale for the split: a partially-synced node that declines Mithril does **not** sync from genesis —
it resumes from its tip. Calling that "Blockchain Sync" is accurate; reserving "…from Genesis" for the
true block-0 case (bootstrap decline, wipe-and-restart) keeps the copy honest while still giving the
slow path one consistent family name — the JA renderings keep the 「ブロックチェーン同期」 family name
visible in both slow-path terms for the same reason. ブロックチェーン and ジェネシス are the established
katakana in `ja-JP.json` (36 and 5 pre-existing occurrences respectively). Owned by **CAT-A**.

### DD-706-2 — Progress label "Fast sync:" → "Ledger state:" / 「台帳状態:」

The "Fast sync" figure in the bootstrap progress detail is the Mithril **ancillary-files** package
(a byte size) that unpacks into the node's **ledger state**. Relabel it "Ledger state:" (「台帳状態:」);
it stays a byte size. The immutable-chunk side already shows a file count and stays that way. No new
backend metric pipeline in this task. Owned by **CAT-C**. *(JA review ❓#6 — pending round-2 sign-off.)*

### DD-706-3 — Remove dead `PARTIAL_SYNC_NO_CERTIFIED_RANGE`, gated on a reachability check

task-705 (`mithrilPartialSyncPreflight.ts:134-144`) changed `derivePartialSyncRange` to **return**
`{start:N, end:N}` instead of throwing `PARTIAL_SYNC_NO_CERTIFIED_RANGE` when
`localImmutableNumber >= latestCertifiedImmutableNumber`. Grep confirms no remaining throw/emit site in
`source/main`. The code survives only as a type-union member, an error-copy map entry, two message
descriptors + their locale strings, and spec cases. **Remove it** — but CAT-F must first confirm the
"no certified snapshot at all" user situation cannot still reach the overlay via a different path; if it
can, repoint that path to an existing error rather than deleting the copy. Owned by **CAT-F**.

### DD-706-4 — Plumbing the partial-sync snapshot size also fixes the ancillary-only bar jitter

Extra #6 ("add snapshot catch-up size to partial-sync progress if available") and the low-priority
"progress bar jumps when only ancillary files download" are the **same fix**. The jitter comes from the
fallback weighting branch (`MithrilStepIndicator.tsx:106-107,193-196`) used when `snapshotSizeBytes` is
absent — which it always is on the partial-sync overlay. Threading the backend's `_latestSnapshot.size`
into the partial-sync status → store → overlay makes the byte-weighted branch active, which both shows
the size and smooths the bar. Owned by **CAT-C**; verify the smoothing, else adjust the fallback weights.

**❌ Re-planned 2026-07-10 (plan-review R-1) — mechanism unsound as specced; downgraded to the Tier-3
follow-up (option a).** `_latestSnapshot.size` is the **whole-DB** snapshot size, not the catch-up
delta: the byte-weighted branch would compute a ~97-99% snapshot weight (at or above the 95% fallback it
replaces — jitter unchanged), its ancillary term reads an `ancillaryProgress` prop the partial-sync path
never supplies (pinned to 0), and the "≈ {totalSize} total" line would render the full snapshot size
(e.g. ~150 GB) on a 2-chunk catch-up — the opposite of Extra #6's intent. **Decision (DD-706-9):** ship
C1 (relabel) + C3 (unit sanity) in this wave; the size line + jitter fix move to the Tier-3 post-merge
follow-up with the corrected root cause recorded — the missing **ranged-delta** byte size (doesn't exist
in today's metadata; must be derived) and the missing ancillary-percent feed, **not** the fallback
weights. `hasKnownSnapshotSize` (S7) still lands with F4; the follow-up imports the same predicate.
CAT-C's doc preserves the mechanically-verified recon for the follow-up implementer.

### DD-706-5 — Scope, structure, deliverable

One task-706, organized like task-704: this thin master + one plan doc per CAT. Priority-tiered (below);
medium/low items may spill to a post-merge follow-up if they balloon, but are logged, never silently
dropped. Deliverable of this planning pass is the plan docs; implementation follows on approval.

### DD-706-6 — "At/past the latest snapshot" is its own state, not the near-tip state

Sharpens the two loose "diagnostic tooltip / prompt" extra items (Extra #2/#3). The intent is specific:
when the node's tip is **at or past the latest certified snapshot** (backend `gap <= 0`, i.e. no epochs
left to catch up), the user must be told they are **ahead of the latest snapshot**, that they **do not
need Mithril Sync**, and that **Blockchain Sync will complete the remaining block sync** on its own.

This is not a wording tweak to the existing near-tip copy — it is a **state that is not distinguishable
in the UI today**. `getPartialSyncBehindness` (`MithrilPartialSyncService.ts:970-976`) omits
`behindByImmutables` on the `gap <= 0` branch, and the store (`MithrilPartialSyncStore.ts:255-257`)
never captures it, so both `gap <= 0` (at/past snapshot) and `0 < gap < threshold` (a few chunks behind,
where Mithril still helps) collapse into the single `near-tip` variant. Rewording that one variant would
make a **false** claim for the 1–19-immutables-behind window. CAT-D must **split the variant** on the
`gap <= 0` signal, thread it end-to-end, and give the at/past-snapshot case its own copy on both the DD
tooltip and the confirmation prompt. Owned by **CAT-D** (needs sign-off).

**✅ Resolved 2026-07-09 — the `gap <= 0` state overlaps task-705's target; copy hedges as an offer.**
The immutable gap is *not* proof of a healthy node. `gap <= 0` is the exact condition task-705 WI-3 was
built for: a node with recent immutables but **missing/corrupted/incompatible ledger state** also sits at
`gap <= 0`, `isSignificantlyBehind` stays false, and it drops into a slow from-genesis **ledger replay**
that Mithril's ledger-only restore is meant to short-circuit. The probe cannot tell "healthy & ahead"
from "stuck replaying" — both are `gap <= 0` — and this user can reach the copy: DD opens from the replay
screen via the status icon (`SyncingConnectingPage.tsx:68`).

A third option (c) — auto-detect the stuck case and split the copy on a real replay signal — was
**investigated and rejected**: the only candidate, `blockSyncProgress[replayedBlock]`, is log-derived,
starts at 0, and reads `< 100` for healthy never-replayed nodes too, so it can't discriminate (details in
CAT-D's resolution section). **Decision: (a) hedge, framed as a non-diagnostic offer** — state the fact +
the reassuring "Blockchain Sync will finish" path, then offer Mithril Sync as a remedy "if sync seems slow
or runs into verification issues." The copy drops the flat "you don't need Mithril Sync" and never asserts
"stuck," so it is honest for both the healthy and the stuck node; the button stays enabled on both surfaces
(behavior unchanged). LOCKED EN strings are in CAT-D Step D1; JA goes to the translator in round 2.

### DD-706-7 — Round-2 lock-ins (decided 2026-07-09)

The remaining open items were grilled and locked; details live in the owning CAT docs:

- **At/past-snapshot copy (DD-706-6)** → **hedge as a non-diagnostic offer**; auto-detect (option c)
  investigated and rejected (`replayedBlock` is log-derived, non-discriminating). EN locked → CAT-D D1.
- **Moving-stage caution (CAT-D D2)** → EN locked, **mirrors the existing shutdown "preserve data
  integrity" voice** (`loading.screen.stoppingCardanoDescription`); the stronger "corrupt" wording was
  rejected for voice consistency. → CAT-D D2.
- **DD-706-3 dead-code gate (CAT-F F1)** → **confirmed dead by reachability check → DELETE** (no repoint;
  the "no snapshot at all" case is covered by the live `PARTIAL_SYNC_METADATA_UNAVAILABLE`). → CAT-F F1.
- **`mithrilSyncInterrupt` owner (S3)** → **CAT-F owns end-to-end** (extraction + JA). → S3, CAT-F F3.
- **CAT-G G2 race scope** → **time-boxed reproduce-first + minimal defensive re-query; defer the deep
  lock-release fix to a tracked follow-up**; G1 ships regardless. → CAT-G G2.
- **CAT-C jitter (DD-706-4)** → *superseded 2026-07-10*: plan-review R-1 showed the size plumbing cannot
  smooth the bar (whole-DB size ⇒ ≥95% weight; ancillary percent never supplied); downgraded outright to
  the Tier-3 follow-up — see the DD-706-4 resolution note and DD-706-9.
- **JA round-2 strings** → the translator's call, not ours; the DD-706-1 vocabulary renderings are the
  only JA decision that was ours and they are already locked.

### DD-706-8 — Unknown/malformed snapshot size fails the disk preflight safe (PR #3337 N1; decided 2026-07-10)

`_assertSufficientDiskSpace` (`MithrilPartialSyncService.ts:1066`) computes its requirement from
`this._latestSnapshot?.size ?? 0`. Two fail-open holes, both validated on the branch:

- **Missing size** — `normalizeSnapshotItem` (`mithrilSnapshotMetadata.ts:16-22`) defaults size to `0`
  when the aggregator metadata carries none of the recognized size fields, so `requiredBytes` degrades
  to the 4 GB `DISK_SPACE_REQUIRED` floor (`config.ts:161`) — far below a real mainnet snapshot.
- **Malformed size** — `Number(garbage)` is `NaN`, which `?? 0` does **not** catch; NaN propagates
  through both `Math.max` calls and `freeBytes < NaN` is `false`, so the preflight silently passes
  with no floor at all.

**Decision:** treat a non-positive-finite size as a **metadata failure** — throw the existing
`PARTIAL_SYNC_METADATA_UNAVAILABLE` stage error at `preparing` (same code the empty-snapshot-list path
already uses, `MithrilPartialSyncService.ts:860-865`). Zero new i18n (nothing added to the JA round-2
hand-off); the user gets the standard retryable overlay error at sync start. The reviewer's literal
`?? Infinity` was rejected (doesn't fix NaN; leaks "Required ~Infinity GB" into the error copy), as was
a new dedicated error code (new EN+JA string in the same wave that deletes one) and a conservative
hardcoded bound (magic number that goes stale).

Validation lives at the **preflight consumer only** — not in `resolveLatestSnapshotMetadata` (the
behind-ness probe shares that resolver and never needs size; rejecting there would silently degrade the
proactive prompt) and not in `normalizeSnapshotItem` (shared with the bootstrap snapshot selector, where
an unknown size is display-tolerable). Blast radius: the check runs only inside `start()`; the proactive
prompt and Diagnostics surfaces never execute it.

**Seam with CAT-C:** a single predicate `hasKnownSnapshotSize` exported from `mithrilSnapshotMetadata.ts`
(`Number.isFinite(size) && size > 0`) is used by F4 to reject at preflight **and** by C2 to omit
`snapshotSizeBytes` from transfer progress when invalid — necessary because the drift re-resolve
(`MithrilPartialSyncService.ts:249`) re-assigns `_latestSnapshot` after the preflight passed, so C2 can
see an invalid size even post-validation. See S7. Owned by **CAT-F** (Step F4), Tier 1.
*(2026-07-10: with C2 downgraded per the DD-706-4 resolution, F4 is the sole in-wave consumer; the
seam contract binds the Tier-3 follow-up instead — see S7.)*

### DD-706-9 — Plan-review resolutions (decided 2026-07-10)

The plan-validation pass (`task-ux-706-plan-review.md`) checked every code reference in this doc and the
seven CAT docs against the branch (six of eight areas dual-validated; the service spec executed live).
Its findings were resolved as follows and are folded into this doc and the CAT docs:

- **R-1 / CAT-C C2 (DD-706-4)** → **option (a), downgrade** — C1 + C3 ship; the size line + jitter fix
  move to the Tier-3 follow-up with the corrected root cause. See the DD-706-4 resolution note.
- **R-2 / CAT-F F2** → fix relocated: the service spec fails **9** tests today (not 4), and the
  slot-clobber regression at `:1565` inlines its own mocks, out of reach of both originally planned fix
  sites — only a **top-level `beforeEach`** `pathExists.mockResolvedValue(true)` (`spec.ts:113-114`)
  reaches all of them. The primary-test rewrite also needs `mockResolvedValue` (metadata resolves twice)
  and the `--start 25 --end 25` download-args assertion. Folded into CAT-F.
- **Native-dialog copy** (`mithrilPartialSyncNodeStartup.ts:103,109` — lowercase "Mithril sync",
  non-i18n, shown before the renderer exists) → **CAT-A owns**: new Step A5, casing-only (the dialog
  stays EN-only native copy; no spec asserts the literals).
- **JA Help-menu label** (`menu.helpSupport.daedalusDiagnostics` = 「Daedalus診断」,
  `source/main/locales/ja-JP.json:36`) → **B1 anchors to it**: the Help menu is the only place the JA
  UI names the Diagnostics page, so B1 aligns the two renderer strings to 「Daedalus診断」 (no
  main-process change) — the handoff note must name a menu entry that exists verbatim.
- **All remaining plan-text corrections applied as written** — CAT-A descriptor name + spec inventory;
  CAT-B `environment.isMacOS` idiom; CAT-D threading
  list (+`DaedalusDiagnostics.tsx`, +`MithrilPartialSyncAvailability`) + acceptance rewritten to the
  locked hedge copy; CAT-E decline-button redirect + CSS-var-first technique; CAT-F `yarn i18n:manage`
  procedure + fifth generated JSON; CAT-G trace correction (download spawns **outside** the mutation
  lock) + G1 blast radius; master seam fixes incl. the new **S8** D↔F spec-seam ordering rule.

**Post-decision audit (2026-07-10):** after the fold-in, a nine-validator audit (one per doc +
cross-doc consistency) re-checked the doc set against the review, these decisions, and the source.
Result: **zero blockers**, all areas pass. One major (this fold-in had left CAT-F's F4 "Seam S7"
paragraph asserting the pre-downgrade two-consumer state — caught independently by two validators) and
the factual minors (stale spans/cites, a stale B3↔D hedge, leaked doc markup, the A–D round-2 trigger
missing CAT-F's surface) were fixed in place the same day. Left as-is by convention: append-only
resolution paragraphs whose body text predates their correction notes.

### DD-706-10 — JA close-out decisions (decided 2026-07-12)

The remaining JA review threads were grilled to closure:

- **Round-1 typography flags (ellipsis style, parenthesis style) — resolved by codebase convention;
  not sent to round 2.** Measured at the merge-base with `develop` (feature strings excluded): JA
  ellipsis is ASCII `...` (7 pre-existing strings, zero 「…」 — including the same-screen sibling
  「Cardanoノードを再起動しています...」); JA parens are full-width （） around Japanese text (39 lines)
  and ASCII around Latin/technical content (`(ADA)`, `(Ctrl + D)`). The feature copy already matches
  both conventions, so the round-1 flags were not feature inconsistencies; changing either style would
  be an app-wide sweep outside 706's scope. Closed silently — no round-2 mention.
- **PR merge is not gated on JA round 2.** Send the round-2 package as soon as CAT A–F land; merge on
  code-review approval regardless; translator wording feedback lands as a tracked post-merge follow-up
  (Tier-3 list). The first-pass JA drafts (CAT-A A4, CAT-C C1, CAT-D D1/D2, CAT-F F3) ship knowingly.
- **Round-2 package shape:** regenerated full copy-table as current-state reference + a delta review
  doc (new/revised/unconfirmed entries only, with prior wording and per-entry status; linked-term
  groups noted). No screenshots. Includes the optional `recommendationUnknown` softening question and
  the §9 chain-storage strings flagged "confirm if not previously reviewed". Details in the Round-2
  hand-off section.

## CAT table

| CAT | Doc | Scope (one line) | Tier | Type |
| --- | --- | --- | --- | --- |
| A | `task-ux-706-plan-cat-a.md` | Three-term vocabulary sweep (DD-706-1) + EN casing fix, incl. the native startup-dialog literals (Step A5, DD-706-9) | 1 (high) | copy/i18n |
| B | `task-ux-706-plan-cat-b.md` | JA consistency fixes: page name aligned to the Help-menu label 「Daedalus診断」 ×2 (DD-706-9), shutdown→停止, tip 先端→最新ブロック; + handoff-note Ctrl/Cmd platform substitution | 1 (high) | copy/i18n + small code |
| C | `task-ux-706-plan-cat-c.md` | Progress metrics: Ledger state relabel (DD-706-2) + unit sanity; size/jitter plumbing downgraded to the Tier-3 follow-up (DD-706-4 resolution) | 1 (label) | copy/i18n |
| D | `task-ux-706-plan-cat-d.md` | At/past-snapshot variant split + copy (DD-706-6, Extra #2/#3) + moving-stage "don't close Daedalus" caution (locked shutdown voice, no "corrupt") | 1 (high) | copy + code (needs sign-off) |
| E | `task-ux-706-plan-cat-e.md` | Recovery/action button copy-overflow layout fix | 1 (high) | css |
| F | `task-ux-706-plan-cat-f.md` | Code-review cluster: dead `PARTIAL_SYNC_NO_CERTIFIED_RANGE` removal (DD-706-3) + broken service spec fix + `mithrilSyncInterrupt` message-structure integration + disk-preflight size-validation fail-safe (DD-706-8, PR #3337 N1) | 1 (high) | code/test/i18n |
| G | `task-ux-706-plan-cat-g.md` | Windows storage-picker mixed-separator fix + node-stop→download race guard (investigate) | 2 (medium) | code |

`mithrilSyncInterrupt` appears in both A and F: **CAT-F owns it end-to-end** (ownership locked 2026-07-09)
— the locale extraction (the code-review "message structure" item) **and** the JA string
(`代わりにMithril同期を使う`); CAT-A does not edit the key, only reviews DD-706-1 vocabulary. See S3.

## Structure, execution order, and seam contracts

Execute A → G. All CATs are behavior-preserving except CAT-D (adds a new availability variant + threads
the `gap <= 0` signal), CAT-F (two accepted behavior deltas: the removed error code, DD-706-3, and the
unknown-size preflight rejection, DD-706-8), and CAT-G (a display-path fix + a stop/download ordering
guard). CAT-C is copy-only after the DD-706-4 downgrade. Shared-file seams:

- **S1 — locale JSON trio** (`en-US.json`, `ja-JP.json`, `defaultMessages.json`): edited by A, B, C, D, F.
  Every key is on the same line in `en-US.json`/`ja-JP.json` (Block A = lines 156–178, Block B = 297–386;
  `defaultMessages.json` is EN-only and NOT line-aligned). Each CAT owns disjoint keys — mirror EN↔JA by
  line and keep `defaultMessages.json` + `MithrilBootstrap.messages.ts` `!!!` defaults in lockstep with
  EN (i18n-messaging skill governs this; run its sync/validation, do not hand-desync the four sources).
  One addition lands outside the blocks: CAT-F's extracted `loading.screen.mithrilSyncInterrupt` inserts
  at ~line 391 (between `loading.screen.loadingWalletData` and `loading.screen.pushingLedger`) — a fifth
  locale region; keep the EN↔JA insertion line-parallel.
- **S2 — `MithrilBootstrap.messages.ts`**: A (genesis strings, casing), C (Ledger state label), D
  (moving-stage caution — the new optional caution id `moveCaution` is CAT-D's, not CAT-C's), F (delete
  the two `noCertifiedRange` descriptors). Disjoint descriptors; letter order avoids collisions.
- **S3 — `mithrilSyncInterrupt`**: currently an un-extracted inline def at
  `SyncingConnectingStatus.tsx:85-90`. **Owner locked 2026-07-09: CAT-F, end-to-end** — F extracts it to
  the three locale files and (optionally) a messages module **and** supplies the JA
  (`代わりにMithril同期を使う`). CAT-A does not edit the key; it only reviews the JA against DD-706-1. No
  build-time coordination needed — single owner.
- **S4 — `mithril-partial-sync.types.ts`**: D adds `isAtOrPastSnapshot` to
  `MithrilPartialSyncAvailability` (lines 89-101 — the typed IPC payload the store consumes); F removes
  the `PARTIAL_SYNC_NO_CERTIFIED_RANGE` union member (line 34). Different regions — no conflict. (C2's
  `MithrilPartialSyncTransferProgress` field moved to the Tier-3 follow-up with the DD-706-4 downgrade.)
- **S5 — `MithrilStepIndicator.tsx`**: D only (post-downgrade — CAT-C no longer touches this file):
  attach the caution to the `install-snapshot` sub-item kept active during `finalizing` (sub-item render
  `:560-591`; a new DOM element + SCSS class, not a message swap).
- **S6 — behindness signal path** (`MithrilPartialSyncService.getPartialSyncBehindness` →
  `MithrilPartialSyncStore` → `DaedalusDiagnosticsDialog` → **`DaedalusDiagnostics`** (the presentational
  middle hop that actually renders the section) → `MithrilPartialSyncSection` →
  `MithrilPartialSyncRecommendation` / `MithrilPartialSyncConfirmation`): CAT-D threads the new
  `gap <= 0` signal along this path. No other CAT touches it.
- **S7 — snapshot-size validity predicate** (`mithrilSnapshotMetadata.ts`): F4 exports
  `hasKnownSnapshotSize` (`Number.isFinite(size) && size > 0`) and uses it to reject the disk preflight
  (DD-706-8). With C2 downgraded (DD-706-4 resolution), F4 is the **sole in-wave consumer**; the Tier-3
  size-plumbing follow-up must import this same predicate rather than inlining a duplicate — the drift
  re-resolve (`MithrilPartialSyncService.ts:249`) re-assigns `_latestSnapshot` after the preflight
  passed, so the plumbing cannot rely on F4's check alone.
- **S8 — `MithrilPartialSyncService.spec.ts` behindness block (D↔F)**: CAT-D's probe-return change
  (`isAtOrPastSnapshot`) reshapes the same exact-shape `toEqual` assertions CAT-F must turn green
  (`:1565`, and the behindness describe, `:2153` to end of file — incl. the assertions past the
  originally cited span, `:2364/:2377/:2391/:2404`), and D's new variant tests hit the same
  falsy-`pathExists` default F2 fixes. Ordering rule: land R-2's top-level `beforeEach`
  `pathExists.mockResolvedValue(true)` **with or before CAT-D's spec additions** (i.e. D applies it,
  since D lands first in A→G order), and CAT-F then verifies rather than re-fixes. (Minor, no real
  collision: CAT-E and CAT-G both touch `MithrilDecisionView` — scss vs a util call site; C and D both
  edit `MithrilStepIndicator.spec.tsx` (label assertions vs caution coverage) and A and C both edit
  `MithrilPartialSyncOverlay.spec.tsx` — disjoint assertions, A→G order applies.)

## Priority tiers

- **Tier 1 (block merge):** A (incl. Step A5 dialog literals), B (incl. Ctrl/Cmd substitution + the
  Help-menu label), C (label + unit sanity), D, E, F.
- **Tier 2 (medium — fix if it stays bounded, else follow-up):** G (both items; the node-stop race is
  an investigate-then-guard, explicitly medium).
- **Tier 3 (post-merge follow-up — logged, never silently dropped):** the partial-sync snapshot
  catch-up size line + the ancillary-only bar jitter (DD-706-4, downgraded 2026-07-10 per plan-review
  R-1). Corrected root cause on record: the fix needs a **ranged-delta** byte size (doesn't exist in
  today's metadata — must be derived, e.g. from the download child's reported totals) plus an
  ancillary-percent feed for the partial-sync path; the whole-DB `_latestSnapshot.size` cannot smooth
  the bar. Must import `hasKnownSnapshotSize` (S7). Also Tier 3: **JA translator round-2 wording
  feedback** (DD-706-10 — the package goes out when CAT A–F land; merge does not wait, so any feedback
  lands as a post-merge copy pass).

## Manual QA / smoke-test coverage (Extra #7)

The replay-screen **"Use Mithril Sync instead"** interrupt button shipped in task-705 (WI-2) — rendered
below the sync progress table on the loading / node-status screen after a 2 s debounce — is **not**
covered by `mithril-partial-sync-smoke-test-cheat-sheet.md`. Before the next installers go to QA, add a
smoke-test step exercising it: reach a ledger-replay state, confirm the button appears (after the
debounce, not flickering), click it, and confirm it hands off to the Mithril overlay. Capture it as a
checklist item in the cheat sheet's loading-screen section.

**Cheat-sheet vocabulary drift (follow-up, do not drop):** the cheat sheet still uses the pre-DD-706-1
vocabulary throughout ("standard sync", "Standard Sync (slow)", "Restart Node Sync (slow)", "Sync from
genesis", "Back to diagnostics", "Mithril fast sync"). Installers cut after CAT-A will render the new
copy, so the cheat sheet must be re-synced to the DD-706-1 terms in the same pass that adds the button
step — logged here as a tracked follow-up, not a silent gap.

## Pending external sign-off (do not treat these strings as final)

| Item | CAT | Blocker |
| --- | --- | --- |
| "Ledger state:" / 「台帳状態:」 | C | JA review ❓#6 — awaiting round-2 confirmation |
| At/past-snapshot copy (DD tooltip + confirmation prompt) | D | **EN locked 2026-07-09** (hedge-as-offer, DD-706-6); JA translator to review the draft in round 2 |
| Moving-stage caution wording | D | **EN locked 2026-07-09** (mirrors shutdown "preserve data integrity" voice); JA translator to review the draft in round 2 |
| Whole DD-706-1 vocabulary (ブロックチェーン同期 / ジェネシスからのブロックチェーン同期) | A | decided by product; JA translator has not reviewed the new terms — send in round 2 |

**Round-2 hand-off (package shape locked 2026-07-12, DD-706-10):** after CAT-A–F land (F also changes
the round-2 surface: it deletes copy-table rows §7.5/§7.6 and adds the `mithrilSyncInterrupt` JA
string), produce two documents:

1. **Regenerate `mithril-partial-sync-ja-copy-table.md`** as the full current-state EN↔JA reference
   (the current table is already stale — it predates `recommendationNearTip`, `finalizeFailed`,
   `insufficientDiskSpace`, `startFailure`, and `recommendationUnknown`, and now the new
   at/past-snapshot variant).
2. **Write the round-2 review doc as a delta** (replacing the round-1 format of
   `mithril-partial-sync-ja-copy-review.md`): only new / revised / unconfirmed strings (~20 entries),
   each with the current EN+JA, the prior wording where changed, and a status. Include the optional
   `recommendationUnknown` softening question (low priority, CAT-D) and the six §9 chain-storage
   strings marked "confirm if not previously reviewed" (round-1 coverage unconfirmed). Note the
   linked-term groups so the translator rewords them together: 「台帳状態」 (C1 label + CAT-D D1
   drafts), 「データの完全性を保つために…」 (D2 caution mirrors
   `loading.screen.stoppingCardanoDescription`), 「最新ブロック」 (B3's two keys).

No screenshots in the package (decided 2026-07-12). Text-only; the translator can request visuals for
specific entries. Tracked as the final process step alongside `nix fmt`.

**Produced 2026-07-12 (close-out):** both documents exist —
`mithril-partial-sync-ja-copy-table.md` regenerated to the full current-state EN↔JA reference
(noCertifiedRange rows dropped, all 706 wordings + new keys in), and the delta review doc written as
`mithril-partial-sync-ja-copy-review-round-2.md` (round-1 doc left intact as the historical record).

## Verification gates (per CAT and final)

1. `yarn lint` + `yarn compile` — Node v24: regenerate `.scss.d.ts` via `typed-scss-modules` first and
   apply the jest `identity-obj-proxy` sidecar before treating failures as regressions (see the
   renderer-verify-env note).
2. i18n integrity: `defaultMessages.json`, `en-US.json`, `ja-JP.json`, and the `*.messages.ts` `!!!`
   defaults must stay in sync; no orphaned keys; no missing JA. Run the i18n-messaging validation.
3. Scoped jest over touched specs. CAT-F must turn `MithrilPartialSyncService.spec.ts` green — **9
   failing tests today, not 4** (`:414` primary; behindness `:2168/:2181/:2194`; concurrent read
   `:2339`; certified-epoch trio `:2377/:2391/:2404`; slot-clobber `:1565`, which inlines its own
   mocks): the top-level `beforeEach` `pathExists.mockResolvedValue(true)` (`spec.ts:113-114`) covers
   all eight secondary failures (R-2) — and add no new red. `partialSyncErrorCopy.spec.ts:9` updates
   with the DD-706-3 removal. CAT-D adds coverage for the new at/past-snapshot variant selection
   (ordering per S8). F4 adds the disk-preflight size-validation cases (missing/0, NaN, valid) per
   DD-706-8.
4. `yarn prettier:check` on touched files; classify failures against pre-existing HEAD drift first
   (`git show HEAD:<f> | prettier --stdin-filepath <f>`); never reformat `toHaveBeenCalledWith('str',{obj})`.
5. **`nix fmt`** across all changes before pushing (Extra #1).
6. Behavior gate: the only sanctioned user-visible behavior deltas are CAT-D's new at/past-snapshot
   copy + moving-stage caution, CAT-F's removed error code (DD-706-3) and its unknown-size preflight
   rejection (DD-706-8 — a sync that previously proceeded on a 4 GB floor or no check now fails fast
   with the metadata-unavailable error), CAT-E's button layout, and CAT-G's fixes. CAT-C is copy-only
   after the DD-706-4 downgrade. All other copy edits are wording-only.
7. Manual QA: the smoke-test cheat sheet gains the replay-button step (Extra #7); the handoff-note
   shortcut renders "(Cmd + D)" on macOS and "(Ctrl + D)" on Windows/Linux (Extra #8).

## Review-log paths (created during build)

- `task-ux-706-plan-review.md` (plan validation pass over this master + the per-CAT docs)
- `task-ux-706-impl-review.md` (per-CAT implementation reviews)

## Final outcome (build close-out, 2026-07-12)

All seven CATs landed on the working tree of `feat/mithril-partial-sync-ux-refinement` (nothing
committed), verified per the gates, and approved by code review (pass 1, 2026-07-12). What landed:

- **A** — DD-706-1 three-term vocabulary sweep across EN+JA locales, `!!!` defaults, and the two
  native startup-dialog literals (A5); completed-with-deviations (the `i18n:manage` auto-add of
  `mithrilSyncInterrupt` was re-removed per S3 until CAT-F landed the key).
- **B** — page name aligned to 「Daedalus診断」 (cancel button + handoff note), シャットダウン→停止,
  先端→最新ブロック ×2, and the handoff-note `({shortcut})` substitution (Cmd + D on macOS,
  Ctrl + D elsewhere), with a jest test pinning the macOS branch.
- **C** — C1 "Ledger state:" / 「台帳状態:」 relabel; C2 stayed downgraded to Tier 3 per DD-706-4/R-1
  (follow-up entry verified, nothing implemented by design); C3 unit sanity confirmed the chunk count
  renders only as a file count (no chunk-as-bytes anomaly, no backend follow-up).
- **D** — at/past-snapshot variant split threaded end-to-end (`isAtOrPastSnapshot` from
  `getPartialSyncBehindness` through store/dialog to tooltip + confirmation) with the locked hedge
  copy, plus the D2 moving-stage caution on both overlays in the shutdown voice; S8 applied (service
  spec handed to F at 75/76).
- **E** — CSS-only button-overflow fix on three surfaces (error view, decision view, proactive
  prompt); confirmation dialog verified as fitting and left untouched; completed-with-deviations
  (reasoned width computation in lieu of storybook eyeballing; `min-width: 180px` tuning).
- **F** — dead `PARTIAL_SYNC_NO_CERTIFIED_RANGE` deleted after a live reachability re-check (zero
  references repo-wide); service spec green 78/78 with the primary test rewritten;
  `mithrilSyncInterrupt` extracted end-to-end with the JA (代わりにMithril同期を使う); DD-706-8
  unknown-size preflight fail-fast via `hasKnownSnapshotSize`.
- **G** — Windows storage-picker display-path separator fix (renderer-only; main-side resolver
  untouched) and the reproduce-first stop→download race guard (live `getNodeState` re-query),
  reproduced deterministically at spec level before fixing.

**JA round-2 flags:** the round-2 package was produced with this close-out (see the Round-2 hand-off
section). First-pass JA drafts knowingly shipped pending translator review (DD-706-10): the DD-706-1
family renderings ブロックチェーン同期 / ジェネシスからのブロックチェーン同期 (A), 「台帳状態」 (C1),
the three CAT-D drafts (at/past-snapshot tooltip + confirmation body, moveCaution), and
代わりにMithril同期を使う (F3). Round-1 typography flags stayed closed by codebase convention (not
sent to round 2); the bootstrap feature title / accept button deliberately keep the fast-sync name.
Merge is not gated on round 2.

**Follow-up outcomes (tracked, none dropped):**

- Tier-3 size-plumbing follow-up unchanged (ranged-delta byte size + ancillary-percent feed; must
  import `hasKnownSnapshotSize` per S7).
- CAT-G G2 deep fix: the live re-query is a window-narrowing placeholder only — the real guard needs
  a bounded settle on ImmutableDB lock / immutable-directory release before the download child
  spawns (repro recipe: the two stale-snapshot coordinator specs).
- Cheat sheet still pending: replay interrupt-button smoke-test step + DD-706-1 vocabulary re-sync
  (Extra #7); B4 makes its macOS ⌘D note verifiable.
- Final pre-push gates still to run: `nix fmt` (gate 5) + full-tree lint/prettier; several touched
  files carry pre-existing HEAD prettier drift — classify, don't reformat. The pre-existing
  `.gitignore` modification (2026-07-09, not wave-touched) should not ride into the feature commit
  unless intended.
- CAT-E: visual eyeball pass on the three fixed button rows when a display is available; re-audit
  the confirmation dialog only if round 2 lengthens `mithrilPartialSyncConfirmationCancel` beyond
  ~16 full-width glyphs.
- Tracker hygiene: `mithril-partial-sync-ux-refinement-tasks.json` had stopped at task-703 (no
  704/705/706 entries); a minimal completed task-706 entry was added at close-out, totals left as
  found.

## Post-review fix wave (2026-07-13, amended into the task commit)

The consolidated audit of the task commit confirmed two medium misses; both were fixed and amended
in, with decisions grilled against DD-706-1/DD-706-9 and CAT-D:

- **A5 extended beyond casing (dialog rename):** DD-706-9 scoped the native startup dialog to
  casing-only, but DD-706-1 bans "partial sync" in user-facing copy and no doc exempted the phrase.
  Decision: minimal phrase swap — title `Interrupted Mithril Sync detected`, message
  "…an interrupted Mithril Sync after live chain replacement began…". Internal strings (wipe-log
  reason, post-cutover error messages, preflight errors) deliberately keep "Mithril partial sync":
  the MithrilErrorView technical-details pane is a verbatim raw-diagnostics surface, and renaming
  ~20 log/error literals would hurt log greppability for zero authored-copy gain.
- **D2 caution timing (partial-sync cutover):** the "don't close Daedalus" caution keyed only on
  `install-snapshot` (bootstrap), so on the partial-sync overlay it never showed during the actual
  live-chain cutover (item `installing`) and appeared only at `finalizing` — after the move, once
  the recoverable marker was written. Fixed minimally: caution now keys on
  `install-snapshot || installing` when active, with a spec pinning the partial-sync shape. The
  pre-existing `keepInstallingActiveDuringFinalizing` over-extension (caution persists through
  cleanup on the bootstrap overlay) stays — conservative direction, pre-dates the wave; follow-up
  if it ever bothers QA.
- **Cheat sheet §5 corrected against the marker state machine:** the blocking dialog corresponds
  exactly to marker `cutover-in-progress`, i.e. the "Installing snapshot..." stage; quitting during
  Finalizing (marker `installed-awaiting-node-start`) resumes normally. The edge check previously
  named the wrong stage and the old dialog title.
- **Low sweeps:** the two combined-progress translator descriptions ("fast sync" → "ledger state",
  matching the DD-706-2 relabel) and the lowercase "full Mithril sync" description, mirrored into
  both generated i18n JSONs; the staged tasks.json v1.13.0 backfill (704/705 entries, totals
  reconciled to 28/169) committed with the amend per grill decision.
- **No action:** S7 drift re-resolve and G2 restart/wipe live-getter seams stay deferred as already
  tracked above; the Cmd + D test misattribution existed only in the implementer's chat summary
  (repo docs attribute it correctly to `SyncingConnectingMithrilPrompt.spec.tsx`).
