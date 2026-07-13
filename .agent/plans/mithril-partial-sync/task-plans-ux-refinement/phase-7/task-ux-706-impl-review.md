# task-ux-706 — implementation review log

Implementation — CAT-A:
Timestamp: 2026-07-12T18:38:14Z
Files changed:
- source/renderer/app/i18n/locales/en-US.json (12 keys: A2 casing x3, A3 Blockchain Sync x4, A4 Blockchain Sync from Genesis x5)
- source/renderer/app/i18n/locales/ja-JP.json (10 keys: A1 Mithril同期 outlier, A3 ブロックチェーン同期 x4, A4 ジェネシスからのブロックチェーン同期 family x5; EN↔JA stayed line-parallel, 1516/1516 lines)
- source/renderer/app/components/loading/mithril-bootstrap/MithrilBootstrap.messages.ts (10 !!! defaults: description, decline, startFailureHint, stepIndicatorLabel, verify/convert hints, partialSyncNodeStartingDetail, partialSyncFailedHint, partialSyncRestartNormally, partialSyncWipeAndFullSync)
- source/renderer/app/components/loading/syncing-connecting/SyncingConnectingMithrilPrompt.tsx (promptBodyBenefit, promptStandardButton defaults; prettier collapse of the now-shorter defaultMessage)
- source/main/mithril/mithrilPartialSyncNodeStartup.ts (A5: two native-dialog literal casings, :103 button + :109 message)
- Spec assertions: SyncingConnectingMithrilPrompt.spec.tsx (:54, :93, :146), MithrilProactivePromptContainer.spec.tsx (:68), MithrilPartialSyncOverlay.spec.tsx (:99, :292, :307), MithrilBootstrap.spec.tsx (:168, :243, :263 + stale comment :239)
- Generated via yarn i18n:manage (never hand-edited): source/renderer/app/i18n/locales/defaultMessages.json, translations/messages.json
Verification run:
- Acceptance greps all clean: no Latin "Mithril Sync" in any ja-JP.json value; no Standard Sync/標準同期/Node Sync/ノード同期を slow-path names; no user-facing "sync from genesis"/bare ジェネシスから同期; no lowercase "Mithril sync" in en-US.json or the native dialog; EN/JA key lists line-identical.
- Scoped jest (Node v24 identity-obj-proxy scss sidecar per verify-env memory): 5 suites / 71 tests green (SyncingConnectingMithrilPrompt, MithrilProactivePromptContainer, MithrilPartialSyncOverlay, MithrilBootstrap, mithrilPartialSyncNodeStartup specs).
- yarn compile (tsc --noEmit) clean; eslint on touched files 0 errors (11 pre-existing warnings, none on edited lines).
- prettier 2.1.2 on touched files: all clean except SyncingConnectingMithrilPrompt.spec.tsx, whose sole remaining hunk is the pre-existing toHaveBeenCalledWith oscillation at :185, byte-identical at HEAD (classified via git show HEAD | prettier --stdin-filepath); left untouched per the oscillation rule.
Deviations / handoffs:
- yarn i18n:manage auto-inserts loading.screen.mithrilSyncInterrupt into en-US.json/ja-JP.json:391 with the "!!!Use Mithril Sync instead" placeholder (the un-extracted inline def at SyncingConnectingStatus.tsx is pre-existing HEAD drift the extractor now sees). Removed both lines per seam S3 — CAT-F owns that key end-to-end. Any CAT that re-runs i18n:manage before CAT-F lands will see the same auto-add and should re-remove it; the entry legitimately remains in the two generated files (defaultMessages.json, translations/messages.json), which mirror source.
- Regenerating the generated files also trued up unrelated pre-existing HEAD staleness (e.g. DiagnosticsTimeStatusRow "NTP service unreachable" + description fields). Tool output, not hand edits; kept.
- JA round-2: all new ブロックチェーン同期 / ジェネシスからのブロックチェーン同期 renderings are first-pass drafts for the translator package (DD-706-10); A4 sentence forms use ジェネシスからブロックチェーン同期を実行してください per plan.

Implementation — CAT-B:
Timestamp: 2026-07-12T18:45:36Z
Files changed:
- source/renderer/app/i18n/locales/en-US.json (B1 cancel :160 "Back to Daedalus Diagnostics"; B4 handoff note :173 "(Ctrl + D)" → "({shortcut})")
- source/renderer/app/i18n/locales/ja-JP.json (B1 :160 Daedalus診断に戻る + :173 Daedalus Diagnostics画面→Daedalus診断画面; B2 :178 シャットダウン→停止; B3 :164/:169 先端→最新ブロック; B4 :173 ({shortcut}); EN↔JA stayed line-parallel, 1516/1516 lines, key lists identical)
- source/renderer/app/components/status/MithrilPartialSyncConfirmation.tsx (:31 cancel !!! default)
- source/renderer/app/components/loading/syncing-connecting/SyncingConnectingMithrilPrompt.tsx (promptHandoffNote !!! default gains {shortcut}; renderChoiceView destructures global.environment.isMacOS and passes shortcut: 'Cmd + D' | 'Ctrl + D' — per the crypto.ts/StakePoolsSettings destructure precedent)
- Spec assertions: MithrilPartialSyncConfirmation.spec.tsx (:120), MithrilPartialSyncSection.spec.tsx (:78, prettier-wrapped), SyncingConnectingMithrilPrompt.spec.tsx (new macOS-branch test asserting "(Cmd + D)"; existing Ctrl + D test unchanged — jest globals leave isMacOS undefined so the non-mac branch renders byte-identically)
- Generated via yarn i18n:manage (never hand-edited): defaultMessages.json, translations/messages.json
Verification run:
- Acceptance greps clean: no bare user-facing 診断 without the Daedalus prefix, no 先端, no シャットダウン in the process summary, no Latin "Diagnostics" left in renderer ja-JP.json; {shortcut} byte-identical EN↔JA at :173 and never translated; menu labels (source/main/locales :36) confirmed untouched.
- Scoped jest (Node v24 identity-obj-proxy scss sidecar per verify-env memory): 4 suites / 49 tests green (SyncingConnectingMithrilPrompt incl. new Cmd + D test, MithrilPartialSyncConfirmation, MithrilPartialSyncSection, MithrilProactivePromptContainer); section spec re-run green after prettier wrap.
- tsc --noEmit clean (CAT-A's regenerated .scss.d.ts still present); eslint on touched files 0 errors (6 pre-existing warnings, none on edited lines).
- prettier 2.1.2: all touched files clean except SyncingConnectingMithrilPrompt.spec.tsx (sole hunk = pre-existing toHaveBeenCalledWith oscillation, HEAD has drift) and MithrilPartialSyncConfirmation.tsx (two pre-existing HEAD-drift hunks, neither on the edited line) — both classified via git show HEAD | prettier --stdin-filepath and left untouched per the oscillation/drift rule.
Deviations / handoffs:
- yarn i18n:manage re-added loading.screen.mithrilSyncInterrupt to en-US.json/ja-JP.json:391 (same auto-add CAT-A logged); re-removed both lines per seam S3 — CAT-F owns the key end-to-end. The entry legitimately remains in the two generated files.
- Added one jest test for the macOS shortcut branch (not mandated by the plan doc, which lists the Cmd + D rendering under manual QA); smallest way to pin the acceptance criterion in CI. No other spec additions.
- JA round-2: revised strings for the translator delta — Daedalus診断に戻る, Daedalus診断画面 handoff note, 停止 process summary, and the two 最新ブロック keys (linked-term group per DD-706-10).

Implementation — CAT-C:
Timestamp: 2026-07-12T18:51:22Z
Files changed:
- source/renderer/app/i18n/locales/en-US.json (:312 combinedDetail "Fast sync:" → "Ledger state:"; :313 combinedLabel "Snapshot Files and Fast Sync" → "Snapshot Files and Ledger State")
- source/renderer/app/i18n/locales/ja-JP.json (:312 高速同期: → 台帳状態:; :313 スナップショットファイルと高速同期 → スナップショットファイルと台帳状態; EN↔JA stayed line-parallel, 1516/1516 lines, key lists identical; placeholders byte-identical)
- source/renderer/app/components/loading/mithril-bootstrap/MithrilBootstrap.messages.ts (:236 progressCombinedLabel, :243 progressCombinedDetail !!! defaults)
- Spec assertions: MithrilStepIndicator.spec.tsx (:125, :163, :184, :227, :255, :293, :318), MithrilPartialSyncOverlay.spec.tsx (:273 — doc cited :271; CAT-A's earlier edits shifted the line, live repo wins)
- Generated via yarn i18n:manage (never hand-edited): defaultMessages.json (:2765/:2770), translations/messages.json
Verification run:
- Scoped jest (Node v24 identity-obj-proxy scss sidecar, .cache/jest.scss-override.js): 2 suites / 35 tests green (MithrilStepIndicator, MithrilPartialSyncOverlay).
- tsc --noEmit exit 0; eslint on touched files 0 errors (2 pre-existing warnings in MithrilPartialSyncOverlay.spec.tsx:33, not on edited lines); prettier 2.1.2 clean on all five touched files (no HEAD-drift classification needed).
- Acceptance greps: no "Fast sync:"/"Snapshot Files and Fast Sync"/高速同期 progress-label strings remain in the four i18n sources or messages.ts; all four sources in lockstep. Remaining "fast sync" hits are exclusively the feature title (/fast sync with mithril/i in MithrilBootstrap.spec.tsx + MithrilProgressView.spec.tsx, ja-JP.json:297/:345 accept/title) — untouched per plan — plus two MithrilStepIndicator.spec.tsx test names (:168, :207) describing the internal ancillary data flow (props still fastSync*), which the plan's assertion-line list does not cover.
- C2 (DOWNGRADED per plan-review R-1 / DD-706-9a): no code implemented; verified the Tier-3 entry already present in task-ux-706.md:325-332 with the corrected root cause (ranged-delta size + ancillary-percent feed + hasKnownSnapshotSize S7 import). Obligation satisfied by verification only.
- C3 unit sanity (check, no change): combinedDetail snapshot side uses formatFileCount (MithrilStepIndicator.tsx:214-219); only byte figures are the ancillary "Ledger state" pair (formatTransferSize, :220-222) and the whole-snapshot "≈ {totalSize} total" line (:225-233), both legitimate. Also traced InlineProgressBar's byte-format fallback (InlineProgressBar.tsx:25-27, downloaded/total props): its single live call site (MithrilStepIndicator.tsx:682-687) passes details only, so the fallback is never fed chunk counts. No converted MB/GB immutable figure anywhere — no backend follow-up needed.
Deviations / handoffs:
- yarn i18n:manage re-added loading.screen.mithrilSyncInterrupt to en-US.json/ja-JP.json:391 (same auto-add CAT-A/B logged); re-removed both lines per seam S3 — CAT-F owns the key end-to-end. The entry legitimately remains in the two generated files.
- JA round-2: 「台帳状態」 (C1 label) pending translator round-2 sign-off (❓#6); implemented now per DD-706-10 (merge not gated). Linked-term group with CAT-D's D1 drafts per the round-2 package notes.

Implementation — CAT-D:
Timestamp: 2026-07-12T19:10:57Z
Files changed:
- source/main/mithril/MithrilPartialSyncService.ts (D1 signal: getPartialSyncBehindness now returns isAtOrPastSnapshot on all three paths — true at gap<=0, false at gap>0 and on probe failure; gap logic itself untouched)
- source/common/types/mithril-partial-sync.types.ts (MithrilPartialSyncAvailability gains optional isAtOrPastSnapshot; short-circuits omit it, absence = false)
- source/renderer/app/stores/MithrilPartialSyncStore.ts (@observable isAtOrPastSnapshot = false, set via Boolean() in _applyAvailability)
- source/renderer/app/containers/status/DaedalusDiagnosticsDialog.tsx + components/status/DaedalusDiagnostics.tsx (isMithrilPartialSyncAtOrPastSnapshot threaded: container → prop decl → destructure → section)
- source/renderer/app/components/status/MithrilPartialSyncSection.tsx (variant ladder gains 'at-or-past-snapshot' below probe-failed, above near-tip; signal also passed into the confirmation)
- source/renderer/app/components/status/MithrilPartialSyncRecommendation.tsx (variant union + recommendationAtOrPastSnapshot tooltip, LOCKED EN, JA draft)
- source/renderer/app/components/status/MithrilPartialSyncConfirmation.tsx (atOrPastSnapshot body variant selected whenever the signal is set, regardless of epochs; behind/behindUnknown selection otherwise unchanged)
- source/renderer/app/components/loading/mithril-bootstrap/MithrilBootstrap.messages.ts (D2 progressMoveCaution / loading.mithrilBootstrap.progress.moveCaution, LOCKED EN shutdown-voice)
- source/renderer/app/components/loading/mithril-bootstrap/MithrilStepIndicator.tsx + .scss (caution rendered inside the install-snapshot sub-item only while active — new subItemLabelGroup/subItemCaution/subItemWithCaution classes, secondary-text styling reusing --theme-mithril-secondary-text-color, no severe-error styling, no aria-live; .scss.d.ts regenerated via typed-scss-modules per verify-env memo)
- source/renderer/app/i18n/locales/en-US.json / ja-JP.json (3 key pairs inserted line-parallel: ConfirmationAtOrPastSnapshot :158, RecommendationAtOrPastSnapshot :165, moveCaution :322; 1519/1519 lines, key lists identical; EN byte-for-byte from the locked doc strings, JA = round-2 drafts as-is)
- Specs: MithrilPartialSyncService.spec.ts (S8: top-level beforeEach pathExists.mockResolvedValue(true) + isAtOrPastSnapshot added to the 10 exact-shape toEqual assertions — slot-clobber, behindness in-range x5, concurrent, certified-epoch trio), MithrilPartialSyncStore.spec.ts (new _applyAvailability mirror/clear test), MithrilPartialSyncSection.spec.tsx (defaultProps + at/past tooltip, probe-failed-outranks, confirmation-body-regardless-of-epochs tests), MithrilPartialSyncConfirmation.spec.tsx (defaultProps + at/past-over-behind test), DaedalusDiagnostics.spec.tsx (defaultProps gains the new required prop), MithrilStepIndicator.spec.tsx (caution-attached + caution-dropped tests)
- storybook/stories/nodes/status/Diagnostics.stories.tsx (baseProps + section fixture gain the flag; new 'Partial Sync At Or Past Snapshot' DD story and 'At Or Past Snapshot' confirmation story)
- Generated via yarn i18n:manage (never hand-edited): defaultMessages.json, translations/messages.json
Verification run:
- Baseline pinned first: service spec 9 failing at start of CAT-D, exactly the master-doc list. After the S8 beforeEach fix + shape updates: 75/76 green; the single remaining red is the :414 primary test, owned by CAT-F (left untouched per instruction).
- Scoped renderer jest (Node v24 identity-obj-proxy sidecar .cache/jest.scss-override.js): 6 suites / 110 tests green (Section, Confirmation, DaedalusDiagnostics, Store, StepIndicator, ProactivePromptContainer) + overlay regression pass (MithrilPartialSyncOverlay, MithrilBootstrap: 31 green) + controller/IPC-channel specs green (mocked seams, no assertion changes needed).
- tsc --noEmit -p . exit 0 (covers storybook; tsconfig excludes only node_modules). eslint on all touched files: 0 errors (pre-existing decorator/ts-ignore warnings only).
- prettier 2.1.2: every touched file clean except pre-existing HEAD-drift hunks, classified per git show HEAD | prettier --stdin-filepath (Service.ts/spec, types.ts, Store.ts/spec, DaedalusDiagnostics.tsx, Confirmation.tsx :23 id-wrap, StepIndicator.tsx) — hunk locations byte-identical to HEAD modulo my line shifts; my two newly-dirty files (Recommendation.tsx, StepIndicator.spec.tsx) prettier-written, and my new Confirmation descriptor id wrapped to the prettier style so only the HEAD hunk remains.
- i18n integrity: all four sources in lockstep for the 3 new keys (scripted defaultMessage comparison), EN↔JA line-parallel 1519/1519 with identical key order, no placeholders in the new strings, locked EN byte-for-byte from the plan doc.
- Acceptance: at/past variant renders only from the service-computed gap<=0 signal (no epochs-absence inference, no renderer threshold); button stays enabled on both surfaces (asserted); probe-failed outranks at/past (asserted); near-tip (B3 最新ブロック) and recommendationUnknown untouched; behindUnknown still renders for genuinely-behind-no-epoch (existing test green); caution visible only while install-snapshot is active and clears after (asserted both ways); kill-switch gating unchanged (DD spec kill-switch test green).
Deviations / handoffs:
- isAtOrPastSnapshot returned as explicit false on the probe-failed catch path too (constant shape across all returns), per the anchor-verification instruction to update the :2213/:2228 assertions; plan D1 text alone only mandated the gap<=0 surfacing.
- DaedalusDiagnostics.spec.tsx was not in the plan's spec inventory but needed the new required prop (type seam); one-line defaultProps addition.
- Confirmation prop declared optional (mirrors behindByEpochs); Section/DaedalusDiagnostics props required (mirror isProbeFailed).
- Overlay stories not edited for D2: the existing Finalizing stories render the caution automatically via keepInstallingActiveDuringFinalizing; the two new Diagnostics stories cover the D1 variant.
- yarn i18n:manage re-added loading.screen.mithrilSyncInterrupt at en/ja :395; re-removed both lines per seam S3 (CAT-F owns the key) — same auto-add A/B/C logged; entry legitimately remains in the two generated files.
- Handoff to CAT-F: service spec :414 primary test still red (F owns the rewrite); the S8 top-level pathExists fix is in place, so F verifies rather than re-fixes — 8 of the 9 baseline failures are now green.
- JA round-2 (DD-706-10): both at/past drafts + the moveCaution draft flagged for the translator delta; linked-term groups 台帳状態 (with C1) and データの完全性を保つために… (mirrors loading.screen.stoppingCardanoDescription); optional low-priority question: soften recommendationUnknown to the same offer framing (left as-is per plan).

Implementation — CAT-E:
Timestamp: 2026-07-12T19:19:27Z
Files changed:
- source/renderer/app/components/loading/mithril-bootstrap/MithrilErrorView.scss (.actions gains flex-wrap: wrap; new grouped .primaryAction/.secondaryAction rule sets the plan recipe — --rp-button-height/-width: auto, --rp-button-line-height: 1.3, height/width auto, max-width 100%, min-height 50px, white-space normal — plus a min-width: 180px tune)
- source/renderer/app/components/loading/mithril-bootstrap/MithrilDecisionView.scss (same recipe: flex-wrap on .actions + grouped geometry rule incl. min-width 180px)
- source/renderer/app/components/loading/syncing-connecting/SyncingConnectingMithrilPrompt.scss (same recipe; no min-width added — the existing per-class min-width: 180px rules are kept and, being later in the file at equal specificity, stay authoritative)
- No .scss.d.ts changes: grouped selectors reuse existing class names; typed-scss-modules regen on all three files produced byte-identical output (git status clean for *.scss.d.ts).
Verification run:
- Storybook visual verification impractical in this environment (no display); replaced with a reasoned width computation at the standard metrics (14px font; CJK glyph = 14px full-width; Latin ≈ 7.2–7.5px average), per the assignment's explicit allowance:
  - MithrilErrorView 3-button row (720px card − 64px padding = 656px content; equal shrink ≈ 210px/button, ≈ 160px text box): EN "Wipe chain data and do full Mithril Sync" ≈ 292px and JA チェーンデータを削除して完全なMithril同期を実行 ≈ 328px wrap to 2–3 × 20px lines inside the fixed 50px/12px-padding box (26px content height) — confirmed overflow; fixed.
  - MithrilDecisionView 2-button row: in the 721–727px viewport band (row mode persists above the 720px media query while the overlay's 24px content padding shrinks the card to ~673px; ≈ 248px text box) JA decline ジェネシスからのブロックチェーン同期 = 18 × 14 = 252px wraps — confirmed overflow (JA-only, narrow band); fixed.
  - SyncingConnectingMithrilPrompt (520px card − 48px = 472px; buttons shrink to ≈ 230px, ≈ 180px text box): JA ブロックチェーン同期（低速） = 14 × 14 = 196px wraps at ALL widths — confirmed overflow; fixed.
  - Confirmation Dialog (580px wrapper, width:50% ⇒ ≈ 280px/button, ≈ 230px text box): longest labels EN "Back to Daedalus Diagnostics" ≈ 202px, JA Daedalus診断に戻る ≈ 128px — no overflow; audit-only, untouched (Dialog.scss/DaedalusDiagnostics.scss unchanged).
  - Post-fix reflow check: with content-sized buttons + flex-wrap, the worst rows lay out without intra-button wrap (EN error row wraps to 2 flex lines: 267+340 then 231; JA error row: 272 then 376+236) and any residual wrap grows height via min-height/auto instead of clipping.
- stylelint clean on the three files (custom-properties-first + alphabetical order respected); prettier 2.1.2 --check clean (no HEAD-drift classification needed); tsc --noEmit -p . exit 0; scoped jest with the .cache/jest.scss-override.js identity-obj-proxy sidecar: 3 suites / 46 tests green (MithrilPartialSyncOverlay, MithrilBootstrap, SyncingConnectingMithrilPrompt; no MithrilDecisionView spec exists). No label text touched; kill-switch/recovery-action boundaries untouched (css-only change).
Deviations / handoffs:
- Added min-width: 180px to the MithrilErrorView/MithrilDecisionView grouped rules (plan's "implementer may tune visually" latitude): with width:auto, short labels ("Try again", "Quit Daedalus") would otherwise collapse to ~110–140px; 180px matches the existing prompt/diagnostics convention and never re-triggers overflow (3 × 180 + 24 = 564 ≤ 656).
- Behavior nuance in the <720px column stack: buttons now actually stretch full-width (align-items: stretch previously lost to the definite 360px width); stack order and breakpoint unchanged — reads as the originally intended behavior.
- Buttons at default widths now render content-sized (+min-width floor) instead of the shrunken 360px basis (error row ~210px → natural, decision row ~322px → ~206–252px) — the inherent visual consequence of the plan's recommended width:auto recipe.
- Handoff to verify stage / manual QA: storybook error-story permutations (retry/restart/wipe × EN/JA) and the prompt row should get an eyeball pass when a display is available; computation says all fit without clipping.

Implementation — CAT-F:
Timestamp: 2026-07-12T19:30:04Z
Files changed:
- source/common/types/mithril-partial-sync.types.ts (F1: PARTIAL_SYNC_NO_CERTIFIED_RANGE union member removed, atomically with the copy-map entry)
- source/renderer/app/components/loading/mithril-bootstrap/partialSyncErrorCopy.ts (F1: NO_CERTIFIED_RANGE const + COPY_BY_CODE entry removed)
- source/renderer/app/components/loading/mithril-bootstrap/MithrilBootstrap.messages.ts (F1: partialSyncErrorNoCertifiedRangeTitle/Hint descriptors removed)
- source/renderer/app/components/loading/mithril-bootstrap/partialSyncErrorCopy.spec.ts (F1: tuple dropped from the it.each covered set)
- source/main/mithril/MithrilPartialSyncService.spec.ts (F2: primary test rewritten as 'downloads a ledger-only range when local immutables are at the certified tip' — mockResolvedValue for the double metadata resolve, local=25/certified=25, full downloadArgs assertion pinning --start 25 --end 25; F4: two new disk-preflight tests — size 0 and size NaN reject with PARTIAL_SYNC_METADATA_UNAVAILABLE at preparing and check-disk-space is never invoked)
- source/main/mithril/mithrilSnapshotMetadata.ts (F4: hasKnownSnapshotSize exported — positive finite size, null-safe on the item)
- source/main/mithril/mithrilSnapshotMetadata.spec.ts (F4: unit cases 42/0/-1/NaN/null)
- source/main/mithril/MithrilPartialSyncService.ts (F4: hasKnownSnapshotSize import + guard at top of _assertSufficientDiskSpace throwing the existing PARTIAL_SYNC_METADATA_UNAVAILABLE stage error at preparing; stale "fails closed" comment above the margin constants rewritten to the new contract)
- source/renderer/app/i18n/locales/en-US.json / ja-JP.json (F1: noCertifiedRange title/hint pair auto-deleted by i18n:manage at :369-370; F3: loading.screen.mithrilSyncInterrupt auto-added at :392 — EN value hand-stripped to "Use Mithril Sync instead", JA hand-set to 代わりにMithril同期を使う; EN↔JA line-parallel 1518/1518, key lists identical)
- Generated via yarn i18n:manage (never hand-edited): defaultMessages.json, translations/messages.json (noCertifiedRange entries dropped; mithrilSyncInterrupt entry added, sourced from the inline descriptor kept in SyncingConnectingStatus.tsx with its !!! default)
Verification run:
- F1 gate re-confirmed live before deleting: NO_CERTIFIED_RANGE appears in source/main+source/common only as the type-union member (no throw, no errorCode =, no dynamic construction); the empty-snapshot-list path still throws live PARTIAL_SYNC_METADATA_UNAVAILABLE (MithrilPartialSyncService.ts:860-865). DELETE per DD-706-7, no repoint.
- Baseline pinned first: service spec 75/76 with only the :418 primary red, exactly the CAT-D handoff; S8 verified (top-level beforeEach pathExists.mockResolvedValue(true) already present at spec :124 — not re-fixed).
- Scoped jest final: 3 suites / 103 tests green (service 78 = 76+2 new, metadata 11 = 10+1 new, errorCopy 14).
- yarn compile (tsc --noEmit) exit 0; eslint on all touched files 0 errors (pre-existing warnings only).
- prettier 2.1.2: mithrilSnapshotMetadata.ts/.spec.ts, MithrilBootstrap.messages.ts, both locale JSONs clean; the five dirty files (types.ts, Service.ts, Service.spec.ts, errorCopy.ts, errorCopy.spec.ts) all classified HEAD-DIRTY via git show HEAD | prettier --stdin-filepath with hunk starts byte-identical modulo my line shifts (require-mock pattern, Record assignment break, `as any` parens) — no new drift from my lines; left untouched per the oscillation/drift rule.
- Acceptance greps: zero noCertifiedRange/NO_CERTIFIED_RANGE references repo-wide (source, storybook, translations, generated files); mithrilSyncInterrupt in lockstep across all four i18n sources (descriptor !!! default, defaultMessages, en stripped, ja DD-706-1); i18n:check clean (only the standing pre-existing missing-key report lines, unrelated keys).
- Behavior gate: both sanctioned CAT-F deltas landed (dead error code removed; unknown/0/NaN snapshot size now fails the preflight fast with the retryable metadata-unavailable copy instead of a 4 GB floor or no check). Both disk-measurement fallbacks (whole-snapshot bound, fail-open free space) untouched; recovery actions, kill switch, empty-chain bootstrap untouched.
Deviations / handoffs:
- The primary test was renamed (old name asserted a rejection that no longer exists); assertion is the full downloadArgs array mirroring the staged-restore sibling, which is stronger than the minimum '--start 25 --end 25' the plan required.
- The `this._latestSnapshot?.size ?? 0` read was left as-is after the new guard (the ?? 0 is now unreachable); smallest diff, no contract change.
- i18n:manage auto-deleted the obsolete en/ja noCertifiedRange keys (the residual-to-verify from the plan resolved: the manager deletes, it is not report-only), and auto-added mithrilSyncInterrupt with the !!! placeholder in both files — EN stripped and JA set by hand, per seam S3 single-ownership.
- Round-2 handoff (DD-706-10): copy-table rows §7.5/§7.6 (noCertifiedRange title/hint) are deleted — drop them from the regenerated EN↔JA table; 代わりにMithril同期を使う is a first-pass draft for the translator delta.
- Seam S7 handoff: hasKnownSnapshotSize is the single size-validity predicate; the Tier-3 size-plumbing follow-up must import it (drift re-resolve reassigns _latestSnapshot after the preflight passed).

Implementation — CAT-G:
Timestamp: 2026-07-12T19:38:16Z
Files changed:
- source/renderer/app/components/chain-storage/chainStorageUtils.ts (G1: getManagedChainDisplayPath joins with '\' when the parent contains a backslash and no forward slash, stripping trailing backslashes first; POSIX inputs still go through path.join unchanged; default-path fallback branch untouched; display-only — main-side chainStoragePathResolver.ts untouched, 0 diff lines)
- source/renderer/app/components/chain-storage/ChainStorageLocationPicker.spec.tsx (G1: new win32 case D:\Cardano\wallet → D:\Cardano\wallet\chain; existing POSIX assertion :76 untouched)
- source/main/utils/chainStorageCoordinator.ts (G2: PartialSyncDependencies gains optional getNodeState; startPartialSync lock closure prefers the live node state over the click-time snapshot before _ensureNodeStoppedForPartialSync; no coordinator restructure, handlers.start()/post-lock IIFE untouched)
- source/main/utils/chainStorageCoordinator.spec.ts (G2: two new tests — stale 'stopped' snapshot with live 'stopping', and no snapshot with live 'stopping' — both assert nodeStopHandler runs before handlers.start via invocationCallOrder and that layout sees nodeState 'stopped')
- source/main/mithril/MithrilController.ts (G2: _getPartialSyncDependencies supplies getNodeState: () => this.getNodeState(), the existing live getter)
Verification run:
- G2 repro (reproduce-first, within the 30-min box): the two new coordinator specs were written and run against the unmodified coordinator first — both red (nodeStopHandler 0 calls; handlers.start spawned with the node still 'stopping'), a deterministic spec-level reproduction of the stale click-time null/STOPPED snapshot path; green after the guard.
- Scoped jest: chainStorageCoordinator.spec.ts 45/45 (incl. the pre-existing running/stopping/stopped startPartialSync cases, which pass no getNodeState and keep the snapshot fallback green) + MithrilController.spec.ts 12/12; renderer side with the .cache/jest.scss-override.js identity-obj-proxy sidecar: ChainStorageLocationPicker.spec.tsx + MithrilBootstrap.spec.tsx 29/29 (blast radius incl. the :126/:170 '/mnt/current-chain/chain' assertions through the MithrilDecisionView.tsx:52 second caller — fixed for free, POSIX unchanged).
- tsc --noEmit -p . exit 0 (main-process code touched); eslint on the five touched files 0 errors (21 pre-existing warnings, none on edited lines).
- prettier 2.1.2: chainStorageUtils.ts + picker spec clean; coordinator.ts/.spec.ts + MithrilController.ts DIRTY but classified pure HEAD drift via git show HEAD | prettier --stdin-filepath (hunk counts 5/15/2 identical at HEAD and working copy, locations shifted only by my insertions, my lines only as context) — left untouched per the drift rule.
- Boundaries: kill switch, recovery actions, empty-chain bootstrap untouched; no IPC payload change (getNodeState is a main-process-internal callback).
Deviations / handoffs:
- The guard prefers the live state whenever getNodeState is provided (also when the snapshot said running/stopping), rather than re-querying only on a null/STOPPED snapshot — the live value is strictly fresher and this is the smaller branch; the snapshot fallback is kept for callers without the getter (existing specs prove it).
- Tracked post-merge follow-up (G2 spill clause, do not drop): the live re-query is a window-narrowing placeholder, NOT the fix — CardanoNode._isDead (:935-936) confirms only IPC-disconnect + process-exit, so a re-queried state can read STOPPED while immutable-file/ledger handles are still releasing. The deep fix needs a bounded settle verifying ImmutableDB lock/immutable-dir release (a lock-release signal from CardanoNode) before handlers.start() spawns the download child.

Code-Review (pass 1):
Timestamp: 2026-07-12T19:55:54Z
Scope reviewed: full working-tree diff (41 files, nothing committed) vs task-ux-706.md + CAT docs A-G; all seven CAT impl entries above cross-checked against the diff.
Findings — verification re-run independently:
- Compile/tests: tsc --noEmit clean (verify-env sidecars present: .cache/jest.scss-override.js + regenerated .scss.d.ts incl. the three new MithrilStepIndicator classes). Jest: 5 main suites 161 tests green (service 78, metadata, coordinator, controller, nodeStartup); 11 renderer suites 187 tests green with the scss sidecar. eslint on the 11 functionally changed files: 0 errors (pre-existing warnings only).
- prettier 2.1.2 (repo-local binary; npx resolves a newer version and reports false dirt): all touched files clean except MithrilPartialSyncStore.spec.ts and partialSyncErrorCopy.spec.ts — both classified pre-existing HEAD drift (identical hunk counts at HEAD via git show HEAD | prettier --stdin-filepath), matching the builders' classification.
- i18n integrity: en-US/ja-JP 1518/1518 lines, 1517 keys each, key order identical, every key line-parallel; {shortcut} byte-identical EN↔JA; four-source lockstep verified programmatically for all 20 changed/added keys; EN changed-key set = A2 x3 + A3 x4 + A4 x5 + B1 + B4 + C1 x2, JA = A1 + A3 x4 + A4 x5 + B1 x2 + B2 + B3 x2 + C1 x2 — exactly the plan inventory; noCertifiedRange pair removed and mithrilSyncInterrupt + 3 CAT-D keys added in all four sources. defaultMessages/translations diffs reconcile: net +2 ids (4 adds − 2 removes) plus tool-regenerated descriptor moves (DiagnosticsTimeStatusRow family — verified as moves, not net changes).
- Acceptance greps all clean: no Latin "Mithril Sync" in JA values, no 標準同期/Standard Sync/Node Sync, no bare ジェネシスから同期, no lowercase "Mithril sync" in EN (incl. the native dialog), no 先端/シャットダウン/bare 診断, no 高速同期 progress labels (feature title/accept keys untouched per CAT-A out-of-scope). NO_CERTIFIED_RANGE: zero references repo-wide.
- Locked invariants: diff contains no kill-switch, recovery-action, success-overlay-dismiss, or staged-restore touchpoints (grep-swept); no synthetic throughput/remaining-time/percent added to common types; the at/past variant renders only from the backend-computed gap<=0 flag threaded as an explicit boolean (no renderer threshold, no absence-inference — confirmation selects atOrPastSnapshot regardless of epochs, probe-failed outranks it); empty-chain bootstrap flow untouched and its error-default spec green; getNodeState is main-internal (no IPC contract change); MithrilPartialSyncAvailability gains one optional field (backward-compatible).
- Locked EN strings (D1 tooltip + confirmation, D2 caution) byte-for-byte from the CAT-D doc; JA drafts match the doc drafts; B4 both shortcut branches asserted (Ctrl + D existing, Cmd + D new).
- Scope hygiene: every changed file maps to a CAT scope; .gitignore mtime is 2026-07-09 — three days before the wave (builders 18:38–19:38Z) — a pre-existing local tooling change, NOT wave-touched; it should not ride into the feature PR commit unless deliberately intended.
Non-blocking observations (tracked, no action required this wave):
- task-ux-706.md still reads "Build status: not-started" — stale now that CAT A–G have landed; scribe/orchestrator to flip.
- Outstanding master-doc process steps outside the CAT builders' scope: smoke-test cheat-sheet replay-button step + DD-706-1 vocabulary re-sync (Extra #7), nix fmt before push (gate 5), JA round-2 package hand-off (DD-706-10). All already logged as tracked in the master doc — none silently dropped.
- G2's live re-query is recorded (correctly) as a window-narrowing placeholder with the deep lock-release fix as a tracked follow-up, per the locked scope.
Decision: approved

Scribe — documentation close-out:
Timestamp: 2026-07-12T20:02:33Z
Files changed:
- .agent/plans/mithril-partial-sync/task-plans-ux-refinement/phase-7/task-ux-706.md (Build status flipped to completed 2026-07-12; Final outcome section added covering what landed per CAT, the JA round-2 flags, and the tracked follow-up outcomes; Round-2 hand-off section gained the produced-2026-07-12 note)
- .agent/plans/mithril-partial-sync/mithril-partial-sync-ux-refinement-tasks.json (minimal task-ux-706 entry added to phase-7 with status completed / completedAt 2026-07-12T20:02:33Z — the tracker had stopped at task-703 with no 704/705/706 entries, so the entry had to be created rather than updated; metadata/summary totals deliberately left untouched per the change-nothing-else constraint; JSON re-validated with a parse)
- .agent/plans/mithril-partial-sync/task-plans-ux-refinement/phase-7/task-ux-706-research.md (created — durable findings from the wave: i18n:manage full-sync semantics incl. auto-delete + auto-add, S3 sequencing lesson, broken npx / repo-local prettier rule, CardanoNode._isDead lock-release gap, C3 chunk-count rendering proof, CAT-E rp-button custom-property override technique, isAtOrPastSnapshot constant-shape contract, S7 drift re-resolve predicate rule)
- .agent/plans/mithril-partial-sync/mithril-partial-sync-ja-copy-table.md (regenerated per DD-706-10 to the full current-state EN↔JA reference: 78 rows with keys, DD-706-1 vocabulary header, noCertifiedRange rows dropped with a purge note; all values copied verbatim from the working-tree locale files)
- .agent/plans/mithril-partial-sync/mithril-partial-sync-ja-copy-review-round-2.md (created — the round-2 delta review doc: 4 new first-pass drafts, 17 revised entries (13 EN+JA, 4 JA-only), 3 EN-only FYI rows, linked-term groups 台帳状態/データの完全性/最新ブロック/ブロックチェーン同期, the optional recommendationUnknown softening question, the six §9 chain-storage confirm-if-unreviewed strings, and the §7.5/§7.6 TM purge note; round-1 typography flags excluded per DD-706-10 closed-silently; round-1 doc left intact as historical record)
Verification run:
- Master doc round-2 prescription checked against :358-376 before acting: the package is due "after CAT-A–F land", which the wave satisfies (implemented, verified, code-review approved on the working tree) — so the package was produced now, not deferred post-merge.
- Delta wordings sourced from git diff HEAD on en-US.json/ja-JP.json (prior wording = HEAD side); current wordings read from the working-tree locale files; tasks JSON parses clean after the edit; review logs otherwise untouched (no rewrite of earlier entries).
Deviations / handoffs:
- Tasks-JSON conflict resolved in favor of the live repo: the prescribed "set status on the task-706 entry" was impossible as written (entry absent); a minimal entry was added instead and the gap (704/705 also missing, tracker stale since 2026-07-03 v1.12.0) recorded in the entry itself and the master doc's final outcome.
- task-ux-706-research.md did not exist and the 704/705 convention had lapsed; created anyway because the wave produced durable findings that would otherwise live only in this log.
- The round-2 review doc was created as a new file (mithril-partial-sync-ja-copy-review-round-2.md) rather than overwriting the round-1 doc — "replacing the round-1 format" was read as a format instruction (delta vs full listing), and the round-1 doc remains the record of what was already validated.
- Still open after this close-out (unowned by scribe): sending the round-2 package to the translator, cheat-sheet replay-button step + vocabulary re-sync (Extra #7), nix fmt + full-tree gates at push time, and the decision on the pre-existing .gitignore modification.

Commit stage — single task commit:
Timestamp: 2026-07-12T20:11:25Z
Files changed:
- this log only (entry appended before committing so the commit contains the full stage record; the commit hash is therefore reported in the orchestration return, not here)
Staged and committed: all modified source/, storybook/, translations/ files; locale + generated i18n files (en-US.json, ja-JP.json, defaultMessages.json, translations/messages.json); .agent task-706 docs (master + CAT plan docs, impl-review log, research notes, tasks JSON) plus the two wave-produced copy docs outside phase-7 (mithril-partial-sync-ja-copy-table.md regenerated per DD-706-10, mithril-partial-sync-ja-copy-review-round-2.md) — both verified by diff/content as products of this wave before staging.
Verification run: git diff --cached --name-only checked against the porcelain inventory before committing; git log -1 and git status after. No compile/jest re-run — no source changed at this stage and code-review pass 1 above already ran the full verification with sidecars and approved.
Deviations / handoffs:
- .gitignore left uncommitted on purpose: pre-existing local agent-tooling ignores (mtime 2026-07-09, before the wave), flagged by code-review pass 1 as not belonging to the feature commit; decision on it remains with the branch owner.
- Repo state conflict vs the stage brief: the brief said nothing was staged, but the five phase-7 plan docs were already in the index at start (prior attempt or planning step); they were verified task-relevant and kept.

Post-review fix wave — consolidated-audit remediation (amended into the task commit):
Timestamp: 2026-07-13T09:58:13Z
Files changed:
- source/main/mithril/mithrilPartialSyncNodeStartup.ts (dialog title + message: "Mithril partial sync" → "Mithril Sync" per DD-706-1; grilled decision extends A5 beyond DD-706-9's casing-only scope. Wipe-log reason at :118 and post-cutover error strings deliberately unchanged — raw-diagnostics surfaces)
- source/renderer/app/components/loading/mithril-bootstrap/MithrilStepIndicator.tsx (showMoveCaution now keys on install-snapshot OR installing when active — the partial-sync cutover reports item 'installing', so the caution previously never covered the actual destructive window)
- source/renderer/app/components/loading/mithril-bootstrap/MithrilStepIndicator.spec.tsx (new spec: caution attaches to the active 'installing' stage item in the partial-sync item shape)
- source/renderer/app/components/loading/mithril-bootstrap/MithrilBootstrap.messages.ts (three translator-facing descriptions: two "fast sync" → "ledger state" per DD-706-2, one "full Mithril sync" casing)
- source/renderer/app/i18n/locales/defaultMessages.json + translations/messages.json (the three descriptions mirrored; JSON parse re-validated; no defaultMessage changed, so locale files untouched)
- .agent/plans/mithril-partial-sync/mithril-partial-sync-smoke-test-cheat-sheet.md (§5 quit-during-cutover: dialog title + button casing updated; stage corrected to "Installing snapshot..." — the blocking dialog maps to marker cutover-in-progress only, verified against MithrilPartialSyncService.ts:277-285; quitting during Finalizing resumes normally)
- .agent/plans/mithril-partial-sync/task-plans-ux-refinement/phase-7/task-ux-706.md (Post-review fix wave section: grilled decisions D1-D3/D5 recorded)
- .agent/plans/mithril-partial-sync/mithril-partial-sync-ux-refinement-tasks.json (the staged v1.13.0 backfill rides the amend per grill decision D5)
Verification run:
- MithrilStepIndicator.spec.tsx 19/19 (incl. the new installing-caution test); mithrilPartialSyncNodeStartup.spec.ts 15/15; tsc --noEmit exit 0; eslint 0 errors on touched files (4 pre-existing signature warnings); both patched JSONs parse.
- prettier: MithrilStepIndicator.tsx flags only the pre-existing STATUS_TO_STEP generic drift (:44-47), byte-identical failure at HEAD — left per the oscillation rule; all other touched files clean.
Deviations / handoffs:
- keepInstallingActiveDuringFinalizing over-extension (caution persists through bootstrap cleanup) deliberately kept — pre-dates the wave, errs conservative.
- S7 drift re-resolve and G2 restart/wipe live-getter seams remain deferred as tracked in the master doc; the Cmd + D test misattribution existed only in the implementer chat summary (repo docs already correct).
Decision: approved for amend
