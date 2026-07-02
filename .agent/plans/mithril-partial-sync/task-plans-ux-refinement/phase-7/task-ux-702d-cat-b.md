# task-ux-702d-cat-b — proactive-prompt handoff note: point to "Daedalus Diagnostics screen under the Help menu (Ctrl + D)"

> Per-category doc, decomposed from the canonical plan `task-ux-702d.md` (task-ux-702d = Mithril Sync UX
> finalization cleanup, ad-hoc wave). **Implementation record — as landed (2026-07-01).** Parent task:
> `task-ux-702d`. Plan review: `task-ux-702d-plan-review.md`. Research: `task-ux-702d-research.md`. If this doc ever
> disagrees with live code, prefer live code and reconcile here.

## Sequencing / status position
Second landed CAT of the wave. Copy/i18n-only; no file collisions with any other CAT (the message key and its locale
entries are self-contained).

## Findings closed & decisions implemented
| Finding # | Finding (short) | CAT | Severity |
|---|---|---|---|
| #2 | **Handoff note under-specifies where to restart Mithril Sync**: the proactive-prompt handoff note body (`SyncingConnectingMithrilPrompt.tsx:41-42` `promptHandoffNote`, rendered at `:185`) named only "the Diagnostics screen" — no app qualifier and no keyboard shortcut, so a user who skips has no concrete way to find the restart entry point. Now points to "the Daedalus Diagnostics screen under the Help menu (Ctrl + D)". Copy/i18n-only; the separate bold "Note:" label span (`promptHandoffNoteLabel` `:46-51`, rendered `:183`) is untouched. | **CAT-B** | Low |

## Locked invariants this change must NOT break
- **Vocab guardrail #8** — Mithril-Sync vocabulary preserved; no "partial sync"/%/immutable strings introduced.
- The separate bold "Note:" label span (`promptHandoffNoteLabel`) is untouched.
- **Offer-signal boundary #4** — the prompt's gating/suppression logic is untouched.

## Exact files (full repo-relative paths)
- `source/renderer/app/components/loading/syncing-connecting/SyncingConnectingMithrilPrompt.tsx`
- `source/renderer/app/components/loading/syncing-connecting/SyncingConnectingMithrilPrompt.spec.tsx`
- `source/renderer/app/i18n/locales/en-US.json`
- `source/renderer/app/i18n/locales/ja-JP.json`
- `source/renderer/app/i18n/locales/defaultMessages.json` (regenerated via `yarn i18n:manage`)
- `translations/messages.json` (regenerated via `yarn i18n:manage`)

Implementable from this section alone. Copy/i18n-only; no store, IPC, backend, layout, or gating change.

### Rationale
When the user picks **Standard Sync (slow)** (dismiss) on the proactive prompt, the choice-view handoff note is the
only place that tells them Mithril remains available later. The note body said only "…from the Diagnostics screen":
no app qualifier and no route to get there. Diagnostics is not a first-class visible destination — it lives under the **Help** menu and is also reachable by
the `Ctrl + D` shortcut — so a user who skips is left without a concrete way back to the restart entry point. Naming
the surface fully ("the Daedalus Diagnostics screen"), stating where to find it ("under the Help menu"), and citing
the shortcut ("(Ctrl + D)") makes the fallback actionable. This is a pure wording change to one message; the note's structure (a bold "Note:" label span plus this
body span) and the prompt's gating/flow are unchanged.

### Reference details (verified against the live post-702c tree, 2026-07-01)
- **Message key:** `daedalus.diagnostics.dialog.mithrilProactivePromptHandoffNote` (body text only — the word "Note:"
  is a *separate* key/span, so it is not in this string).
- **Component `defineMessages` (carries `!!!`):**
  `source/renderer/app/components/loading/syncing-connecting/SyncingConnectingMithrilPrompt.tsx:39-45`
  (`promptHandoffNote`; the `defaultMessage` literal is `:41-42`). Rendered in the choice view at `:185`
  (`<span>{intl.formatMessage(messages.promptHandoffNote)}</span>`), preceded by the bold label span `:183` fed by the
  distinct `promptHandoffNoteLabel` key (`:46-51`, `!!!Note:`) — **not** touched by CAT-B.
- **EN locale (no `!!!`):** `source/renderer/app/i18n/locales/en-US.json:171`.
- **JA locale:** `source/renderer/app/i18n/locales/ja-JP.json:171` (the key is translated here, *not* whitelisted —
  it is absent from `whitelist_ja-JP.json`).
- **Generated defaultMessages (carries `!!!`):** `source/renderer/app/i18n/locales/defaultMessages.json:3131`
  (regenerated from the tsx by `yarn i18n:manage`; see i18n below).
- **Spec assertion (exact old string):**
  `source/renderer/app/components/loading/syncing-connecting/SyncingConnectingMithrilPrompt.spec.tsx:76`
  (inside the "shows the capitalized \"Mithril Sync\" handoff note in the choice view" test, `:70-79`). This was the
  only spot in the spec asserting the sentence.

### Implementation (as landed)
Change only the sentence, keeping the `!!!` prefix where the file convention carries it:
- `SyncingConnectingMithrilPrompt.tsx:41-42` — `defaultMessage` →
  `'!!!If skipped, you can still start the Mithril Sync from the Daedalus Diagnostics screen under the Help menu. (Ctrl + D)'`.
- `en-US.json:171` (no `!!!`) →
  `"If skipped, you can still start the Mithril Sync from the Daedalus Diagnostics screen under the Help menu. (Ctrl + D)"`.

The bold "Note:" label span, the prompt's gating/suppression logic, and all layout/SCSS are left untouched. Post-edit
`grep` for the old fragment `from the Diagnostics screen` across `source/` is clean (no stale copy).

### i18n
- **EN:** updated at `en-US.json:171` (verbatim, above).
- **JA:** updated at `ja-JP.json:171` →
  `"スキップした場合でも、ヘルプメニューにあるDaedalus Diagnostics画面からMithril同期を開始できます。(Ctrl + D)"`. Follows the existing
  JA phrasing (`スキップした場合でも、…画面から…開始できます。`) and the Mithril-Sync glossary (`Mithril同期`); keeps
  the proper noun "Daedalus" and the shortcut literally as `(Ctrl + D)`. Key is translated in `ja-JP.json`, not
  whitelisted (`whitelist_ja-JP.json` has no entry for it), so no whitelist edit is needed.
- **Generated `defaultMessages.json`:** regenerated via **`yarn i18n:manage`** (ran to completion, exit 0, ~26s in
  this environment). The regen touched exactly one entry — `:3131` now carries the new `!!!`-prefixed body — and left
  the file otherwise byte-identical (`git diff` = 1 line changed). No hand-edit fallback was needed.

### Tests
- **Spec assertion updated:** `SyncingConnectingMithrilPrompt.spec.tsx:76` now asserts the new body string verbatim
  (`If skipped, you can still start the Mithril Sync from the Daedalus Diagnostics screen under the Help menu. (Ctrl + D)`). No other
  assertion in the spec referenced the old sentence. Per the Node v24 renderer verify-env caveat, the full jest/tsc
  suite was intentionally **not** run for this copy change — the assertion-text update is sufficient.
- **Manual / Storybook proof:** on a significantly-behind node, open the proactive prompt and read the choice-view
  handoff note; confirm it renders "**Note:** If skipped, you can still start the Mithril Sync from the Daedalus
  Diagnostics screen under the Help menu. (Ctrl + D)" with the bold "Note:" label intact. Storybook:
  `storybook/stories/loading/mithril/MithrilPartialSyncDialogue.stories.tsx` renders the choice view; verify the new
  wording. No new "partial sync"/%/immutable vocabulary is introduced.

## Risks (category-specific)
None recorded (copy/i18n-only).

## Operator / verify-only gates
Covered by the Tests subsection above (no separate master Verification-plan bullet existed for CAT-B).

## Cross-category coupling notes
None (the message key is self-contained).
