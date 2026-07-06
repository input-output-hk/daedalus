# task-ux-704 — CAT-F per-section plan: Storybook & fixture hardening (F-1, F-2, F-3, F-4)

> Per-CAT implementation doc for task-ux-704. **Self-contained — implementable from this doc
> alone by an implementer who makes zero decisions.** Parent: `task-ux-704.md` (findings F7
> export-variant → F-1, F8 → F-2, F6 → F-3, F12 → F-4; all CONFIRMED). If this doc ever
> disagrees with live code, prefer live code, locate anchors by the QUOTED snippets (never by
> line number), and reconcile here. All line numbers below are approximate as of 2026-07-04
> (pre-CAT-E) and are a reading aid only.

## Sequencing position and seam contract S3

CAT-F runs **after CAT-E** (wave order A → H). Seam S3 (master doc): CAT-E deletes the
`showMithrilPartialSyncConfirmationOnOpen` pipeline (renderer Props + pass-through +
`Diagnostics.stories.tsx` confirmation-story seam) and reworks that story; CAT-F then retypes
the same stories. Coordinate, don't double-edit:

- **Hard gate before starting F-3:** `git grep -n showMithrilPartialSyncConfirmationOnOpen`
  must return zero hits in `source/` and `storybook/`. If it still hits, CAT-E has not landed —
  STOP and escalate (see E1). Do not delete the prop yourself.
- F-3 retypes whatever the stories file contains post-CAT-E. The typed `baseProps` must not
  reference the deleted prop; if CAT-E's rework moved or renamed the confirmation stories,
  re-locate every F-3 anchor by quoted content and apply the same mechanics.
- F-1/F-2/F-4 touch files CAT-E does not own; they are unaffected by S3.

## Conventions (wave-wide, restated as binding here)

- **One commit for all of CAT-F.** Conventional Commits, subject line only — no body, no
  trailer. Suggested subject:
  `refactor(mithril): harden partial sync storybook stories and fixtures`
- **Node v24 prep before trusting tsc/jest:** regenerate `.scss.d.ts` via `typed-scss-modules`
  and apply the gitignored jest `identity-obj-proxy` sidecar first (repo verify-env note).
  `tsconfig.json` has no `include` and excludes only `node_modules`, so `yarn compile` DOES
  typecheck `storybook/**` — the F-2/F-3 typing below is enforced by the compile gate.
- **Do not reformat unrelated hunks.** prettier 2.1.2 oscillates; before blaming an edit,
  classify against pre-existing HEAD drift: `git show HEAD:<f> | prettier --stdin-filepath <f>`.
  Never reformat `toHaveBeenCalledWith('str', {obj})` call shapes.
- **Rollback:** revert the CAT-F commit (`git revert <sha>`). No migration state.
- Behavior gate: no IPC names, no emitted status shapes, no user-visible copy changes. The
  production-file edits here (F-1 `export` keyword; F-3 Props type line plus removal of the
  suppression that line makes stale) are type/visibility only.
- No task/finding IDs (F-1, CAT-F, …) in source comments or test titles.

## i18n

**No i18n changes in CAT-F.** No new message ids, no `en-US.json`/`ja-JP.json` edits, no
defaultMessages regeneration. F-2 deliberately avoids importing locale strings (rationale in
section 2).

---

# 1. F-1 — Export the download-progress anchor id; import it at all 6 copy sites

**Defect:** `MithrilStepIndicator.tsx` (~:104) declares the module-private constant that gates
the combined download progress bar:

```ts
const DOWNLOAD_PROGRESS_ANCHOR_ID = 'step-3';
```

The literal `'step-3'` is hand-copied at 6 sites: `MithrilPartialSyncOverlay.stories.tsx` ~:82
and `mithrilFixtures.ts` ~:165, :182, :199, :236, :278. The stories file's own comment (~:60-65)
records that a wrong anchor id previously silently dropped the progress bar — the exact rot this
change prevents.

**Decision already made (do not revisit):** the "typed field on `MithrilProgressItem`" variant
was REJECTED (master doc, refuted list) — it ripples into both main-process emitters (which
synthesize `step-N` ids from CLI output) and the IPC types. Export-constant variant only.

### Step 1.1 — Export the constant

File: `source/renderer/app/components/loading/mithril-bootstrap/MithrilStepIndicator.tsx`

Locate (exact):

```ts
const DOWNLOAD_PROGRESS_ANCHOR_ID = 'step-3';
```

Replace with:

```ts
export const DOWNLOAD_PROGRESS_ANCHOR_ID = 'step-3';
```

Leave the in-module literals at ~:72 (`DOWNLOAD_SUB_IDS` set) and ~:91 (`ITEM_ID_TO_MESSAGE`
key) as literals — they precede the declaration (rewiring them means reordering module-scope
consts for TDZ) and they enumerate the full sub-id vocabulary, not hand-copies of the anchor.

### Step 1.2 — Overlay stories: import + use

File: `storybook/stories/loading/mithril/MithrilPartialSyncOverlay.stories.tsx`

(a) Locate the existing component import:

```ts
import MithrilPartialSyncOverlay from '../../../../source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay';
```

Insert immediately after it:

```ts
import { DOWNLOAD_PROGRESS_ANCHOR_ID } from '../../../../source/renderer/app/components/loading/mithril-bootstrap/MithrilStepIndicator';
```

(b) In `getDownloadingProgressItems`, locate the third item's id line:

```ts
    id: 'step-3',
```

Replace with:

```ts
    id: DOWNLOAD_PROGRESS_ANCHOR_ID,
```

(c) The block comment above `getDownloadingProgressItems` (begins
`// downloading frame: the download phase's disk-check + certificate-chain sub-steps`) warns
about the hand-copy hazard this step removes. Replace that whole comment (through the line
ending `Labels are still pulled from intl at render (never module scope).`) with:

```ts
// downloading frame: disk-check + certificate-chain completed, snapshot-download
// anchor active. The active id must be the imported DOWNLOAD_PROGRESS_ANCHOR_ID
// or MithrilStepIndicator silently drops the combined progress bar.
```

This replacement supersedes the CAT-H inventory's :60-65 rewrite (cross-referenced in its
mooted list).

Leave the later prose comment mentioning "the `step-3` anchor" (above the
`'Download Progress Bar (Partial)'` story) untouched — CAT-H owns the comment inventory (seam
S4; it re-locates entries by quoted text).

### Step 1.3 — Fixtures: import + 5 uses

File: `storybook/stories/loading/_support/mithrilFixtures.ts`

(a) Locate the end of the type-import block:

```ts
} from '../../../../source/common/types/mithril-bootstrap.types';
```

Insert immediately after it:

```ts
import { DOWNLOAD_PROGRESS_ANCHOR_ID } from '../../../../source/renderer/app/components/loading/mithril-bootstrap/MithrilStepIndicator';
```

(b) Replace every `id: 'step-3',` in `progressPresetMap` (5 occurrences: presets
`download-early`, `download-mid`, `verifying`, `finalizing`, `finalizing-with-conversion`) with:

```ts
      id: DOWNLOAD_PROGRESS_ANCHOR_ID,
```

Import-graph note (verified): `mithrilFixtures.ts` is imported only by storybook files
(5 stories + `mithrilHarness.tsx`, 6 files total); no jest spec imports it, so pulling the component module
(with its `.scss`/`.svg` imports) into the fixtures cannot affect jest. Storybook webpack
already bundles those asset types for every story.

**Consciously untouched `'step-3'` literals:** `tests/mithril/e2e/steps/mithril-bootstrap.ts`
(~:112, :122 — cucumber fixtures; importing a renderer component constant into e2e steps is not
wanted) and the three spec files (`MithrilBootstrapService.spec.ts`,
`MithrilPartialSyncOverlay.spec.tsx`, `MithrilStepIndicator.spec.tsx`) which pin the wire ids
the main process emits.

---

# 2. F-2 — Copy-proof confirm-view seam in the dialogue stories (no prod change)

**Defect:** `MithrilPartialSyncDialogue.stories.tsx` (~:16-35) reaches the confirm view by
DOM-querying `button.textContent === 'Mithril Sync (fast)'` and clicking in a `useEffect` — a
copy change silently renders the wrong state.

**Chosen variant (decision made, do not revisit): select via the component's own imported
`.scss` module class.** The locale-string variant (importing the en-US string for
`daedalus.diagnostics.dialog.mithrilProactivePromptMithrilButton`) was rejected because:

- The global `StoryWrapper` renders stories under the DaedalusMenu locale switcher
  (`translations[locale]`, en-US **and** ja-JP, remounting via `key: locale`) — an en-US string
  match still silently renders the choice view under ja-JP.
- The `messages` object in `SyncingConnectingMithrilPrompt.tsx` is module-private
  (`defineMessages` result is not exported), so only a raw id string into `en-US.json` is
  possible — which rots on the deferred E9i id-namespace rename.

The class selector is copy- and locale-independent. Precedent for a story importing a component
`.scss`: `TopBarEnvironment.stories.tsx` (~:10, `TopBar.scss`). The class exists at
`SyncingConnectingMithrilPrompt.scss` ~:62 (`.primaryAction`) and is applied to exactly one
button in the choice view (the "Mithril Sync (fast)" action,
`SyncingConnectingMithrilPrompt.tsx` ~:196-201). `.scss.d.ts` typings are gitignored and
regenerated by the Node v24 prep, so `yarn compile` will see `primaryAction`.

### Step 2.1 — Rewire `ConfirmViewPrompt`

File: `storybook/stories/loading/mithril/MithrilPartialSyncDialogue.stories.tsx`

(a) Locate:

```ts
import SyncingConnectingMithrilPrompt from '../../../../source/renderer/app/components/loading/syncing-connecting/SyncingConnectingMithrilPrompt';
```

Insert immediately after it:

```ts
import styles from '../../../../source/renderer/app/components/loading/syncing-connecting/SyncingConnectingMithrilPrompt.scss';
```

(b) Locate the whole `useEffect` body inside `ConfirmViewPrompt`:

```ts
  useEffect(() => {
    const actionButton = Array.from(
      containerRef.current?.querySelectorAll('button') || []
    ).find((button) => button.textContent === 'Mithril Sync (fast)');

    actionButton?.click();
  }, []);
```

Replace with:

```ts
  useEffect(() => {
    // Reach the confirm view through the real click path. Selecting by the
    // component's own .scss class stays truthful across copy edits and locale
    // switches; a missing button fails loudly instead of showing choice view.
    const actionButton = containerRef.current?.querySelector<HTMLButtonElement>(
      `button.${styles.primaryAction}`
    );
    if (!actionButton) {
      throw new Error('confirm-view story: primary action button not found');
    }
    actionButton.click();
  }, []);
```

The throw is deliberate: if the class (or the button) ever disappears, the story errors visibly
in the preview pane instead of silently rendering the choice view — the same silent-rot failure
mode F-2 exists to remove. No production file changes in this item.

---

# 3. F-3 — Typed Diagnostics stories (post-CAT-E)

**Defect:** `Diagnostics.stories.tsx` casts the entire `baseProps` object `as any` (~:74) and
gives `renderConfirmationStory` an `: any` return (~:85). Verified (2026-07-04) the casts hide:

- (a) the REQUIRED prop `isMithrilPartialSyncProbeFailed: boolean;`
  (`DaedalusDiagnostics.tsx` ~:414, non-optional) is missing — stories render the probe branch
  (`isProbeFailed={...}` pass-through at ~:727) with `undefined`;
- (b) `onRestartNode` is passed as an action object `{ trigger: ... }` (~:68-70) against a Props
  declaration of a bare function (`onRestartNode: (...args: Array<any>) => any;`, ~:420) — a
  **pre-existing Props inaccuracy**: the component's sole use is
  `this.props.onRestartNode.trigger();` (~:914) and the only production caller passes the
  `restartNode` Action instance (`DaedalusDiagnosticsDialog.tsx` ~:158; `Action` has `trigger`,
  `actions/lib/Action.ts` ~:33). The `{trigger}` shape IS the real runtime contract, so the
  Props type is fixed (no per-prop cast kept);
- (c) `coreInfo.daedalusProcessID: 98954` / `daedalusMainProcessID: 82734` are numbers where
  `CoreSystemInfo` declares `string` — surfaced the moment `baseProps` is typed;
- (d) `renderConfirmationStory` returns a TWO-parameter function reading `props.currentTheme`
  from the second parameter — that slot is React's legacy-context object (`{}` here), while
  `StoryWrapper` passes `currentTheme` as a **prop** (first parameter). The theme-remount
  `key` has therefore always been `undefined`; the `: any` hid the mis-read.

Why the pattern compiles (verified against the installed typings — do NOT cite
`Staking.stories.tsx` ~:166 as precedent: that registration sits under a stale ts-migrate
`@ts-ignore` at ~:165, so it proves nothing): `StoryApi.add` accepts `StoryFn`, whose
`ArgsStoryFn` branch is `(args: TArgs, context) => …` with `TArgs = Args = { [name: string]:
any }` (@storybook/csf typings shipped with @storybook/react 6.4.22), and the repo compiles
with `strict: false` (so no `strictFunctionTypes`) — a `(props: { currentTheme: string }) =>
JSX.Element` registration therefore typechecks, confirmed by a scratch compile against the
installed typings. The stale ts-migrate `@ts-ignore` comments in
`TopBarEnvironment.stories.tsx` (and the Staking one) are not counter-examples — do not add
ignores.

### Step 3.1 — Type `baseProps` and fill the gaps

File: `storybook/stories/nodes/status/Diagnostics.stories.tsx`

(a) Locate `import React from 'react';` and insert after it:

```ts
import type { ComponentProps } from 'react';
```

(b) In `coreInfo`, locate:

```ts
  daedalusProcessID: 98954,
  daedalusMainProcessID: 82734,
```

Replace with (rendering output is the same digits; `CoreSystemInfo` declares both as `string`):

```ts
  daedalusProcessID: '98954',
  daedalusMainProcessID: '82734',
```

(c) Locate `const baseProps = {` and replace with:

```ts
const baseProps: ComponentProps<typeof DaedalusDiagnostics> = {
```

(d) Locate `  isMithrilPartialSyncSignificantlyBehind: true,` and insert after it:

```ts
  isMithrilPartialSyncProbeFailed: false,
```

(`false` preserves what the stories render today: the CTA-ready/blocked branches, not the
probe-failure banner.)

(e) Locate the object's closing `} as any;` and replace with:

```ts
};
```

Keep `cardanoNodeState: 'running' as const` as-is. Verified 2026-07-04: after (b) and (d) the
literal satisfies the Props type completely — every other required prop is already present and
correctly shaped (`systemInfo`/`TipInfo`/handlers all match). If `yarn compile` surfaces any
OTHER mismatch in this object, that is post-verification drift — see E3, do not improvise.

### Step 3.2 — Fix the `onRestartNode` Props inaccuracy at its source

File: `source/renderer/app/components/status/DaedalusDiagnostics.tsx`

(a) Locate (inside `type Props = {`):

```ts
  onRestartNode: (...args: Array<any>) => any;
```

Replace with:

```ts
  onRestartNode: { trigger: (...args: Array<any>) => any };
```

(b) Locate (inside the `restartNode = () => {` method, ~:913, directly above the
`this.props.onRestartNode.trigger();` call):

```ts
    // @ts-ignore ts-migrate(2339) FIXME: Property 'trigger' does not exist on type '(...arg... Remove this comment to see the full error message
```

Delete that line (keep the `this.props.onRestartNode.trigger();` line beneath it unchanged).
Once Props declares the `{ trigger }` shape, this suppression suppresses nothing and its FIXME
text asserts the opposite of the new declaration; CAT-H classifies this file's pre-existing
comments as no-action, so the stale line must be removed here or it ships.

Scope guard: these two lines are the ONLY lines CAT-F changes in this file. Do not touch the
`onRestartNode` props of `NotResponding.tsx` / `WalletWithNavigation.tsx` — those components
invoke it as a function (correct as declared). The production caller
(`DaedalusDiagnosticsDialog.tsx`, Action instance) and the existing spec stub
(`DaedalusDiagnostics.spec.tsx` ~:61, `{ trigger: jest.fn() } as any`) both satisfy the new
shape; the spec's now-redundant cast stays (specs are not in CAT-F scope).

### Step 3.3 — Type the confirmation fixtures and story factory

File: `storybook/stories/nodes/status/Diagnostics.stories.tsx`

Locate the block from `const confirmationBaseProps = {` through the closing `};` of
`renderConfirmationStory` (the function ending `    );\n  };`) and replace it with:

```ts
type ConfirmationProps = ComponentProps<typeof MithrilPartialSyncConfirmation>;

const confirmationBaseProps: ConfirmationProps = {
  isActionBlocked: false,
  startError: null,
  onCancel: action('onCancel'),
  onConfirm: action('onConfirm'),
};

// StoryWrapper hands currentTheme to the story as a prop (first parameter);
// keying the modal on it remounts per theme switch.
const renderConfirmationStory = (
  storyProps: Partial<ConfirmationProps> = {}
): ((props: { currentTheme: string }) => JSX.Element) =>
  function RenderConfirmationStory(props: { currentTheme: string }) {
    return (
      <MithrilPartialSyncConfirmation
        key={props.currentTheme}
        {...confirmationBaseProps}
        {...storyProps}
      />
    );
  };
```

Keep any comment CAT-E left above `confirmationBaseProps` if it is still accurate post-E;
delete it only if it still describes the removed `isShowingConfirmation`/on-open seam. The
three `.add(..., renderConfirmationStory(...))` call sites are unchanged (their argument
literals already satisfy `Partial<ConfirmationProps>`).

---

# 4. F-4 — Zero-safe numeric fallbacks in the overlay story

File: `storybook/stories/loading/mithril/MithrilPartialSyncOverlay.stories.tsx`

**Defect:** `||` treats `0` as unset, so zero-progress states are unexpressible (0 files
downloaded falls back to 7/9/645).

Locate (inside `transferProgress={{ ... }}`):

```ts
        filesDownloaded: props.filesDownloaded || baseProps.filesDownloaded,
        filesTotal: props.filesTotal || baseProps.filesTotal,
        elapsedSeconds: props.elapsedSeconds || baseProps.elapsedSeconds,
```

Replace with:

```ts
        filesDownloaded: props.filesDownloaded ?? baseProps.filesDownloaded,
        filesTotal: props.filesTotal ?? baseProps.filesTotal,
        elapsedSeconds: props.elapsedSeconds ?? baseProps.elapsedSeconds,
```

**Keep untouched:** the boolean trio `props.canRetry || false` /
`props.canRestartNormally || false` / `props.canWipeAndFullSync || false` (`||` is correct for
booleans), plus `props.error || null` and
`props.onDismissCompleted || baseProps.onDismissCompleted` (object/function-valued; not in
scope). No current story passes `0`, so all existing stories render byte-identically.

---

## Verification

Environment prep first (Node v24): regenerate `.scss.d.ts` via `typed-scss-modules`; apply the
jest `identity-obj-proxy` sidecar. Then:

```bash
yarn test:jest --testPathPattern "source/renderer/app/components/(status|loading/mithril-bootstrap)/.*\.spec\.tsx"
yarn compile
yarn lint
```

- Jest pass/fail counts must be unchanged (CAT-F edits no spec and no runtime logic; the
  covered specs are `MithrilStepIndicator`, `MithrilPartialSyncOverlay`, `MithrilBootstrap`,
  `MithrilProgressView`, `DaedalusDiagnostics`, `MithrilPartialSyncConfirmation`,
  `MithrilPartialSyncSection`).
- `yarn compile` covers `storybook/**` — it is the enforcement gate for F-2/F-3 typing.
- `yarn prettier:check` on the six touched files; classify any failure against HEAD drift
  (`git show HEAD:<f> | prettier --stdin-filepath <f>`) before attributing it to this CAT.
- Interactive `yarn storybook` inspection is **optional, author-run**: worth eyeballing
  "Downloading File Count", "Download Progress Bar (Partial)", both dialogue "Confirm View"
  stories (in en-US **and** ja-JP via the DaedalusMenu locale switcher), and the three
  Diagnostics stories across a theme switch (the confirmation modal now genuinely remounts).

## Files touched

- `source/renderer/app/components/loading/mithril-bootstrap/MithrilStepIndicator.tsx` — F-1
  (`export` keyword only)
- `source/renderer/app/components/status/DaedalusDiagnostics.tsx` — F-3 (one Props line + the
  stale `@ts-ignore ts-migrate(2339)` line above the `.trigger()` call)
- `storybook/stories/loading/mithril/MithrilPartialSyncOverlay.stories.tsx` — F-1, F-4
- `storybook/stories/loading/_support/mithrilFixtures.ts` — F-1
- `storybook/stories/loading/mithril/MithrilPartialSyncDialogue.stories.tsx` — F-2
- `storybook/stories/nodes/status/Diagnostics.stories.tsx` — F-3

No spec files, no i18n files, no main-process files.

## Out of scope (do NOT do in this wave)

- **F5** fixture literal-union typing — `satisfies` is blocked by prettier 2.1.2 /
  ts-eslint 5.20; post-merge candidate using bare inference or an identity helper.
- **F9a** harness validation-hook extraction (`mithrilHarness.tsx`).
- **F10** preset builder + duplicate preset (the byte-identical `download-early` /
  `download-mid` presets in `mithrilFixtures.ts` are F10's — leave both).
- **E9i** message-id namespace rename (would churn shipped ja-JP ids; also the reason F-2
  avoids raw message-id lookups).
- The rejected F7 typed-field variant (anchor id on `MithrilProgressItem`) — refuted in the
  master doc; do not re-raise.
- Spec hygiene (`DaedalusDiagnostics.spec.tsx` casts), e2e step literals, and the emitters'
  `step-N` synthesis.

## Acceptance checks

- **F-1:** `git grep "id: 'step-3'" storybook/` returns nothing;
  `DOWNLOAD_PROGRESS_ANCHOR_ID` is exported and imported at both storybook files; the
  "Download Progress Bar (Partial)" story still shows the combined bar.
- **F-2:** no copy-string DOM matching remains in `MithrilPartialSyncDialogue.stories.tsx`;
  both "Confirm View" stories reach the confirm view; the seam survives a locale switch.
- **F-3:** no `as any` / `: any` remains in `Diagnostics.stories.tsx`;
  `isMithrilPartialSyncProbeFailed: false` present in `baseProps`; `onRestartNode` Props line
  declares the `{ trigger }` shape and no `ts-migrate(2339)` suppression remains in
  `DaedalusDiagnostics.tsx`; `yarn compile` green with the stories in-program.
- **F-4:** the three numeric fallbacks use `??`; the three boolean `|| false` fallbacks are
  untouched; passing `filesDownloaded={0}` (spot check) renders a zero-progress bar.

## Escalations

- **E1 (S3 gate):** if `showMithrilPartialSyncConfirmationOnOpen` still exists anywhere in
  `source/` or `storybook/` when CAT-F starts, CAT-E has not landed — stop; do not remove it
  from here and do not type around it.
- **E2 (.add typing):** if `yarn compile` rejects `.add('…', renderConfirmationStory(...))`
  after Step 3.3 (not expected — the installed csf typings accept the pattern; see the typing
  note above Step 3.1), escalate with the
  compiler error. Do not reintroduce a whole-object cast and do not add `@ts-ignore`.
- **E3 (typed-baseProps drift):** the verified complete mismatch set for Step 3.1 is exactly
  the two `coreInfo` PID fields plus the missing probe prop. Any additional compile error in
  `baseProps` means the Props or story drifted after 2026-07-04 — reconcile to the
  Props-declared types keeping story rendering identical, and record the delta in the impl
  review.
- All quoted anchors above were re-verified against HEAD on 2026-07-04, including:
  `DOWNLOAD_PROGRESS_ANCHOR_ID` declaration and its 6 copy sites; the dialogue story's
  textContent query; `.primaryAction` in the prompt `.scss` and on the choice-view button;
  Props `:414/:420` and the `.trigger()` call site `:914`; `CoreSystemInfo` string PID fields;
  the `||` fallback trio.
