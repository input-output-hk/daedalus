# task-ux-704 — CAT-E plan: Renderer cleanups (E-1…E-5)

> Per-CAT implementation doc for task-ux-704 (code-quality remediation wave). **Self-contained —
> implementable from this doc alone by an implementer who makes zero decisions.** Parent:
> `task-ux-704.md` (traceability rows E2, D12, E9f/g/h, E9a/d). If this doc ever disagrees with
> live code, prefer live code, locate anchors by the QUOTED snippets (never by line number), and
> reconcile here. All line numbers below are approximate as of 2026-07-04 (verified against
> HEAD `7c9f7de20` + working tree) and are a reading aid only.

## Sequencing position

Fifth section (order A → B → C → D → **E** → F → G → H). Runs AFTER CAT-D and BEFORE CAT-F.

- **Seam S3 (E↔F, `Diagnostics.stories.tsx`):** E-1 deletes the
  `showMithrilPartialSyncConfirmationOnOpen` pipeline and reworks the "Partial Sync
  Confirmation" story's opening seam (Step 1.6). CAT-F retypes the same stories AFTERWARDS —
  do not add fixture typing, `satisfies`, or `as`-removals to the stories here beyond what
  Step 1.6 specifies; the new story code must merely compile.
- CAT-D, which runs earlier in this wave, will have tightened two files this CAT touches:
  `MithrilPartialSyncSection.tsx` (Props gains `onStartMithrilPartialSync: () =>
  Promise<void>`, its Step 5.1) and `DaedalusDiagnosticsDialog.tsx` (typed
  `shouldCloseDiagnosticsForPartialSyncOverlay` params). Non-overlapping lines; leave CAT-D's
  edits alone. Step 1.6's `async () => { … }` wrapper compiles against both the old
  `(...args: Array<any>) => any` signature and the tightened one, so this CAT is safe even if
  executed against a tree where CAT-D has not landed yet.
- CAT-H (last) re-locates comment-inventory entries by quoted text; deletions here (the story
  comment block, the section's `componentDidMount` branch) simply moot the corresponding
  entries — that is CAT-H's expected S4 outcome.

## Anchor corrections from plan-authoring verification (2026-07-04)

1. **Prop naming is layered, not uniform.** The pipeline is
   `showMithrilPartialSyncConfirmationOnOpen` only at the dialog/diagnostics layers
   (`DaedalusDiagnosticsDialog.tsx` ~:139, `DaedalusDiagnostics.tsx` ~:415/:530/:729-731); the
   section's own prop is **`showConfirmationOnOpen`** (`MithrilPartialSyncSection.tsx` ~:27,
   consumed ~:50), which is also the name PRD D13 and the specs use.
2. **The "4 deep-link specs (:264-303)" are really 3 + 1.** Three specs test exactly the
   deleted auto-open behavior and are dropped. The fourth (`'threads the epochs figure into
   the confirmation modal without any sync-%'`, ~:302) is epochs-threading coverage that
   merely USES the prop as a convenient opening seam — it is REWORKED onto the CTA-click seam
   (Step 1.4c), not dropped: it is the only spec pinning the section→confirmation
   `behindByEpochs` pass-through (`MithrilPartialSyncConfirmation.spec.tsx` covers the modal's
   copy, not the section's threading).
3. The confirmation story block spans ~:104-114 (comment ~:105-108 + `.add` ~:109-114), not
   :109-113.
4. The E-2 hand-built object spans `MithrilBootstrapStore.ts` ~:372-379 (method
   `returnToStorageLocation` opens at ~:356); the validate wrapper is ~:317-336 with the
   channel request at ~:319 and the channel import at ~:22 — as briefed.
5. **E-4's "same ids/defaultMessages today" is half-right:** the four ids match, but the
   `serviceUnreachable` defaultMessages have ALREADY drifted — canonical
   `'!!!NTP service unreachable'` (`DaedalusDiagnostics.tsx` ~:354-358) vs the row's inline
   `'!!!Service unreachable'` (`DiagnosticsTimeStatusRow.tsx` ~:82-85). Rendered copy is
   identical either way (react-intl resolves by id from the locale bundles — en-US :192
   `"NTP service unreachable"`, ja-JP :192), so consolidating onto the canonical descriptor
   changes nothing visible — this pre-existing drift is precisely the failure mode E-4 removes.

## Locked invariants that constrain CAT-E

- **PRD D13 (user sign-off 2026-06-25, `mithril-partial-sync-ux-refinement-prd.md`
  ~:687-714):** the proactive prompt's INLINE confirm view is the only deep-link-replacement
  surface (`MithrilProactivePromptContainer.tsx` ~:88-93 renders
  `SyncingConnectingMithrilPrompt`, whose `handleStart` ~:133-156 is the single `onStart` call
  site); the deep-link plumbing was recorded as "not built" and the section prop "left in
  place, unused, as a benign residual". Deleting the residual violates nothing — D13 documents
  it will never be wired — but this plan MUST update the D13 note to record the removal
  (Step 1.7).
- **Lock #3 (confirmation precedes start)** is untouched everywhere: E-1 deletes an
  auto-OPEN path for the confirmation, never a confirmation itself.
- **E-2 is the wave-sanctioned D12 defect fix**, the one place in this CAT where user-visible
  output may change — and only from wrong to right: the picker's helper copy for a previous
  custom path that already contains a chain subdirectory currently claims a subdirectory
  "will be created" (`chainSubdirectoryStatus: 'will-create'` hardcoded); after E-2 it reflects
  the real validation. No message TEXT changes; only which existing message is selected.
- Comments/test titles: never cite task/finding IDs (E-1, CAT-E, D12…); rationale comments
  below are written accordingly — copy them verbatim. No change-history comments.
- `import/no-cycle` is disabled (`.eslintrc` :31) — relevant to E-4's chosen mechanism.

## i18n

**No new message ids, no id renames, no defaultMessage changes, no locale-artifact edits**
(`en-US.json` / `ja-JP.json` / defaultMessages regeneration all untouched — if any step below
seems to require touching them, STOP; that step is out of scope as planned).

Two deliberate consequences of that rule:

- **E-3a orphans `subdirectoryWarningExists`** (`ChainStorage.messages.ts` ~:83, id
  `chainStorage.locationPicker.subdirectoryWarningExists`): the descriptor STAYS defined even
  though its last consumer is deleted. Removing it would churn extraction artifacts and
  shipped ja-JP copy for zero benefit; an unused descriptor is inert.
- **E-4 keeps the `defineMessages` block physically in `DaedalusDiagnostics.tsx`** and only
  adds `export`. The actual guarantee is narrower: no step in this plan regenerates i18n
  artifacts, and the `en-US.json` / `ja-JP.json` bundles are keyed by id, so all three
  artifacts stay byte-identical within this wave. Extraction itself IS per-file —
  `defaultMessages.json` holds a `DiagnosticsTimeStatusRow.tsx` descriptor group (with the
  drifted `"!!!Service unreachable"`, ~:1842) that the first regeneration after E-4 will
  drop, and the `DaedalusDiagnostics.tsx` group already carries pre-existing drift.

---

# E-1 — Delete the dead `showMithrilPartialSyncConfirmationOnOpen` pipeline

**Evidence (verified):** the only production write is the hardwired `false` at
`DaedalusDiagnosticsDialog.tsx` ~:139; it threads
`DaedalusDiagnostics.tsx` (Props ~:415 → destructure ~:530 → pass-down ~:729-731) into
`MithrilPartialSyncSection.showConfirmationOnOpen` (~:27), whose only effect is the
`componentDidMount` auto-open (~:50-52). Genuinely dead end-to-end: no dialog-open payload
ever sets it, and PRD D13 superseded the deep-link with the prompt's inline confirm.

### Step 1.1 — `source/renderer/app/containers/status/DaedalusDiagnosticsDialog.tsx`

Delete the line:

```tsx
          showMithrilPartialSyncConfirmationOnOpen={false}
```

### Step 1.2 — `source/renderer/app/components/status/DaedalusDiagnostics.tsx`

(a) In `Props`, delete:

```ts
  showMithrilPartialSyncConfirmationOnOpen?: boolean;
```

(b) In the render destructuring, delete:

```ts
      showMithrilPartialSyncConfirmationOnOpen,
```

(c) In the `<MithrilPartialSyncSection` JSX, delete the pass-down:

```tsx
                  showConfirmationOnOpen={
                    showMithrilPartialSyncConfirmationOnOpen
                  }
```

### Step 1.3 — `source/renderer/app/components/status/MithrilPartialSyncSection.tsx`

(a) In `Props`, delete:

```ts
  showConfirmationOnOpen?: boolean;
```

(b) Locate:

```ts
  componentDidMount() {
    this._isMounted = true;
    if (this.props.showConfirmationOnOpen && !this.props.isActionBlocked) {
      this.showConfirmation();
    }
  }
```

Replace with:

```ts
  componentDidMount() {
    this._isMounted = true;
  }
```

### Step 1.4 — `source/renderer/app/components/status/MithrilPartialSyncSection.spec.tsx`

(a) In `defaultProps`, delete the line:

```ts
  showConfirmationOnOpen: false,
```

(b) Delete these three tests in full (they test exactly the deleted behavior; quoted titles):

```ts
  it('opens the confirmation modal on mount when deep-linked, without starting', () => {
```

```ts
  it('deep-link open works even when the node is not significantly behind', () => {
```

```ts
  it('does not deep-link open the confirmation when the action is blocked', () => {
```

(c) REWORK (do not delete) the fourth test onto the CTA-click seam the file's other
confirmation tests already use (e.g. `'starts partial sync only after confirmation'`).
Replace the test titled
`'threads the epochs figure into the confirmation modal without any sync-%'` in full with:

```ts
  it('threads the epochs figure into the confirmation modal without any sync-%', () => {
    renderComponent({ behindByEpochs: 3 });

    screen.getByRole('button', { name: 'Mithril Sync' }).click();

    expect(
      screen.getByText(
        'Your node is about 3 epochs behind. Mithril Sync will restore verified chain data to help your node sync faster.'
      )
    ).toBeInTheDocument();
    expect(screen.queryByText(/% synced/)).toBeNull();
  });
```

(The assertion strings are unchanged from the current test — only the opening seam changes.)

### Step 1.5 — `source/renderer/app/components/status/DaedalusDiagnostics.spec.tsx`

In the spec's base props, delete the line:

```ts
  showMithrilPartialSyncConfirmationOnOpen: false,
```

### Step 1.6 — `storybook/stories/nodes/status/Diagnostics.stories.tsx` (seam S3)

The "Partial Sync Confirmation" story currently opens the confirmation THROUGH the prop being
deleted. Rework it to render `MithrilPartialSyncSection` directly inside a mount-time
CTA-click wrapper — this keeps exercising the component's real state seam
(`showConfirmation() → setState({ isShowingConfirmation: true })`) and is deliberately
**copy-proof** (no text/role matching; the recommendation view renders exactly one `<button>`,
the CTA), consistent with CAT-F's story-hardening direction.

(a) Add the component import next to the existing `MithrilPartialSyncConfirmation` import:

```ts
import MithrilPartialSyncSection from '../../../../source/renderer/app/components/status/MithrilPartialSyncSection';
```

(b) Insert above the `storiesOf('Nodes / Diagnostic', module)` chain:

```tsx
// Drives the section's real confirmation seam: mount it, then click the
// single CTA button its recommendation view renders (no copy-text matching),
// which runs showConfirmation() → setState({ isShowingConfirmation: true }).
const AutoOpenedPartialSyncConfirmation = () => {
  const containerRef = React.useRef<HTMLDivElement>(null);
  React.useEffect(() => {
    containerRef.current?.querySelector('button')?.click();
  }, []);
  return (
    <div ref={containerRef}>
      <MithrilPartialSyncSection
        isActionBlocked={false}
        isMithrilPartialSyncWorking={false}
        isSignificantlyBehind
        isProbeFailed={false}
        behindByEpochs={1}
        onRestoreFocus={action('onRestoreFocus')}
        onStartMithrilPartialSync={async () => {
          action('onStartMithrilPartialSync')();
        }}
      />
    </div>
  );
};
```

Notes (no decisions to make):

- `behindByEpochs={1}` preserves the figure the old story showed (its diagnostics fixture tips
  were epoch 512/513 → `computeBehindByEpochs` = 1).
- The `async () => { … }` wrapper satisfies the CAT-D-tightened
  `onStartMithrilPartialSync: () => Promise<void>` prop (a bare `action(…)` handler returns
  `void` and would not compile).
- Accepted visual tradeoff: the story now shows the confirmation without the surrounding
  diagnostics chrome (the confirmation subtree is identical — the section's render returns
  ONLY the confirmation while it is showing; the chrome remains covered by the two CTA
  stories).

(c) Replace the old story block — locate:

```tsx
  // Opens the confirmation through the real state seam: the supported
  // `showMithrilPartialSyncConfirmationOnOpen` prop flows to
  // MithrilPartialSyncSection.showConfirmationOnOpen → componentDidMount →
  // showConfirmation() → setState({ isShowingConfirmation: true }).
  .add('Partial Sync Confirmation', () => (
    <DaedalusDiagnostics
      {...baseProps}
      showMithrilPartialSyncConfirmationOnOpen
    />
  ));
```

Replace with (story NAME unchanged — CAT-F retypes by name):

```tsx
  .add('Partial Sync Confirmation', () => (
    <AutoOpenedPartialSyncConfirmation />
  ));
```

(d) Delete the now-falsified fixture comment above `confirmationBaseProps` (~:76-77) — after
this step the real `isShowingConfirmation` seam is exercised through the section wrapper, not
"through DaedalusDiagnostics below". Locate and delete:

```ts
// Isolated confirmation-modal fixtures (the real `isShowingConfirmation` seam is
// exercised through DaedalusDiagnostics below; here the modal is rendered direct).
```

**Cross-reference (CAT-H, seam S4):** the CAT-H inventory line
`Diagnostics.stories.tsx:76-77, :105-108 [DELETE]` is fully mooted by this step — (d) deletes
`:76-77` and (c) deletes `:105-108`; CAT-H will find both already gone (its expected
"mooted by an earlier CAT" outcome).

### Step 1.7 — PRD D13 residual note update (mandatory)

File: `.agent/plans/mithril-partial-sync/mithril-partial-sync-ux-refinement-prd.md`, inside
the D13 decision record. Locate:

```
   wiring planned under D1 are **not built**. The already-shipped `showConfirmationOnOpen` prop on
   `MithrilPartialSyncSection` (tasks 301/303) is left in place, unused, as a benign residual.
```

Replace with:

```
   wiring planned under D1 are **not built**. The already-shipped `showConfirmationOnOpen` prop on
   `MithrilPartialSyncSection` (tasks 301/303) was left in place, unused, as a benign residual.
   **Update (2026-07-04, task-ux-704 CAT-E):** the residual was removed — the prop and its
   `DaedalusDiagnostics`/`DaedalusDiagnosticsDialog` threading were deleted; deep-linking remains
   unbuilt and this decision otherwise stands.
```

(Task IDs are fine in plan/PRD documents; the ban applies to source comments and test titles.)

---

# E-2 — Replace the forged `ChainStorageValidation` in `returnToStorageLocation`

**Defect (D12, CONFIRMED):** `MithrilBootstrapStore.ts` ~:372-379 hand-builds a validation
with hardcoded `chainSubdirectoryStatus: 'will-create'` for the previous custom path — wrong
whenever that path already contains a chain subdirectory, and the field drives user-facing
picker copy (`ChainStorageLocationPicker.tsx` ~:157-160 / `chainStorageUtils.ts`
`getStorageHelpText`). **No IPC change needed:** the store already wraps the validate channel
(`validateChainStorageDirectory` action ~:317-336, channel import ~:22, request ~:319), and
that wrapper already degrades to an `isValid: false` shape on channel failure.

### Step 2.1 — `source/renderer/app/stores/MithrilBootstrapStore.ts`

(a) Locate (inside `returnToStorageLocation`):

```ts
      const previousCustomPath = this.customChainPath;
      const cleanupValidation =
        await prepareChainStorageLocationChangeChannel.request();
```

Insert immediately after:

```ts
      // Revalidate the previous custom path instead of forging a validation:
      // a hand-built 'will-create' is wrong when that directory already holds
      // a chain subdirectory, and the status drives the picker's helper copy.
      const previousPathValidation =
        cleanupValidation && previousCustomPath != null
          ? await this.validateChainStorageDirectory(previousCustomPath)
          : null;
```

(b) Locate the hand-built assignment:

```ts
          this.chainStorageValidation = {
            isValid: true,
            path: previousCustomPath,
            resolvedPath: previousCustomPath,
            availableSpaceBytes: cleanupValidation.availableSpaceBytes,
            requiredSpaceBytes: cleanupValidation.requiredSpaceBytes,
            chainSubdirectoryStatus: 'will-create',
          };
```

Replace with (real validation when it succeeds; today's provisional shape as the fallback so
a validate failure degrades to exactly the current behavior):

```ts
          this.chainStorageValidation =
            previousPathValidation && previousPathValidation.isValid
              ? previousPathValidation
              : {
                  isValid: true,
                  path: previousCustomPath,
                  resolvedPath: previousCustomPath,
                  availableSpaceBytes: cleanupValidation.availableSpaceBytes,
                  requiredSpaceBytes: cleanupValidation.requiredSpaceBytes,
                  chainSubdirectoryStatus: 'will-create',
                };
```

Nothing else in the method changes (the `await` happens before the `runInAction`, which stays
synchronous; the outer `catch`/`finally` and the in-flight guard are untouched).

### Step 2.2 — `source/renderer/app/stores/MithrilBootstrapStore.spec.ts`

(a) Rework the existing happy-path test to pin the fix. In the test titled
`'returns to the picker with the previous custom path as a draft when cleanup resets an empty
selection'`, after the existing
`mockPrepareChainStorageLocationChangeRequest.mockResolvedValue({ … })` block, add:

```ts
    mockValidateChainStorageDirectoryRequest.mockResolvedValueOnce({
      isValid: true,
      path: '/mnt/custom-parent',
      resolvedPath: '/mnt/custom-parent',
      availableSpaceBytes: 4096,
      requiredSpaceBytes: 1024,
      chainSubdirectoryStatus: 'existing-directory',
    });
```

then replace the final `expect(store.chainStorageValidation).toEqual({ … })` assertion object
so it expects that validated result (same object as the mock above — in particular
`chainSubdirectoryStatus: 'existing-directory'`, no longer `'will-create'`), and add:

```ts
    expect(mockValidateChainStorageDirectoryRequest).toHaveBeenCalledWith({
      path: '/mnt/custom-parent',
    });
```

(b) Add a fallback test immediately after it, mirroring its setup:

```ts
  it('falls back to a provisional draft validation when revalidating the previous path fails', async () => {
    const store = setupStore();
    store.customChainPath = '/mnt/custom-parent';
    store.storageLocationConfirmed = true;
    mockPrepareChainStorageLocationChangeRequest.mockResolvedValue({
      isValid: true,
      path: null,
      resolvedPath: '/tmp/state/chain',
      availableSpaceBytes: 4096,
      requiredSpaceBytes: 1024,
    });
    mockValidateChainStorageDirectoryRequest.mockRejectedValueOnce(
      new Error('validation channel unavailable')
    );

    await store.returnToStorageLocation();

    expect(store.pendingChainPath).toBe('/mnt/custom-parent');
    expect(store.chainStorageValidation).toEqual({
      isValid: true,
      path: '/mnt/custom-parent',
      resolvedPath: '/mnt/custom-parent',
      availableSpaceBytes: 4096,
      requiredSpaceBytes: 1024,
      chainSubdirectoryStatus: 'will-create',
    });
  });
```

(The rejected channel exercises the wrapper's own catch → `isValid: false` → fallback path.)
The neighbouring test `'returns to the picker without resetting the current custom path when
cleanup is skipped'` needs no change (with `cleanupValidation` null, the new validate call is
skipped).

---

# E-3 — Picker/util hygiene

### Step 3.1 — E-3a: drop the unreachable `'existing-directory'` help-text branch

**Unreachability argument (verified):** `getStorageHelpText`'s sole call site is
`ChainStorageLocationPicker.tsx` ~:157; its output renders only under
`storageHelpText && !hasBlockchainData` (~:421, mirrored in `describedByIds` ~:184), and
`hasBlockchainData` (~:158-160) is `displayedValidation.isValid && chainSubdirectoryStatus ===
'existing-directory'` — true exactly when the branch fires (the util also requires
`isValid`). The branch's copy can therefore never render.

(a) `source/renderer/app/components/chain-storage/chainStorageUtils.ts` — in
`getStorageHelpText`, delete the case:

```ts
    case 'existing-directory':
      return intl.formatMessage(messages.subdirectoryWarningExists);
```

(the `default: return null;` now covers it — same rendered outcome). The
`subdirectoryWarningExists` descriptor stays defined (see i18n section).

(b) `source/renderer/app/components/chain-storage/ChainStorageLocationPicker.tsx` — collapse
the twin `<p>` blocks. Locate:

```tsx
        {storageHelpText && !hasBlockchainData && (
          <p
            className={classNames(
              styles.storageSubtext,
              styles.storageHelpText
            )}
            id={STORAGE_LOCATION_HELP_TEXT_ID}
          >
            {storageHelpText}
          </p>
        )}
        {hasBlockchainData && (
          <p
            className={classNames(
              styles.storageSubtext,
              styles.storageHelpText
            )}
            id={STORAGE_LOCATION_DATA_FOUND_ID}
          >
            {intl.formatMessage(messages.dataFoundNotice)}
          </p>
        )}
```

Replace with:

```tsx
        {(storageHelpText || hasBlockchainData) && (
          <p
            className={classNames(
              styles.storageSubtext,
              styles.storageHelpText
            )}
            id={
              hasBlockchainData
                ? STORAGE_LOCATION_DATA_FOUND_ID
                : STORAGE_LOCATION_HELP_TEXT_ID
            }
          >
            {hasBlockchainData
              ? intl.formatMessage(messages.dataFoundNotice)
              : storageHelpText}
          </p>
        )}
```

Equivalence: after (a), `storageHelpText` is `null` whenever `hasBlockchainData` is true (the
only status producing help text is `'will-create'`), so the combined gate and per-branch
id/content reproduce both original blocks exactly. The `describedByIds` computation (~:184-190)
needs NO edit — it already emits the same single id the rendered `<p>` carries in each state.
The existing picker spec `'shows the data-found notice instead of the existing-directory help
text'` pins this behavior and must stay green untouched.

### Step 3.2 — E-3b: delete the dead hardcoded-English `message` field

`source/renderer/app/components/chain-storage/ChainStorageLocationPicker.tsx` — locate
(~:344-351):

```ts
        setSelectionValidation({
          isValid: false,
          path: nextPath ?? null,
          reason: 'unknown',
          message: 'An unexpected error occurred. Please try again.',
        });
```

Delete the `message:` line. Dead data (verified): `getValidationMessage` maps
`reason: 'unknown'` to `messages.validationUnknown` intl copy and never reads `.message`;
`ChainStorageValidation.message` is optional (`message?: string`, types ~:112).

---

# E-4 — De-duplicate the four diagnostics message descriptors

**Evidence:** four ids defined in `DaedalusDiagnostics.tsx`'s `defineMessages` are re-declared
as inline descriptors in `DiagnosticsTimeStatusRow.tsx`:

| id | canonical (DaedalusDiagnostics.tsx) | inline copy (DiagnosticsTimeStatusRow.tsx) |
| --- | --- | --- |
| `daedalus.diagnostics.dialog.localTimeDifference` | ~:209-213 | ~:50-53 |
| `daedalus.diagnostics.dialog.localTimeDifferenceChecking` | ~:324-328 | ~:62-65 |
| `daedalus.diagnostics.dialog.localTimeDifferenceCheckTime` | ~:329-333 | ~:66-69 |
| `daedalus.diagnostics.dialog.serviceUnreachable` | ~:354-358 | ~:82-85 (defaultMessage already drifted — see anchor correction 5) |

**Chosen mechanism:** export the existing `defineMessages` const from `DaedalusDiagnostics.tsx`
and import it in the row. The block does NOT move (zero i18n-artifact churn); the resulting
module cycle (`DaedalusDiagnostics` → `DiagnosticsTimeStatusRow` → `DaedalusDiagnostics`) is
benign — the row reads `messages.*` only at render time (TS emits deferred property access)
and `import/no-cycle` is disabled in this repo.

### Step 4.1 — `source/renderer/app/components/status/DaedalusDiagnostics.tsx`

Locate:

```ts
const messages = defineMessages({
```

Replace with:

```ts
export const messages = defineMessages({
```

### Step 4.2 — `source/renderer/app/components/status/DiagnosticsTimeStatusRow.tsx`

(a) Add the import after the existing `globalMessages` import:

```ts
import { messages } from './DaedalusDiagnostics';
```

(b) Replace the three inline-descriptor call sites:

```ts
          {intl.formatMessage({
            id: 'daedalus.diagnostics.dialog.localTimeDifference',
            defaultMessage: '!!!Local time difference',
          })}
```

→ `{intl.formatMessage(messages.localTimeDifference)}`

```ts
            {isForceCheckingSystemTime
              ? intl.formatMessage({
                  id: 'daedalus.diagnostics.dialog.localTimeDifferenceChecking',
                  defaultMessage: '!!!Checking...',
                })
              : intl.formatMessage({
                  id: 'daedalus.diagnostics.dialog.localTimeDifferenceCheckTime',
                  defaultMessage: '!!!Check time',
                })}
```

→

```ts
            {isForceCheckingSystemTime
              ? intl.formatMessage(messages.localTimeDifferenceChecking)
              : intl.formatMessage(messages.localTimeDifferenceCheckTime)}
```

```ts
                : intl.formatMessage({
                    id: 'daedalus.diagnostics.dialog.serviceUnreachable',
                    defaultMessage: '!!!Service unreachable',
                  })}
```

→ `: intl.formatMessage(messages.serviceUnreachable)}`

No id renames; the ids continue to resolve from the untouched locale bundles, so rendered
copy is unchanged in both locales (including `serviceUnreachable`, whose inline defaultMessage
was the drifted one and was never rendered).

---

# E-5 — Overlay type/export hygiene

### Step 5.1 — E-5a: delete the `as any` on an already-typed prop

`source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.tsx` —
locate (~:267):

```tsx
              error={isFinalizeFailureShown ? null : (error as any)}
```

Replace with:

```tsx
              error={isFinalizeFailureShown ? null : error}
```

Verified sound: the overlay's own prop is `error?: MithrilPartialSyncError | null` (~:26) and
`MithrilErrorView`'s prop already accepts
`MithrilBootstrapError | MithrilPartialSyncError | null` (~:20) — the cast is pure noise.

### Step 5.2 — E-5b: delete the spec-only re-export

(a) Same file — delete the line (~:286):

```ts
export { isMithrilPartialSyncOverlayStatus } from '../../../../../common/types/mithril-partial-sync.types';
```

(Verified: its sole importer is the component's own spec; every production consumer — the
store, the dialog — already imports the guard from the common types module.)

(b) `MithrilPartialSyncOverlay.spec.tsx` — repoint the spec at the common module. Locate:

```ts
import { isMithrilPartialSyncBlockingNodeStart } from '../../../../../common/types/mithril-partial-sync.types';
import MithrilPartialSyncOverlay, {
  isMithrilPartialSyncOverlayStatus,
} from './MithrilPartialSyncOverlay';
```

Replace with:

```ts
import {
  isMithrilPartialSyncBlockingNodeStart,
  isMithrilPartialSyncOverlayStatus,
} from '../../../../../common/types/mithril-partial-sync.types';
import MithrilPartialSyncOverlay from './MithrilPartialSyncOverlay';
```

The `describe('isMithrilPartialSyncOverlayStatus', …)` block (~:451+) stays as-is.

---

## Verification

Environment prep first (do not misread as regressions): under Node v24 regenerate the
`.scss.d.ts` typings via `typed-scss-modules` and apply the gitignored jest
`identity-obj-proxy` sidecar per the repo verify-env note **before** judging `yarn compile` /
`yarn test:jest` output.

```bash
yarn test:jest --testPathPattern "Mithril|Diagnostics|ChainStorage"
yarn lint
yarn compile
```

Expected test-count deltas (any other delta is a regression):

- `MithrilPartialSyncSection.spec.tsx`: **−3 tests** (the deleted deep-link trio); the
  reworked epochs test still passes.
- `MithrilBootstrapStore.spec.ts`: **+1 test** (validate-failure fallback); the reworked
  happy-path test still passes.
- All other touched suites (`DaedalusDiagnostics`, `ChainStorageLocationPicker`,
  `MithrilPartialSyncOverlay`, `MithrilPartialSyncConfirmation`): unchanged counts.

Storybook (S3 seam): confirm the reworked story compiles under `yarn compile`; optionally
smoke `yarn storybook` → "Nodes / Diagnostic / Partial Sync Confirmation" shows the
confirmation view on load.

`yarn prettier:check` on touched files; classify failures against pre-existing HEAD drift
first (`git show HEAD:<f> | prettier --stdin-filepath <f>`); never reformat
`toHaveBeenCalledWith('str', {obj})` call shapes (prettier 2.1.2 oscillation — the E-2 spec
assertions above use single-object args, which are safe).

Commit: ONE commit for all of CAT-E, Conventional Commits **subject only** (no body, no
trailers). Suggested subject:

```
refactor(mithril): task-ux-704 CAT-E renderer cleanups
```

Rollback: revert the CAT-E commit. If CAT-F has already landed on top, revert both in reverse
order (CAT-F retypes the story E-1 reworked).

## Files touched

- `source/renderer/app/containers/status/DaedalusDiagnosticsDialog.tsx` — E-1
- `source/renderer/app/components/status/DaedalusDiagnostics.tsx` — E-1, E-4
- `source/renderer/app/components/status/MithrilPartialSyncSection.tsx` — E-1
- `source/renderer/app/components/status/MithrilPartialSyncSection.spec.tsx` — E-1
- `source/renderer/app/components/status/DaedalusDiagnostics.spec.tsx` — E-1
- `storybook/stories/nodes/status/Diagnostics.stories.tsx` — E-1 (S3)
- `.agent/plans/mithril-partial-sync/mithril-partial-sync-ux-refinement-prd.md` — E-1 (D13 note)
- `source/renderer/app/stores/MithrilBootstrapStore.ts` — E-2
- `source/renderer/app/stores/MithrilBootstrapStore.spec.ts` — E-2
- `source/renderer/app/components/chain-storage/chainStorageUtils.ts` — E-3a
- `source/renderer/app/components/chain-storage/ChainStorageLocationPicker.tsx` — E-3a, E-3b
- `source/renderer/app/components/status/DiagnosticsTimeStatusRow.tsx` — E-4
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.tsx` — E-5
- `source/renderer/app/components/loading/mithril-bootstrap/MithrilPartialSyncOverlay.spec.tsx` — E-5

No i18n locale files. No main-process files. No IPC contracts.

## Out of scope (deferred in the master — do not do)

- E1 finalize-orchestration relocation to the store + E3
  `MithrilPartialSyncOverlayContainer` — deferred post-merge.
- E4 shared start-handler util — deferred (store-owned `startError` variant REJECTED;
  extraction variant only, post-merge).
- E5 variant copy table; E6 cancel-state helper; E9e modal shell extraction — deferred.
- E9i message-id namespace rename — deferred (would churn shipped ja-JP ids).
- Story fixture typing / `as any` removal on `baseProps` / `??` fallbacks in stories — CAT-F
  (F6/F8/F12), after this commit (seam S3).
- Deleting the orphaned `subdirectoryWarningExists` descriptor — permanently out (i18n
  artifacts untouched).

## Acceptance checks

- **E-1:** repo-wide grep for `showMithrilPartialSyncConfirmationOnOpen` and
  `showConfirmationOnOpen` returns zero hits in `source/` and `storybook/` (the only surviving
  mentions are plan docs and the updated PRD note); section suite green at −3 tests with the
  reworked epochs test passing via CTA click; the "Partial Sync Confirmation" story renders
  the confirmation view without the deleted prop; PRD D13 carries the update note.
- **E-2:** happy-path return-to-picker adopts the channel's real validation (pinned with
  `'existing-directory'`); validate-failure falls back to the provisional `'will-create'`
  draft; validate channel called with the previous custom path; no IPC signature changes.
- **E-3:** `getStorageHelpText` has no `'existing-directory'` case; exactly one help-text
  `<p>` block remains with conditional id/content; `'shows the data-found notice instead of
  the existing-directory help text'` still green untouched; the `message:` literal is gone;
  `subdirectoryWarningExists` still defined in `ChainStorage.messages.ts`.
- **E-4:** the four ids are declared exactly once in `source/` (grep each id → one
  `defineMessages` hit in `DaedalusDiagnostics.tsx` plus locale-bundle entries); the row has
  no inline descriptors; diagnostics suite green; rendered EN/JA copy unchanged.
- **E-5:** no `as any` in `MithrilPartialSyncOverlay.tsx`; no re-export at its tail; overlay
  spec imports the guard from the common types module and its guard describe block passes
  unchanged.

## Escalations

- **E1 (story seam):** if `MithrilPartialSyncRecommendation` renders more than one `<button>`
  in its initial view at implementation time (it renders exactly one today — the CTA), the
  wrapper's `querySelector('button')` becomes ambiguous — stop and escalate rather than
  falling back to copy-text matching (that would recreate the pattern CAT-F's F8 removes).
- **E2 (PRD edit):** the PRD is otherwise a locked document; Step 1.7 is the ONLY sanctioned
  edit. If the quoted D13 sentence has drifted, reconcile against the live text but keep the
  update note's content; do not touch any other PRD section.
- No other brief/code mismatch remains: every quoted anchor above was re-verified against the
  working tree on 2026-07-04 (pipeline write/read sites, the inline-confirm evidence at
  `MithrilProactivePromptContainer.tsx`/`SyncingConnectingMithrilPrompt.tsx`, the validate
  wrapper and its catch shape, the picker gates and `describedByIds`, the four descriptor
  pairs and their locale entries, the overlay Props/`MithrilErrorView` union, and the
  spec-only re-export importer).
