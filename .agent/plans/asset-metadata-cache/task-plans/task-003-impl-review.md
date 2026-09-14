Implementation: Iteration 1
Timestamp: 2026-09-14T18:55:00Z

Changes made:
- `source/renderer/app/components/wallet/send-form/AssetInput.tsx`: two derived locals, `areDecimalsKnown` and `isInRawUnits`, and `allowOnlyIntegers={isInRawUnits}` on the `NumericInput`. The `onKeyPress` decimal-separator guard is deleted and the handler is now `handleSubmitOnEnter` directly. No other prop changed.
- `source/renderer/app/components/wallet/send-form/AssetInput.spec.tsx`: new, fourteen cases across three denominations, built on one real `ReactToolboxMobxForm` field and the provider stack `WalletSendForm.spec.tsx` uses.

Files touched:
- `source/renderer/app/components/wallet/send-form/AssetInput.tsx`
- `source/renderer/app/components/wallet/send-form/AssetInput.spec.tsx`
- `.agent/plans/asset-metadata-cache/task-plans/task-003.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-003-plan-review.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-003-impl-review.md`
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`

The negative cases, run against the unchanged component first:

The plan required the spec to be run before the change so that a case passing before and after could not be mistaken for one this task closed. Seven of the fourteen failed against the current component and seven passed:

```
✕ refuses a typed decimal amount                                  (unknown)
✕ refuses a typed decimal amount when the decimal places are null (unknown)
✕ never hands a refused character to the form                     (unknown)
✕ refuses a pasted decimal amount                                 (unknown)
✕ refuses a pasted decimal amount written with a comma            (unknown)
✕ refuses a pasted amount carrying group separators               (unknown)
✓ refuses pasted text that is not a number                        (unknown)
✓ submits the raw units it was given                              (unknown)
✕ renders a large amount without group separators                 (unknown)
✓ refuses a typed decimal amount                                  (zero)
✓ refuses a pasted decimal amount rather than rounding it         (zero)
✓ accepts a typed decimal amount and submits it in natural units  (six)
✓ accepts a pasted decimal amount and submits it in natural units (six)
✓ submits the smallest expressible amount without rounding it away (six)
```

Two findings from that run that the plan did not anticipate:

1. **The defect needs the keystroke after the separator.** Typing `1` then `1.` leaves the form field holding `1`: react-polymorph's branch at `NumericInput.js:315` puts the trailing separator in `fallbackInputValue` and leaves `value` alone. The field only becomes `1.5` on the next keystroke. A first draft of the spec asserted on the field value after `1.` alone and passed against the unchanged component, which would have been a test that proved nothing. Every typed case now drives the full `['1', '1.', '1.5']` sequence.

2. **Both zero-decimal cases pass before and after.** With `decimalPlaces` 0, `bigNumberToFixed` floors `1.5` to `1`, so today's behaviour and the new refusal land on the same submitted amount. They are kept as regression guards and are honestly not closures: the zero case was already safe in the submitted value, and what changes for it is that input is refused rather than silently altered.

Verification run, all four checks through Nix on a dirty tree with the new spec staged, each built locally rather than substituted:
- `nix build '.#checks.x86_64-linux.compile' --no-link` — built `8dgzwlps1kh4ilfxx3pi1m7xhi3agf1j-daedalus-compile.drv`, exit 0.
- `nix build '.#checks.x86_64-linux.lint' --no-link` — built `l6l9ckm4xf3610j9mjxfkz46k7sj6hck-daedalus-lint.drv`, exit 0.
- `nix build '.#checks.x86_64-linux.i18n' --no-link` — built `k9lv149jhmz1srk4nfhnkhizciab6hx6-daedalus-i18n.drv`, exit 0. No message changed, so this is a regression guard rather than a result.
- `nix build '.#checks.x86_64-linux.jest' --no-link -L` — 75 suites passed, 989 tests with 986 passed and 3 skipped, exit 0. The previous state of this branch was 74 suites and 975 tests, so one suite and fourteen tests were added and nothing else moved. `WalletSendForm.spec.tsx`, whose fixtures are zero-decimal assets driven through this input, passes unchanged at 12 tests.

One compile failure was hit and fixed during implementation: `form.$(fieldName)` typed the field key as `never`, because `ReactToolboxMobxForm` is generic over its field map and the spec instantiated it without a type argument. Fixed by declaring `type AssetFormFields = { [assetField: string]: string }`, which is the shape `WalletSendForm.tsx:120-124` already uses for the same reason. Worth recording because host `tsc` was not consulted: the failure came from `nix build` and would have been missed by a spec run alone.

No new `@ts-ignore` and no new `@ts-expect-error`: the only occurrences in the diff are three references to the rule inside the plan document.

Deviations from the approved plan:
- None in scope or approach. The single recorded deviation from the task graph's implementation notes, not replacing the placeholder with the ticker or fingerprint, was taken as planned and is repeated in the Final Outcome and in the handoff.

Outcome: Implementation complete and ready for review

Review of Iteration 1
Timestamp: 2026-09-14T18:58:00Z

Acceptance criteria, each against the evidence:

1. *`yarn test:jest` passes.* Met, through `nix build '.#checks.x86_64-linux.jest'`, which built locally and reported 75 suites and 989 tests.

2. *The amount entered and the amount in the built transaction agree for every combination of decimals set, unset and zero.* Met, and asserted as an equality on the submitted value rather than as the absence of an error. Unknown: entering `1500000` submits `'1500000'`, and no sequence of keystrokes or pastes in the spec can put a separator into the field. Zero: entering `1` submits `'1'` and `1.5` cannot be entered. Six: `1.5` submits `'1500000'` and `0.000001` submits `'1'`, which is the boundary a wrong `decimalPlaces` would round away. Both spellings of unknown, `undefined` and `null`, are driven, so the `!= null` comparison is checked rather than assumed.

3. *The negative cases are driven, not implied.* Met. Nine negative cases across the unknown and zero denominations, each with its own assertion, plus the before-and-after run that says which of them the change actually closes. One of them asserts on the component's `onChange` rather than on the resulting value, so the property pinned is that a refused character never reaches the form at all.

4. *`compile`, `lint` and `i18n` green from `nix build`.* Met. All three built locally rather than being substituted, and the log records the derivation path for each.

5. *No new `@ts-ignore` and no new `@ts-expect-error`.* Met.

Two behaviour changes beyond the strict wording of the task graph, both asserted by a test rather than left to be discovered:

- **The field loses its group separators wherever it is integers-only.** `NumericInput.js:374` and `:380` pick `toString()` over `toFormat()` under `allowOnlyIntegers`. This is pinned by "renders a large amount without group separators". It is the right trade here: `config/profileConfig.ts:19-32` offers a number profile whose group separator is `.` and whose decimal separator is `,`, under which a grouped raw-units amount reads `1.500.000` and is indistinguishable from a decimal amount, in the one field where that confusion is expensive.
- **A pasted amount carrying separators is refused rather than parsed.** Today pasting `1,234` under the default profile yields 1234, and pasting `1.5` into a zero-decimal field yields 1. Both are now refused and the field keeps its previous value. Pinned by two cases. The cost is that an amount copied from outside the application with grouping in it has to be retyped; since the field now renders ungrouped, an amount copied from inside it pastes back cleanly.

The recorded deviation, restated so it is not lost: the placeholder is still `'0'` in the integers-only case and was not replaced with the ticker or the fingerprint. The field already carries both, through the `Asset` pill passed as `label` at `AssetInput.tsx:113` and the ticker span at `:153-158`, and `'0'` is an accurate hint for a field that now accepts only integers. The durable translated unit label belongs to `task-039`, which depends on this task. This is the one judgement call in the change and it is flagged for the owner rather than settled here.

Summary: One prop and one deletion, with the evidence that seven of the fourteen cases fail without them. The submitted amount is now what the field displays for every denomination, which is the property the wrong-amount-signed class exists to protect.

Decision: approved
