Planner: Iteration 1
Timestamp: 2026-09-14T18:46:00Z

Plan Summary:
- Created `.agent/plans/asset-metadata-cache/task-plans/task-003.md` with the twenty-one sections the plan-workspace readme requires.
- Classified the task `agent_execution`.
- The change is one derived flag passed to the `NumericInput` as `allowOnlyIntegers`, plus the deletion of the `onKeyPress` decimal-separator guard, plus a new colocated `AssetInput.spec.tsx`.
- The task is in the wrong-amount-signed class, so the Verification Plan lists and drives its negative cases rather than asserting the positive path.

Docs, Workflows, Research, and Skills Consulted:
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-prd.md`, the send-path section at `:1294-1345`.
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`, the `task-003` entry plus `task-019`, `task-021`, `task-039` and `task-040`.
- `.agent/plans/asset-metadata-cache/task-plans/readme.md`, the caution on the wrong-amount-signed class.
- `.agent/skills/i18n-messaging/SKILL.md`, read and found not to apply: no message is added.
- `CLAUDE.md` for the `!= null` rule and the spec conventions.

Repo-Verified Findings Used To Shape The Plan:
- `AssetInput.tsx:116` passes `decimalPlaces={decimals}`, and `domains/Asset.ts:18` leaves `decimals` undefined for an unconfigured asset.
- `NumericInput.js:197` and `:199` select `^([0-9]+)?$` under `allowOnlyIntegers`, and `:206` returns the previous value unchanged when the pattern fails, before any branch that reads `inputType`.
- `NumericInput.js:374` and `:380`: under `allowOnlyIntegers` the displayed string comes from `toString()` rather than `toFormat()`, so group separators are dropped. Measured: `toFormat(0)` of 1234567 is `'1,234,567'`, `toString()` is `'1234567'`.
- `config/profileConfig.ts:19-32`: the `number-2` profile groups with `.` and separates decimals with `,`, so a grouped raw-units amount reads as a decimal amount. Dropping grouping removes that ambiguity.
- The keypress guard at `AssetInput.tsx:128-140` fires only when `decimals === 0`, never fires for a paste, and two of its three constants are `keyCode` values that a `keypress` event cannot carry.
- Measured: `new BigNumber('1.23456789').toFixed(undefined, 4)` returns the string unchanged and `undefined > 0` is false, which is why the defect is invisible today.
- `WalletSendForm.spec.tsx:58-74` builds its fixtures with `decimals: 0`, so the existing suite exercises the branch this task changes.
- `tests/_utils/TestDecorator.tsx` plus `BrowserLocalStorageBridge` and `DiscreetModeFeatureProvider` are the provider stack `AssetInput` needs, and `ReactToolboxMobxForm` with the VJF plugin builds a real field for the spec.

Planned Approach:
- `allowOnlyIntegers` whenever decimal places are unknown or zero, which is the whole set of cases where the field is denominated in raw units.
- Delete the keypress guard, which covers a strict subset of the same ground.
- Do not replace the placeholder with the ticker or fingerprint; record why.

Scope Guard / Self-Review:
- No blocking, no refusal to send, no resolution order, no unit label, no snapshotting.
- No change to `WalletSendForm.tsx`, to `formattedAmountToNaturalUnits`, or to the confirmation dialog.

Outcome: Canonical task plan drafted and ready for critique

Critique of Iteration 1
Timestamp: 2026-09-14T18:48:30Z

Three gaps, in descending severity.

1. The plan says the spec drives a typed separator and a pasted one as if they were two mechanisms. In jsdom they are the same `change` event, and react-polymorph separates them only by `inputType` on the native event. The plan must say which one it is actually exercising and why the distinction does not matter here, rather than leaving a reader to believe two paths were covered when one was. It does not matter, because the integers-only pattern is tested at `NumericInput.js:206` before any branch that reads `inputType`, but that is the argument and it is missing.

2. Every negative case asserts that the field value is unchanged. That assertion also passes if the component hands the form the same value a second time, which is a different and worse behaviour than refusing the character. In a wrong-amount-signed change the property worth pinning is that a refused character never reaches the form at all, so at least one case must assert on the `onChange` the component passes down.

3. The flag is written `decimals != null` and the spec drives only `undefined`. `null` is the other spelling of unknown and the `Asset` domain declares both. A spec that drives one of them does not check the comparison that was deliberately chosen over `!decimals`.

Nothing in the plan is wrong. The decision to accept the loss of group separators, argued from the `number-2` profile making a grouped raw amount indistinguishable from a decimal one, is the strongest part of the plan and turns what looked like a cost into a reason. The decision not to replace the placeholder is correctly reasoned, correctly evidenced against the PRD's own wording, and correctly flagged as a judgement call rather than taken quietly.

Outcome: requires_changes

Planner: Iteration 2
Timestamp: 2026-09-14T18:50:00Z

Plan Updates In Response To Critique:
- Added a note to the Verification Plan stating exactly what the spec can and cannot distinguish: one `change` event per cumulative keystroke for typing, one event carrying the whole string for a paste, and the argument for why `inputType` is irrelevant to this change.
- Added a negative case asserting that the component's `onChange` is not called for a refused character, with the reason that an unchanged value is a weaker property than a value the form never saw.
- Added both spellings of unknown, `undefined` and `null`, to the negative cases, so the `!= null` comparison is checked rather than assumed.

Resulting Approved Plan Shape:
- One prop, one deletion, one new spec.
- Negative cases enumerated and driven for the unknown and the zero denomination, with the positive decimal-denominated path asserted on the submitted value rather than on the absence of an error.
- One recorded deviation from the task graph's implementation notes, with its reasons and an open question for the owner.

Scope Guard / Self-Review:
- The revision adds tests and pins claims. It adds no behaviour that was not already planned.
- Scope is unchanged: still one component, still no message, no cache, no IPC, no network, no dependency.

Outcome: Canonical task plan revised after critique and approved for build execution
