Implementation: Iteration 1
Timestamp: 2026-09-15T12:15:00Z

Changes made:
- `source/renderer/app/components/assets/AssetSettingsDialog.tsx`: one message,
  the condition, and a footer element under the decimal places select.
- `source/renderer/app/components/assets/AssetSettingsDialog.scss`: the footer
  container and the sentence's styles.
- `source/renderer/app/components/assets/AssetSettingsDialog.spec.tsx`: five
  cases.
- the four translation artifacts, regenerated.

Files touched:
- the three files above
- `.agent/plans/asset-metadata-cache/task-plans/task-022.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-022-plan-review.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-022-impl-review.md`
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`

One detail decided during implementation:

The sentence ends by saying that choosing the figure in this dialog applies it.
Without that, a user reading "Daedalus does not apply it on its own" is left
without the action, and the action is the control immediately above the
sentence. It is one clause and it is the reason the sentence is in this dialog
rather than anywhere else.

Verification run:

- `jest source/renderer/app/components/assets/AssetSettingsDialog --coverage=false`
  — 11 passed, of which 5 are new.
- The three cases from the task graph are driven on the rendered sentence:
  present for an unverified published value, absent for a verified one, absent
  when the issuer published nothing.
- The fourth case is the one that separates this condition from `task-020`'s
  verdict: an unverified published **zero**. The verdict is deliberately
  suppressed for that combination, so a sentence written in terms of the verdict
  would be missing exactly where a user most needs it, on a token whose amounts
  are shown as raw integers for a reason they cannot see. It passes because the
  condition is written independently.
- The fifth asserts the sentence holds for a user who has already chosen a value,
  because it is about what the issuer published and not about what was chosen.
- Criterion 4, no new warning surface in the token list, checked against the
  staged diff: nine files, none of them under `components/wallet/tokens`.

Checks, all five through Nix with every change staged:
- `nix build '.#checks.x86_64-linux.compile' --no-link` — built
  `gy9b6msnds65x3p1hb2bbr53f0a0j281-daedalus-compile.drv`, exit 0.
- `nix build '.#checks.x86_64-linux.lint' --no-link` — built
  `xx5va9ii850msqdxxvgb9avlgbvchhfh-daedalus-lint.drv`, exit 0.
- `nix build '.#checks.x86_64-linux.i18n' --no-link` — built
  `mlxa96m9nwrjxdn6xbkp4rwhfzr80fqa-daedalus-i18n.drv`, exit 0.
- `nix build '.#checks.x86_64-linux.stylelint' --no-link` — built
  `ppl8gkvswfawyhmpbyifm7khdkdzbmyz-daedalus-stylelint.drv`, exit 0.
- `nix build '.#checks.x86_64-linux.jest' --no-link -L` — 86 suites passed, 1309
  tests with 1306 passed and 3 skipped, exit 0. The previous state of this branch
  was 86 suites and 1304 tests, so five tests were added to an existing suite and
  nothing else moved.

`nix fmt` reported no change.

No new `@ts-ignore` and no new `@ts-expect-error`.
`git diff HEAD -- package.json yarn.lock` produces nothing.

Deviations from the approved plan:
- None.

Outcome: Implementation complete and ready for review

Review of Iteration 1
Timestamp: 2026-09-15T12:20:00Z

Acceptance criteria, each against the evidence:

1-3. *Shown for unverified, not for verified, not for absent.* All met, asserted
   on the sentence a user reads.

4. *No new warning surface in the token list.* Met, by the diff rather than by
   intent. The restraint here is the substance of the task: a badge for
   unverified metadata would land on 42 percent of registry-known rows, and a
   mark that common stops being read.

5-6. *Compile, lint, i18n, stylelint, jest, suppressions, dependencies.* All met.

The judgement to weigh is the separate condition. Reusing `task-020`'s verdict
would have been one line shorter and would have been silent on an unverified
published zero, which is a token whose amounts show as raw integers with nothing
on screen to say why. The zero case is in the suite so the shortcut cannot be
taken later by mistake.

The footer container holds one element today and exists because `task-029` puts
its control in it next, which is the ordering the task graph asked for.

Decision: approved
