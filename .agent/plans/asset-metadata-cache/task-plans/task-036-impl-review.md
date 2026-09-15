Implementation: Iteration 1
Timestamp: 2026-09-16T19:40:00Z

Changes made:
- `source/main/assets/assetVerification.ts`: `nativeScriptLatestSlot`, a second
  fold over the type `decodeNativeScript` already produces.
- `source/main/assets/assetVerification.spec.ts`: ten cases.
- `source/main/assets/chainPointerVerification.ts`: the witness-set lookup and
  `policyClosed` on a confirmation.
- `source/main/assets/chainPointerVerification.realfs.spec.ts`: six cases, and
  the `@jest-environment node` docblock the module needs.
- `source/main/assets/assetMetadataResolver.ts`: the stored verdict, and `_due`
  reading it.
- `source/main/assets/assetMetadataResolver.realfs.spec.ts`: ten cases.
- `source/main/ipc/assetMetadataChannel.ts`: the record unwrapped on the way to
  the renderer.
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`.

Files touched:
- the seven source files above
- the three review-log files for this task
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`

Two details decided during implementation:

**The synthetic witness set is assembled from bytes.** `cbor.encode` given a
`Buffer` produces a byte string, so a script embedded that way would be wrapped
and would not hash to its own policy id. The first version of the fixture did
exactly that and the closure case failed while every other case passed, which is
the right way round: the check looks for a script whose bytes hash to the policy,
and a wrapped script is not one.

**The confirmation spec needed `@jest-environment node`.**
`decodeNativeScript` guards a key hash with `instanceof Uint8Array`, and a
`Buffer` from `cbor` under jsdom is not an instance of the test realm's
`Uint8Array`, so the decode returned null and every policy read as open. Every
other main-process spec in this directory already carries the docblock. This is
a test-environment artefact and not a runtime one, because the main process is
Node, but it silently turned a rule off rather than failing loudly.

Verification run:

- `yarn jest source/main/assets/assetVerification --coverage=false` — 73 passed,
  10 added.
- `yarn jest source/main/assets/chainPointerVerification --coverage=false` — 24
  passed, 6 added.
- `yarn jest source/main/assets/assetMetadataResolver --coverage=false` — 55
  passed, 10 added.
- The freshness cases write a row a fortnight in the past, so the seven-day
  window has already elapsed and the freezing rule is the only thing that can
  hold a re-read back. They assert the pointer transport's call count rather than
  the row, because what is under test is whether a request went out.
- The Plutus case runs against the recorded preprod transaction, whose witness
  set carries a Plutus V3 script and no native one. That is the undecidable case
  and it was not arranged: the fixture was recorded for `task-034` and happens to
  be one.
- The fold's three uncertain cases are driven directly, because each of them
  decides whether anything is ever frozen and none is exercised by the
  end-to-end path.

Checks, all six through Nix with every change staged:
- `nix build '.#checks.x86_64-linux.compile' --no-link` — exit 0.
- `nix build '.#checks.x86_64-linux.lint' --no-link` — exit 0.
- `nix build '.#checks.x86_64-linux.stylelint' --no-link` — exit 0.
- `nix build '.#checks.x86_64-linux.i18n' --no-link` — exit 0.
- `nix build '.#checks.x86_64-linux.jest' --no-link -L` — 96 suites passed,
  1581 tests with 1578 passed and 3 skipped, exit 0. The branch stood at 96
  suites and 1556 tests, so this adds twenty-five tests and no suite.
- `nix build '.#checks.x86_64-linux.cucumber-unit' --no-link -L` — 36 scenarios
  and 128 steps passed, unchanged.

One earlier lint run failed on an import-order finding introduced here, fixed
before the run above.

`nix fmt` was run and changed files before each check run.

No new `@ts-ignore` and no new `@ts-expect-error`.
`git diff HEAD -- package.json yarn.lock` produces nothing.

Deviations from the approved plan:
- None.

Outcome: Implementation complete and ready for review

Review of Iteration 1
Timestamp: 2026-09-16T19:50:00Z

Acceptance criteria, each against the evidence:

1-4. *The four tiers.* Met, each driven at the resolver on a row past its window.

5. *A forced read reaches a frozen row.* Met.

6. *A Plutus policy is reported open, against the recorded transaction.* Met.

7. *No timer.* Met; nothing here schedules anything and the existing case that
   asserts so still passes.

8-9. *All six checks, suppressions, dependencies.* Met.

Three judgements worth naming.

**The witness set is a better source than the PRD's and the substitution is the
whole task.** The registry's `policy` field is not available for a chain row, so
the rule as written applied to nothing. The script that authorised the mint is in
the transaction, and that transaction has been confirmed against the user's own
chain, so this is the script the chain accepted rather than one a server
published.

**Every uncertainty resolves towards open, and that is not symmetry.** A row
wrongly left open costs one request per week. A row wrongly frozen is never read
again, and the only ways back are a manual refresh on that one asset or deleting
the cache directory. The `n of k` case is where this is visible: the exact answer
is the n-th latest expiry and the code takes the latest, so a policy is only ever
called closed later than it truly is.

**The volume claim behind fetch-once is weaker for this channel than the PRD's
number.** 89.6 percent is a fact about registry entries. Chain rows are NFTs, and
an open collection minting over months is a shape the registry corpus barely
contains. Nothing here measures it and the plan says so rather than borrowing the
figure.

Decision: approved
