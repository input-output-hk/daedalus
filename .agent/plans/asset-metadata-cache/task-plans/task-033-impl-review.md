Implementation: Iteration 1
Timestamp: 2026-09-16T14:45:00Z

Changes made:
- `source/main/assets/httpTransport.ts`: new. The registry client's `post` and
  `readResponse`, moved unchanged except that the response cap is a parameter.
- `source/main/assets/assetRegistryClient.ts`: the implementation removed, the
  two type names and the transport instance re-exported, and the registry cap
  now passed explicitly at the call site rather than arriving as a default.
- `source/main/assets/koiosClient.ts`: new.
- `source/main/assets/koiosClient.spec.ts`: new. Twenty-seven cases.
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`.

Files touched:
- the four source files above
- the three review-log files for this task
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`

Two details decided during implementation:

**The registry client now passes its own cap.** After the extraction the cap
would have arrived as the transport's default, which happens to be the same
number. Passing it explicitly keeps `ASSET_REGISTRY_MAX_RESPONSE_BYTES`
load-bearing: it is asserted by that spec's two cap cases, and a default it
silently agreed with would leave those cases testing the transport's constant
rather than the registry's.

**The batch body is a function rather than a loop body.** `no-continue` is
enforced by eslint, and the loop has three early exits with different meanings.
The first implementation used `continue` and failed lint; the function returns
the outcome and the loop decides whether to stop.

Verification run:

- `yarn jest source/main/assets/koiosClient.spec.ts --coverage=false` — 27
  passed.
- `yarn jest source/main/assets/assetRegistryClient.spec.ts --coverage=false` —
  47 passed, unchanged, which is what says the extraction is transparent. Those
  47 include the loopback-server group that exercises the real socket, so the
  moved code is still driven end to end.
- The request-count criterion is driven twice: one subject, and forty subjects
  sharing a policy and a minting transaction. The second is the case that fails
  if batching were per asset, and it also asserts the second call asks for one
  transaction hash rather than forty.
- The body assertion compares against an exact object rather than checking for
  the absence of forbidden keys. A field added later fails the case, which is
  the property worth having for a request that leaves the machine.
- The `429` case asserts a call count of one; the `503` case immediately after
  it asserts two. Either alone would be satisfied by an implementation that
  never retries or always does.
- The ceiling is driven twice: a limit of zero, which issues nothing, and a
  limit of one across two batches, which issues one request and stops rather
  than working through the second batch to be refused again.
- Five negative parsing cases assert that no pointer survives: an entry for a
  subject nobody asked about, a transaction nobody asked about, a body that is
  not an array, a batch whose second call failed, and a pointer whose
  transaction did not come back.

Checks, all six through Nix with every change staged:
- `nix build '.#checks.x86_64-linux.compile' --no-link` — exit 0.
- `nix build '.#checks.x86_64-linux.lint' --no-link` — exit 0.
- `nix build '.#checks.x86_64-linux.stylelint' --no-link` — exit 0.
- `nix build '.#checks.x86_64-linux.i18n' --no-link` — exit 0.
- `nix build '.#checks.x86_64-linux.jest' --no-link -L` — 93 suites passed,
  1474 tests with 1471 passed and 3 skipped, exit 0. The branch stood at 92
  suites and 1447 tests, so this adds one suite and twenty-seven tests and moves
  nothing else, including the 47 registry cases.
- `nix build '.#checks.x86_64-linux.cucumber-unit' --no-link -L` — 36 scenarios
  and 128 steps passed, unchanged.

An earlier lint run failed on three `no-continue` findings, all introduced here
and all removed by the restructuring above.

`nix fmt` was run and changed two files before the checks.

No new `@ts-ignore` and no new `@ts-expect-error`.
`git diff HEAD -- package.json yarn.lock` produces nothing.

Deviations from the approved plan:
- None.

Outcome: Implementation complete and ready for review

Review of Iteration 1
Timestamp: 2026-09-16T14:55:00Z

Acceptance criteria, each against the evidence:

1. *Two requests per batch, not two per asset.* Met, driven at one subject and
   at forty.

2. *A select list, and no `logo` or `minting_tx_metadata`.* Met, asserted on the
   captured URL.

3. *Subjects and nothing else in the body.* Met, against an exact object.

4. *A 429 backs off and is never retried immediately.* Met, per status code, and
   distinguished from the 503 case.

5. *The ceiling leaves rows absent and surfaces nothing.* Met. Nothing in this
   module throws, and its only outputs are pointers, transactions and resolution
   rows.

6. *A timeout records a retry and does not spin.* Met.

7. *URL composition and the direct option.* Met, four cases.

8-9. *No dependency, all six checks, no suppressions.* Met.

Three judgements worth naming.

**The omitted field is the most consequential line in this task.** Asking for
`minting_tx_metadata` would cost nothing at the wire and would replace a
confirmed value with an unconfirmed one. Nothing downstream would fail, no test
would go red, and the channel's whole argument would be gone. It is left out and
the reason is in the module, not only in the plan.

**The ceiling is a guard, not a quota.** A fifth of the published limit, cleared
by a restart. It cannot enforce the daily cap and does not claim to; what it
stops is this code issuing requests in a loop, which is the failure mode a
desktop client can actually produce.

**The terms of service remain what the research note found them to be.** They
prohibit automated access and systematic retrieval on their face, and whether the
API host is inside the document's definition of the Site is ambiguous. That is a
settled decision of the PRD rather than of this task. This task's contribution to
it is a ceiling well under the published limit, trimmed responses, and one
resolution per asset, and it is worth recording that none of those makes the
terms say something different.

What this task does not establish: that anything Koios returns is true. Nothing
here checks a hash, reads a block, or writes a row. `task-034` is where the index
stops being trusted, and until that lands this module's output has no consumer.

Decision: approved
