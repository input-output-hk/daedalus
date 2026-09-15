Implementation: Iteration 1
Timestamp: 2026-09-16T16:40:00Z

Changes made:
- `source/main/assets/cborSpan.ts`: new. A structural reader that returns byte
  ranges. Bounds-checked everywhere, nesting capped at 128.
- `source/main/assets/cborSpan.spec.ts`: new. Thirty cases.
- `source/main/assets/immutableBlockReader.ts`: new. The on-disk layout, the
  derived chunk size, the tip, and four outcomes.
- `source/main/assets/immutableBlockReader.realfs.spec.ts`: new. Fifteen cases.
- `source/main/assets/chainPointerVerification.ts`: new. The four checks and the
  CIP-25 extraction.
- `source/main/assets/chainPointerVerification.realfs.spec.ts`: new. Eighteen
  cases.
- `source/main/assets/chainPointer.fixture.ts`: new. One real preprod block.
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`.

Files touched:
- the seven source files above
- the three review-log files for this task
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`

Three details decided during implementation:

**The fixture is the block, and the transaction is reassembled from it.** Koios's
`tx_cbor` for this transaction was measured to be byte-identical to the
transaction reassembled from the local block, so recording both would have stored
one fact twice and doubled a 10 KiB fixture. The spec reassembles it and the
fixture's header records why that is the same bytes.

**`itemEnd` double-counted the float widths in the first version.** `readHead`
already consumes the two, four or eight argument bytes of a half, single or
double float, and the simple-value branch added them again. The case that found
it drives all six simple forms; the branch is now one line.

**The synthetic blocks in the confirmation spec are assembled from bytes rather
than through `cbor.encode`.** The transaction bodies have to appear in the block
as the exact bytes that were hashed, and an encoder given a decoded body is free
to produce different ones. That is the same property the whole module exists for,
so a spec that re-encoded would be testing the wrong thing.

Verification run:

- `yarn jest source/main/assets/cborSpan.spec.ts --coverage=false` — 30 passed.
- `yarn jest source/main/assets/immutableBlockReader.realfs.spec.ts` — 15 passed.
- `yarn jest source/main/assets/chainPointerVerification.realfs.spec.ts` — 18
  passed.
- **The whole path was run once against the real chain**, outside the committed
  suite, on 2026-09-16: a reader pointed at
  `~/.local/share/Daedalus/preprod/chain/immutable`, 17 GiB across 6,092 chunks,
  derived a chunk size of 21,600 and a tip slot of 131,566,495, and confirmed the
  live `tx_cbor` response for transaction `907243e6…6dea1c`, returning the CIP-25
  name `Northwind Demo`. That run is evidence for this record and is not a
  committed test, because CI has no chain and cannot have one.
- The positive case in the suite is the same block, read back out of a miniature
  database written in the layout the reader documents. Its slot and header hash
  were confirmed against Koios, so the layout claim is checkable rather than
  self-consistent.
- Each tampering case changes one byte, chosen so only the check under test
  fails. The auxiliary case changes the transaction's last byte, which is inside
  the auxiliary data, so the body still hashes to its claimed id and the case
  reaches the key 7 check rather than stopping at the first.
- The burn case is driven from the recorded transaction rather than a synthetic
  one, because the recorded transaction really does mint one asset and burn a
  sibling in the same field. That is the case a key-only check would have got
  wrong.
- The volatile window is driven at the boundary: confirmed at the tip, pending
  one slot past it, in the same case so the pair cannot drift.
- Six fail-closed paths on the reader and two on the confirmation, each asserting
  `unreadable` or `unavailable` rather than a rejection, because deciding nothing
  is not the same as deciding the pointer is wrong.

Checks, all six through Nix with every change staged:
- `nix build '.#checks.x86_64-linux.compile' --no-link` — exit 0.
- `nix build '.#checks.x86_64-linux.lint' --no-link` — exit 0.
- `nix build '.#checks.x86_64-linux.stylelint' --no-link` — exit 0.
- `nix build '.#checks.x86_64-linux.i18n' --no-link` — exit 0.
- `nix build '.#checks.x86_64-linux.jest' --no-link -L` — 96 suites passed,
  1537 tests with 1534 passed and 3 skipped, exit 0. The branch stood at 93
  suites and 1474 tests, so this adds three suites and sixty-three tests.
- `nix build '.#checks.x86_64-linux.cucumber-unit' --no-link -L` — 36 scenarios
  and 128 steps passed, unchanged.

`nix fmt` was run and changed four files before the checks.

No new `@ts-ignore` and no new `@ts-expect-error`.
`git diff HEAD -- package.json yarn.lock` produces nothing, which is criterion
10 and the whole point of the decoder settlement.

Deviations from the approved plan:
- None.

Review of Iteration 1
Timestamp: 2026-09-16T16:50:00Z

Acceptance criteria, each against the evidence:

1. *A recorded pointer and its block confirm, with the CIP-25 name.* Met, against
   a real block, and separately against the real 17 GiB database.

2. *The volatile window resolves to pending at the boundary.* Met.

3-6. *Tampered body, tampered auxiliary data, a mint naming another policy, a
   transaction not in its block.* Met, each with a rejection reason of its own so
   a case cannot pass by failing for the wrong reason.

7. *All three auxiliary-data shapes.* Met.

8. *An unrecognised shape fails closed and the version is recorded.* Met.
   `IMMUTABLE_PRIMARY_INDEX_VERSION` is exported, the module documents what was
   read and when, and a file carrying any other version is refused.

9. *No confirmation on any failing path.* Met by construction: `confirmed` is
   returned from exactly one place, at the end, after all four checks.

10-12. *No dependency, all six checks, no suppressions.* Met.

Four judgements worth naming.

**Writing a CBOR reader is the largest judgement in this branch.** The plan
argues it and the argument holds: the requirement is real, no available library
meets it, the module is a measurer rather than a decoder, and its failure mode is
one-directional. What deserves saying beside that is what it costs. This is
hand-written parsing of attacker-influenced bytes running in the main process,
and the mitigations are bounds checks on every read, a depth cap, and thirty
cases including truncation, reserved encodings and a length that could not fit
the input. If a decoder that exposes byte ranges is ever added to the dependency
set for another reason, this module should be reconsidered rather than kept out
of habit.

**Measuring the volatile window from the database's own tip is better than
measuring it from k**, and it is worth saying why in a review rather than only in
a plan. k and a slot length give an estimate of where the boundary is; the tip is
the boundary. The estimate would have been wrong on any network whose parameters
differ from the ones assumed, and wrong in the unsafe direction if it
overestimated.

**The reader refuses rather than misreads.** A node release that changes the
primary index version turns the chain channel off until this is updated, and
nothing here detects that in advance. That is the intended direction, and the
cost is that the channel can go quiet after an upgrade with only a log line to
say so.

**The block read is synchronous file I O on the main process's thread.** A chunk
file is on the order of a megabyte, read once per confirmation, behind the
resolver's queue. Nothing waits on it and no render depends on it, but it is not
free and it is not asynchronous.

What this task does not establish: that anything is written. `task-035` takes a
confirmation and turns it into a row, and until then this module has one caller,
which is its own spec.

Decision: approved
