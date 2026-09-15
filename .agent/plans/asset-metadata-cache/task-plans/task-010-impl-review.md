Implementation: Iteration 1
Timestamp: 2026-09-15T00:55:00Z

Changes made:
- `source/main/assets/assetMetadataResolver.ts`: new. The read that answers from disk, the due rule, the claim that stops a subject being scheduled twice, the fetch outside any database call, per-property verification, row construction, the refresh comparison, and the callback that emits what changed.
- `source/main/assets/assetMetadataResolver.realfs.spec.ts`: new. Twenty-seven cases across six groups, against a real database file and a stubbed transport.

Files touched:
- `source/main/assets/assetMetadataResolver.ts`
- `source/main/assets/assetMetadataResolver.realfs.spec.ts`
- `.agent/plans/asset-metadata-cache/task-plans/task-010.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-010-plan-review.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-010-impl-review.md`
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`

Two details decided during implementation:

Subjects are claimed synchronously in `request`, before the work is queued, rather than at the moment the resolve begins. The first version marked them in flight inside `resolve`, which left a window: two `request` calls in the same tick both computed the same subject as due, because the first had not started running yet, and the second queued a duplicate fetch behind the first. Claiming at schedule time closes it, and the claim is released in a `finally` so a throwing fetch does not leave a subject permanently unaskable.

`retryBackoffMs` is threaded from the resolver into the registry client. The failure-path case otherwise spends a real second asleep in the client's retry, and a suite that pays a second per failure case teaches people to write fewer of them.

Verification run:

- `jest source/main/assets/assetMetadataResolver --coverage=false` — 27 passed in 0.7 seconds.
- Reading: a cached subject is answered with the transport asserted to have received no further call; an uncached subject returns nothing and the scheduled fetch fills it; an empty list makes no call; and with the transport failing, a previously cached subject still answers from disk with no call made at all.
- The due rule at its boundaries: a row one millisecond younger than the refresh window is not fetched and one millisecond older is, driven by writing the row with an explicit timestamp. A subject whose `retry_after` is in the future is not fetched and one whose window has passed is. Two overlapping requests for the same subject produce one query.
- The `verified` column is the decimals verdict, asserted four ways: a verified decimals gives `verified: true`; a tampered decimals signature keeps the value and gives `verified: false`, because an unverified value is still the recommended one; a tampered ticker signature with decimals intact still gives `verified: true`, which is what proves the column is not an average over properties; and an entry with no decimals property gives `verified: false` with decimals null.
- An entry with no policy is written unverified with its ticker intact and is not logged as an error.
- A decimals value of 21 is stored as null with the ticker intact, rather than the engine refusing the whole row.
- Emission: the callback fires once with the rows written; it does not fire when nothing resolved; a consumer that throws does not propagate; a failed batch writes no metadata, writes a `failed` resolution, and takes two transport calls, which is the client's one retry.
- A subject the registry omits is recorded `unregistered` with `attempted_at` equal to the `now` the resolver handed the client, which is the shared-clock property `task-007` set up.
- Refreshing, five cases: a risen sequence number rewrites and emits; an unchanged one keeps the stored values, restamps `updated_at` and does not emit, with both halves asserted; a lower one does not downgrade; a stored null sequence number is rewritten because null means nothing is known rather than zero; and a registry response replaces a `chain` row regardless of sequence numbers, with `slot` going back to null.
- Timers: after a resolve completes, the clock is advanced by a day under fake timers and the transport receives no further call.

Checks, all four through Nix with both new files staged:
- `nix build '.#checks.x86_64-linux.compile' --no-link` — exit 0.
- `nix build '.#checks.x86_64-linux.lint' --no-link` — exit 0.
- `nix build '.#checks.x86_64-linux.i18n' --no-link` — exit 0.
- `nix build '.#checks.x86_64-linux.jest' --no-link -L` — 80 suites passed, 1164 tests with 1161 passed and 3 skipped, exit 0. The previous state of this branch was 79 suites and 1137 tests, so one suite and twenty-seven tests were added and nothing else moved.

`nix fmt` was run and changed the two new files before the checks.

No new `@ts-ignore` and no new `@ts-expect-error`. `git diff HEAD -- package.json yarn.lock` produces nothing.

Deviations from the approved plan:
- None in scope or approach.

Outcome: Implementation complete and ready for review

Review of Iteration 1
Timestamp: 2026-09-15T01:03:00Z

Acceptance criteria, each against the evidence:

1. *`yarn test:jest` passes against a stubbed transport and a temporary database.* Met. Each case gets its own `mkdtemp` directory and its own database, so no case can see another's rows.

2. *No `setInterval` or `setTimeout` schedules unsolicited work.* Met, and asserted behaviourally rather than by grepping the module: a day of simulated time after a completed resolve produces no further call. That also covers the modules this one imports, which a source-level check would not.

3-8. *The cached read, the uncached read, the retry window, the unverified write, the offline read, and the refresh window.* All met, with the two window criteria driven one millisecond either side of their boundary rather than in the middle.

9. *`verified` is the verdict for the `decimals` property.* Met, and the case that carries the argument is the third: a failed ticker signature leaves the row verified. Without it the column could have been an average over properties and every other assertion would still have passed.

10. *Checks green, no new suppressions, `package.json` unchanged.* Met.

The claim-at-schedule-time fix is worth naming. The behaviour it prevents is exactly the one this whole design exists to remove: the same subject being asked for repeatedly because nothing deduplicates by subject. Finding it in the resolver, where it would have been two fetches rather than 7,200 a day, is the cheap version of the same bug.

One thing to carry into phase 3. `request` returns the cached rows and starts the fill; `pending()` is how a caller waits for that fill. `task-014`'s handler should push resolved rows through the broadcast channel from the `onResolved` callback rather than awaiting `pending()`, because awaiting it would put the network back in front of a render, which is the property `readCached` exists to preserve.

Summary: The four phase-2 modules are now one cache. A read answers from disk and never waits on the network; a miss schedules exactly one fetch however many callers asked; `verified` is computed from the bytes for the property it actually governs; and the refresh path writes a timestamp without pushing a row nobody needs. Nothing in the module knows what a channel is.

Decision: approved
