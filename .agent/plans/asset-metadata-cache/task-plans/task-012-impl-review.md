Implementation: Iteration 1
Timestamp: 2026-09-15T02:40:00Z

Changes made:
- `source/main/assets/assetMetadataDb.ts`: `ASSET_IMAGE_MAX_ENTRY_BYTES` exported, `writeImage` now enforces it, and `touchImage` and `enforceImageBounds` added.
- `source/main/assets/assetImageStore.ts`: the two bounds and the touch interval as constants, both bounds injectable and defaulting to them, the touch on read, and enforcement after a write.
- `source/main/assets/assetImageStore.realfs.spec.ts`: thirteen cases added across two groups.

Files touched:
- `source/main/assets/assetMetadataDb.ts`
- `source/main/assets/assetImageStore.ts`
- `source/main/assets/assetImageStore.realfs.spec.ts`
- `.agent/plans/asset-metadata-cache/task-plans/task-012.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-012-plan-review.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-012-impl-review.md`
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`

One correction to the approved plan, recorded rather than edited into it:

The Verification Plan asks for a case proving the sweep floor's early return is real, by calling `enforceImageBounds` with a byte bound low enough that the floor derived from it is zero. That case was written and it failed, correctly: with a byte bound of one, the derived floor is zero, the early return does not fire, and the sweep evicts everything, which is the right answer for that bound. The premise was wrong rather than the code. The early return is an optimisation and cannot change an outcome, so there is no outcome that distinguishes it from the full path, and instrumenting the query path to observe it would assert an implementation rather than a property. The case now asserts what the floor is for, that below it the shipped bounds cannot be crossed and nothing is evicted, and the arithmetic of the floor is asserted separately from the exported constants.

One detail decided during implementation:

`ASSET_IMAGE_MAX_ENTRY_BYTES` moved into the database module and the image store re-exports it as `ASSET_IMAGE_MAX_BYTES`. The per-entry cap is now enforced by the writer and is the input to the sweep floor's derivation, both of which live with the table, and keeping the constant beside the enforcement is what makes the invariant local.

Verification run:

- `jest source/main/assets/assetImageStore --coverage=false` — 35 passed, of which 13 are new.
- The entry bound: five entries against a bound of three evicts two, and the three that survive are named, not counted. A table exactly at its bound evicts nothing.
- The byte bound: five entries against a bound admitting three evicts two, and against one admitting four evicts one, so the sweep stops as soon as both bounds hold rather than emptying the table.
- Both bounds crossed at once ends inside both, with the survivors named.
- After an eviction removing four of five images, all five `asset_metadata` rows are still present.
- The floor: below it, the shipped bounds evict nothing. The floor equals the total bound over the per-entry cap, 256, asserted from the exported constants, along with the two shipped values themselves.
- `writeImage` refuses a row one byte over the per-entry cap and accepts one exactly at it, which is the invariant the floor rests on and the reason the check moved into the writer.
- A fetch that stores an image into a table already at its entry bound leaves the table at the bound, using the store's injected bounds, which is what ties the algorithm to the shipped path.
- The ordering: a read of a row whose `fetched_at` is ten hours old advances it to the clock; a read of one sixty seconds old leaves it alone. And the case that distinguishes least recently used from least recently written: of two rows, the older one is read, then a bound admitting one is enforced, and the row that survives is the one that was read rather than the one that was written later.

Checks, all four through Nix:
- `nix build '.#checks.x86_64-linux.compile' --no-link` — exit 0.
- `nix build '.#checks.x86_64-linux.lint' --no-link` — exit 0.
- `nix build '.#checks.x86_64-linux.i18n' --no-link` — exit 0.
- `nix build '.#checks.x86_64-linux.jest' --no-link -L` — 81 suites passed, 1199 tests with 1196 passed and 3 skipped, exit 0. The previous state of this branch was 81 suites and 1186 tests, so thirteen tests were added to an existing suite and nothing else moved.

`nix fmt` was run and changed one file before the checks.

No new `@ts-ignore` and no new `@ts-expect-error`. `git diff HEAD -- package.json yarn.lock` produces nothing.

Deviations from the approved plan:
- The sweep floor case, for the reason under the correction above.

Outcome: Implementation complete and ready for review

Review of Iteration 1
Timestamp: 2026-09-15T02:47:00Z

Acceptance criteria, each against the evidence:

1. *`yarn test:jest` passes.* Met.

2. *Neither bound can be exceeded after a write.* Met on both halves of the question: the algorithm is driven at test-sized bounds across five cases, and the shipped path is driven separately through a fetch that stores an image with the store's own defaults in play.

3-4. *Oldest first, and evict until satisfied and no further.* Met, and asserted by which subjects survive rather than by how many, which is the difference between testing an ordering and testing a count.

5. *Eviction leaves `asset_metadata` untouched.* Met. The foreign key makes it structural, and the case asserts it anyway because the property is the reason the two tables are separate.

6. *Below the floor, nothing is evicted.* Met, in the form the correction arrived at. The original phrasing asked for evidence that cannot exist without instrumenting an implementation detail.

7. *A read updates `fetched_at` past the interval and not inside it.* Met, with the third case carrying the actual argument: read order beats write order.

8. *Checks green, no new suppressions, `package.json` unchanged.* Met.

The correction is the most useful thing in this record. A test written to prove an optimisation is doing something, when the optimisation is defined by not changing anything, is a test that can only fail or assert an implementation. Catching that in the first run rather than by weakening the assertion is the outcome to want.

Moving the per-entry cap into `writeImage` is the other change worth naming. Before it, the sweep floor was correct because a caller in another module happened to check a limit; now it is correct because the table refuses the row. The failure that prevents is the worst kind a cache bound has: one that appears enforced and is not.

Summary: `asset_image` is bounded at 2,000 entries and 64 MiB, evicting least recently used, with the ordering driven by reads rather than by writes and the touch rate-limited so that rendering a row does not rewrite its blob. Phase 2 is complete: the database, the registry client, the three verification steps, the resolver and the image store, all behind no consumer and all bounded.

Decision: approved
