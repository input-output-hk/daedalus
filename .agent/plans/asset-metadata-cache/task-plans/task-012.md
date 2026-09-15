# Task task-012: Image eviction under two bounds

## Task ID and Title

- ID: `task-012`
- Title: `Image eviction under two bounds`

## Why Chosen Now

Both dependencies are complete: `task-006` at `084854a71` and `task-011` at `03fe8f5ae`. `asset_image`
now has a writer and no bound, which is the one state in this phase that gets worse the longer it
stands.

It is the last task of phase 2. After it the main-process cache is complete and bounded, and phase 3
can put an IPC channel in front of it.

## Interaction Mode

- Mode: `agent_execution`

A temporary database and a stubbed transport cover every criterion.

## Scope

- `source/main/assets/assetMetadataDb.ts` gains `touchImage` and `enforceImageBounds`.
- `source/main/assets/assetImageStore.ts` gains the two bounds as constants, calls the enforcement
  after a write, and updates `fetched_at` on a read so the ordering means least recently used.
- `source/main/assets/assetImageStore.realfs.spec.ts` gains the cases.

## Non-Goals

- No bound on `asset_metadata`. A metadata row is small, is read on every render, and can carry
  verified decimal places. Evicting one because a picture was large would lose the only part of this
  cache that changes behaviour.
- No new index and no schema change. The PRD gives the DDL and `asset_image_fetched_at` is the index
  it specifies.
- No timer and no background sweep. Enforcement runs after a write, which is the only moment a bound
  can be crossed.
- No eviction of images belonging to assets the user holds in preference to those they do not. The
  store does not know what is held, and inventing that knowledge here would put a wallet concept
  inside a subject-keyed cache.

## Dependencies

- `task-006` and `task-011`, both complete.

## Research Consulted

- `.agent/plans/asset-metadata-cache/asset-metadata-cache-prd.md`: the two bounds at `:480-486`.
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`: the `task-012` entry.
- `source/main/governance/anchorCache.ts:10-31` and `:80-127`, the named precedent.

## Docs, Workflows, and Skills Consulted

- Docs: `.agent/plans/asset-metadata-cache/task-plans/readme.md`, `CLAUDE.md`.
- Workflows: `.agent/workflows/test.md` for the Jest invocation.
- Skills: none apply.

## Live Repo Findings Verified For Planning

Verified at `03fe8f5ae` on branch `docs/asset-metadata-cache-plan`, 2026-09-14.

**The precedent, and the part of it that matters most.** `anchorCache.ts:24-25` sets
`ANCHOR_CACHE_MAX_ENTRIES = 8000` and `ANCHOR_CACHE_MAX_BYTES = 128 * 1024 * 1024`, and `:29-31`
derives `ANCHOR_CACHE_SWEEP_FLOOR` as `MAX_BYTES / ANCHOR_MAX_BYTES` with the comment that below that
many files neither bound can bite. `:90` returns early below the floor. The derivation is the part
worth copying: the floor is computed from the two caps rather than typed, so they cannot drift.

**Here the floor is 256 and the arithmetic is the same.** 64 MiB divided by the 256 KiB per-entry cap
from `task-011` is 256 entries. Below that the byte bound cannot be crossed, and the entry bound of
2,000 is far above it, so below 256 rows neither bound can bite.

**Why the floor matters more here than it does for anchors.** Anchors are files and the sweep stats
them. `asset_image` rows carry a blob, and SQLite reads a whole row to reach any column of it, which
is the reason the PRD gives at `:337-339` for keeping images out of `asset_metadata` in the first
place. So `sum(byte_length)` over this table reads every blob. At the measured median of 23,251 bytes
a full sum over a table at the entry bound moves about 45 MiB. `count(*)` does not: it can be
answered from the rowid index without reading a row. Counting first and summing only above the floor
is therefore not a micro-optimisation, it is the difference between a bounded write and one that
re-reads the whole table.

**The index the ordering uses already exists.** `asset_image_fetched_at ON asset_image (fetched_at)`
is created in `assetMetadataDb.ts` as the PRD specifies. It orders the eviction. It does not cover
`byte_length`, so the walk that decides how many rows to delete still reads rows; adding a covering
index would be a schema change the PRD does not specify and is not taken.

**Updating `fetched_at` on a read rewrites the blob.** SQLite has no partial row update: an `UPDATE`
of one column rewrites the row, blob included. A token row that renders sixty times a minute would
write its 23 KiB image back sixty times a minute for nothing. The ordering this feeds needs to
distinguish images used today from images used last month, not images used this second from images
used last second, so the touch is applied only when the stored value is more than an hour old.

**`asset_image` cascades from `asset_metadata`, and not the other way.** The foreign key is
`REFERENCES asset_metadata (subject) ON DELETE CASCADE` on `asset_image.subject`, so deleting an
image cannot touch a metadata row. `task-006`'s spec already asserts the foreign key is live.

**Nothing collides.** `grep -rn "enforceImageBounds\|touchImage" source tests` returns nothing.

## Files Expected To Change

- `source/main/assets/assetMetadataDb.ts` — two methods added.
- `source/main/assets/assetImageStore.ts` — two constants, the touch on read, the enforcement after a
  write.
- `source/main/assets/assetImageStore.realfs.spec.ts` — cases.
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json` — `task-012` status.
- `.agent/plans/asset-metadata-cache/task-plans/task-012*.md` — this plan and its two review logs.

The task graph's `targetPaths` name `source/main/assets/assetImageStore.ts` and
`source/main/assets/assetDatabase.ts`. The second is the same module under a different name; it is
`assetMetadataDb.ts`, which is what `task-006` created and what every other phase-2 task imports. The
module is not renamed to match a path that was written before it existed.

## Implementation Approach

1. **Two bounds, both enforced, neither sufficient alone.** `ASSET_IMAGE_MAX_ENTRIES = 2000` and
   `ASSET_IMAGE_MAX_TOTAL_BYTES = 64 * 1024 * 1024`. At the measured median entry size they bite at
   roughly the same point, which is the intent: the entry bound is what stops a flood of tiny images,
   and the byte bound is what stops a handful of large ones.

2. **The sweep floor is derived, not typed, and its invariant is enforced by the table.**
   `Math.floor(ASSET_IMAGE_MAX_TOTAL_BYTES / ASSET_IMAGE_MAX_BYTES)`, which is 256. Below that row
   count, enforcement returns after a `count(*)`, having read no blob. The three constants cannot
   drift apart because two of them compute the third.

   That reasoning holds only if no row exceeds the per-entry cap, so `writeImage` refuses a row over
   it rather than relying on its caller to have checked. A cap enforced in one module and depended on
   in another is a convention; enforced at the write it is a property of the data, and the difference
   matters because the failure it prevents is a bound that looks enforced and is not.

3. **Enforcement, in four steps.**

   ```
   count(*)                      -> below the floor, return
   count(*) and sum(byte_length) -> inside both bounds, return
   oldest first by fetched_at    -> walk, accumulating, until both bounds are satisfied
   delete that set in one statement
   ```

   The walk decides the set; a single `DELETE ... WHERE subject IN (...)` applies it, so there is one
   write rather than one per eviction.

4. **`touchImage` is rate-limited, and the rate is the reason it exists.** A read updates
   `fetched_at` only when the stored value is more than an hour old. SQLite rewrites the whole row on
   an update, blob included, so an unconditional touch would write tens of kilobytes per rendered
   row per paint. An hour's granularity is far finer than the ordering needs, which is to tell an
   image used today from one used last month.

5. **Enforcement runs after a write and nowhere else.** That is the only moment a bound can be
   crossed. A read can only move a row later in the ordering.

6. **The store's bounds are injectable and default to the exported constants.** The same shape as the
   clock already injected there. Test-sized bounds prove the algorithm; the default proves the
   shipped path carries the shipped numbers, and those are different questions.

7. **Eviction touches `asset_image` only.** The foreign key cascades from metadata to image, not the
   reverse, so this is structural rather than a rule to remember. The spec asserts it anyway, because
   the property is the reason the two tables are separate.

## Acceptance Criteria

Carried from the task graph, with the check that settles each.

1. **`yarn test:jest` passes.**
2. **Neither bound can be exceeded after a write.** Asserted after crossing each bound in turn, and
   after crossing both at once.

Six this task adds to its own closure:

3. Exceeding the entry bound evicts the oldest first, asserted by which subjects survive rather than
   only by how many.
4. Exceeding the byte bound evicts until it is satisfied and no further, so a sweep does not empty the
   table.
5. Eviction leaves the corresponding `asset_metadata` rows untouched.
6. Below the sweep floor, enforcement reads no blob and evicts nothing.
7. A read updates `fetched_at` when the stored value is older than the interval and does not when it
   is newer, which is what makes the ordering least recently used rather than least recently written.
8. `compile`, `lint` and `i18n` are green from `nix build`, `package.json` and `yarn.lock` are
   unchanged, and there are no new `@ts-ignore` or `@ts-expect-error`.

## Verification Plan

The bounds are driven at test-sized values by passing them into `enforceImageBounds`, so a case does
not have to write 64 MiB of blobs to cross a bound. The shipped constants are asserted separately,
and the derivation of the floor from them is asserted arithmetically.

**The entry bound.**
- With five entries and a bound of three, the two oldest by `fetched_at` are gone and the three
  newest remain, asserted by subject.
- With exactly the bound, nothing is evicted.

**The byte bound.**
- With five entries of known size and a byte bound that admits three of them, the two oldest are
  gone and the total is under the bound.
- Eviction stops as soon as both bounds are satisfied: a bound that admits four does not evict a
  fifth.

**Both together.**
- A set that violates the entry bound but not the byte bound evicts, and the reverse evicts, and a
  set that violates both ends inside both.

**The floor.**
- Below the floor, enforcement returns zero even when the byte bound is nominally exceeded, which is
  the behaviour the derivation buys and is what proves the early return is real.
- The floor equals the byte bound divided by the per-entry cap, asserted from the exported constants.
- `writeImage` refuses a row one byte over the per-entry cap and accepts one exactly at it, which is
  the invariant the floor rests on.
- The store's default bounds equal the exported constants, and a fetch that stores an image runs
  enforcement with them rather than with anything else.

**Metadata.**
- After an eviction that removes four of five images, all five `asset_metadata` rows are still
  present with their `verified` values intact.

**The touch.**
- Reading a row whose `fetched_at` is two hours old advances it to the current clock.
- Reading a row whose `fetched_at` is one minute old leaves it unchanged.
- After a touch, an eviction that keeps one row keeps the touched one rather than the one written
  later, which is the only case that distinguishes least recently used from least recently written.

**After a write.**
- A fetch that stores an image with the table already at its entry bound leaves the table at the
  bound, not above it.

**Commands.**
- `nix build '.#checks.x86_64-linux.jest' --no-link`
- `nix build '.#checks.x86_64-linux.compile' --no-link`
- `nix build '.#checks.x86_64-linux.lint' --no-link`
- `nix build '.#checks.x86_64-linux.i18n' --no-link`

## Risks and Open Questions

1. **Above the floor, enforcement reads every row of `asset_image`.** `sum(byte_length)` cannot be
   answered from the one index the PRD specifies, and SQLite reads whole rows. At the entry bound
   that is roughly 45 MiB of reads on a write that crosses a bound. It is bounded, it happens only
   after a write above 256 entries, and the alternative is a covering index on `(fetched_at,
   byte_length)`, which is a schema change the PRD does not specify. Recorded with its cost rather
   than taken.
2. **`fetched_at` has one hour of granularity.** Two images read within the same hour are ordered by
   whichever was touched last before that, which for eviction purposes is arbitrary. The ordering
   exists to separate an image used this week from one used last quarter, and an hour is three orders
   of magnitude finer than that.
3. **Eviction has no notion of what the user holds.** An image for a token still in the wallet can be
   evicted ahead of one for a token sold last year, if the sold one was rendered more recently. The
   store is subject-keyed and does not know about wallets, and teaching it would put a wallet concept
   in a cache that was deliberately built without one. The cost is one re-fetch of one image.
4. No open questions for the project owner.

## Required Docs, Research, and Tracking Updates

- Update `task-012`'s `status` in `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`
  to `completed` when the implementation review reads `approved`.
- Append to `task-012-plan-review.md` and `task-012-impl-review.md` as the cycle requires.
- No PRD change. The bounds are its numbers and the schema is unaltered.
- The `assetDatabase.ts` naming inconsistency in this task's `targetPaths` is recorded here for the
  third and last time; the module is `assetMetadataDb.ts` and every phase-2 task imports it by that
  name.

## Review-Log Paths

- Planning review log: `.agent/plans/asset-metadata-cache/task-plans/task-012-plan-review.md`
- Implementation review log: `.agent/plans/asset-metadata-cache/task-plans/task-012-impl-review.md`

## Planning Status

`approved`

## Build Status

`completed`

## Current Outcome

- Plan drafted, critiqued once and revised. Approved for build execution.

## Final Outcome

- `task-012` complete. Reviewed and approved in `task-012-impl-review.md`.
- `asset_image` is bounded at 2,000 entries and 64 MiB, evicting the least recently fetched, with the
  sweep floor derived from the two caps rather than typed.
- The per-entry cap moved into `writeImage`, because the floor's correctness depends on no row
  exceeding it and that was previously a caller's discipline rather than a property of the table.
- `fetched_at` is advanced on a read only when it is more than an hour old: SQLite rewrites a whole
  row on an update, blob included, so an unconditional touch would write tens of kilobytes per
  rendered row per paint.
- One case in the Verification Plan could not be written as specified. It asked for evidence that the
  sweep floor's early return fires, and an optimisation defined by not changing an outcome has no
  outcome that distinguishes it. The correction is recorded in the implementation review.
- Checks, all from `nix build`: `compile` exit 0, `lint` exit 0, `i18n` exit 0, `jest` 81 suites and
  1199 tests with 1196 passed and 3 skipped. `package.json` and `yarn.lock` are unchanged.

## Self-Review

- The sweep floor is copied from the precedent as a derivation rather than as a number, which is the
  part of `anchorCache` worth copying.
- The one cost this design accepts, a full-table read above the floor, is stated with its size and
  with the change that would remove it, and the reason that change is not taken.
- The touch rate limit exists because SQLite rewrites a whole row on update, which is the same fact
  the PRD used to justify separating the two tables in the first place.
