Implementation: Iteration 1
Timestamp: 2026-09-15T01:50:00Z

Changes made:
- `source/main/assets/assetImageStore.ts`: new. Magic-byte media-type detection, the single-subject logo query, the per-entry cap, the in-flight map and the record of subjects with no logo.
- `source/main/assets/assetMetadataDb.ts`: `readImage` and `writeImage` added beside the four existing accessors, with `AssetImageWrite` and `AssetImageRow`.
- `source/main/assets/assetImageStore.realfs.spec.ts`: new. Twenty-two cases across three groups.

Files touched:
- `source/main/assets/assetImageStore.ts`
- `source/main/assets/assetImageStore.realfs.spec.ts`
- `source/main/assets/assetMetadataDb.ts`
- `.agent/plans/asset-metadata-cache/task-plans/task-011.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-011-plan-review.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-011-impl-review.md`
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`

Two details decided during implementation:

`writeImage` returns a boolean rather than a count. The only interesting outcome is whether the foreign key accepted the row, and a count of one or zero says the same thing less clearly. The foreign-key refusal is asserted directly in the spec, both through the store and through the database method.

The spec's PNG fixture is a signature followed by `00 ff 7f 80 01`. Those bytes do not survive any text encoding, so the round-trip case fails if the column is ever anything but a BLOB, rather than passing because the payload happened to be ASCII.

Verification run:

- `jest source/main/assets/assetImageStore --coverage=false` — 22 passed.
- Media-type detection: one case each for PNG, JPEG, GIF and WebP built from their signatures, and refusals for an SVG document, plain text, an empty buffer and a two-byte prefix of a PNG signature.
- The request carries exactly one subject and exactly the property `logo`, asserted on the serialized body.
- A PNG is stored with `media_type` `image/png`, `byte_length` equal to the decoded length and `fetched_at` from the injected clock, and the stored bytes compare equal to the original.
- A second fetch for a stored subject makes no request.
- An entry one byte over the cap is discarded and the subject records no image.
- An SVG payload is discarded and the subject records no image.
- A response answering without a logo is remembered: the second fetch makes no request. A transport failure is not remembered: the second fetch does make one, which is the distinction between a fact and an outage.
- An entry naming a different subject is refused, so a server answering a question nobody asked cannot put one asset's picture on another.
- A 404, a 500, a transport failure and a non-JSON body each store nothing without throwing.
- Two concurrent fetches for the same subject issue one request and both receive a row.
- An image for a subject with no metadata row is refused by the foreign key, through the store and directly through `writeImage`.

Measurement backing the constants, taken 2026-09-14 over 400 registry subjects queried for the logo property alone: 374 carry one, all 374 are PNG by magic bytes, sizes run from 806 to 65,249 bytes with a median of 23,251, and none approaches the 256 KiB cap.

Checks, all four through Nix:
- `nix build '.#checks.x86_64-linux.compile' --no-link` — exit 0.
- `nix build '.#checks.x86_64-linux.lint' --no-link` — exit 0.
- `nix build '.#checks.x86_64-linux.i18n' --no-link` — exit 0.
- `nix build '.#checks.x86_64-linux.jest' --no-link -L` — 81 suites passed, 1186 tests with 1183 passed and 3 skipped, exit 0. The previous state of this branch was 80 suites and 1164 tests, so one suite and twenty-two tests were added and nothing else moved.

`nix fmt` reported no changes, the files having been written in its style.

No new `@ts-ignore` and no new `@ts-expect-error`. `git diff HEAD -- package.json yarn.lock` produces nothing.

Deviations from the approved plan:
- None in scope or approach. Two methods were added to `assetMetadataDb.ts`, which the plan recorded in advance as outside this task's `targetPaths` and inside `task-012`'s.

Outcome: Implementation complete and ready for review

Review of Iteration 1
Timestamp: 2026-09-15T01:57:00Z

Acceptance criteria, each against the evidence:

1. *`yarn test:jest` passes.* Met.

2. *The bulk query still does not request the `logo` property.* Met structurally rather than by convention: `ASSET_REGISTRY_PROPERTIES` is a module constant of five names, it is the only input to `assetRegistryRequestBody`, and this task added no parameter to either. The registry client spec already asserts the serialized bulk body does not contain the string.

3-4. *An over-cap entry and a non-image payload are each discarded and record no image.* Met, asserted on both the return value and a subsequent read.

5. *A stored entry round-trips byte for byte.* Met, with a fixture chosen so that a text column would fail it.

6. *The request carries one subject and one property.* Met.

7. *A foreign-key refusal is reported rather than thrown.* Met, through both layers.

8. *Checks green, no new suppressions, `package.json` unchanged.* Met.

The distinction between a remembered absence and an unremembered failure is the detail most likely to be lost in a later refactor, and it has a case on each side. Remembering a failure would hide a logo until the application restarts; not remembering an absence would re-ask on every render for the twenty-six subjects in four hundred that have no logo.

One thing for `task-012`, which extends both of these files. `read` is currently a plain read. The eviction task's own notes require `fetched_at` to be updated on read so the ordering means least recently used rather than least recently written, so that touch belongs there along with the two bounds, and the bounds have to be enforced after a write rather than before one.

Summary: Logos are fetched one subject at a time, only for assets something has decided to render, and only through a query that cannot ask for more than one. The type comes from the bytes rather than from the response, the cap is four times the largest entry measured across 400 real logos, and nothing here can enumerate subjects to warm the table.

Decision: approved
