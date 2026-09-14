# Task task-011: Image store and its on-demand fetch

## Task ID and Title

- ID: `task-011`
- Title: `Image store and its on-demand fetch`

## Why Chosen Now

Both dependencies are complete: `task-006` at `084854a71` and `task-007` at `49a0ed3b3`. It is the
last phase-2 module that stands on its own, and `task-012` cannot bound a table nothing writes to.

## Interaction Mode

- Mode: `agent_execution`

A stubbed transport and a temporary database cover every criterion.

## Scope

- A new `source/main/assets/assetImageStore.ts`: the single-subject logo query, the base64 decode,
  the media-type check, the per-entry byte cap, and the read.
- `source/main/assets/assetMetadataDb.ts` gains `readImage` and `writeImage`, because `asset_image`
  is its table and the schema is one unit.
- A colocated `source/main/assets/assetImageStore.realfs.spec.ts`.

## Non-Goals

- No eviction. The two bounds are `task-012`, which extends the same two files.
- No bulk mirror and no warming. There is no code path here that enumerates subjects.
- No logo in the bulk query. The bulk query's property list is a module constant in
  `assetRegistryClient.ts` and this task does not touch it, which is what keeps the volume bounded at
  the transport rather than only in the store.
- No IPC. `task-014` decides how a rendered row asks for an image.
- No image transformation, resizing or re-encoding. Bytes in, bytes out.

## Dependencies

- `task-006` and `task-007`, both complete.

## Research Consulted

- `.agent/plans/asset-metadata-cache/asset-metadata-cache-prd.md`: the image bounds at `:474-486`,
  the schema at `:363-372`, and the note at `:488-491` that a chain-sourced image is a URI and is out
  of scope.
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`: the `task-011` entry and
  `task-012`.
- `.agent/plans/asset-metadata-cache/task-plans/task-009.md`, for the logo value encoding.

## Docs, Workflows, and Skills Consulted

- Docs: `.agent/plans/asset-metadata-cache/task-plans/readme.md`, `CLAUDE.md`.
- Workflows: `.agent/workflows/test.md` for the Jest invocation.
- Skills: none apply.

## Live Repo Findings Verified For Planning

Verified at `97a498cc2` on branch `docs/asset-metadata-cache-plan`, 2026-09-14.

**Every logo in the sample is a PNG, and none is close to the cap.** 400 registry subjects were
queried for the `logo` property alone, in twenty batches of twenty:

| Measure | Value |
|---|--:|
| Subjects asked | 400 |
| Carrying a logo | 374 |
| Detected as PNG by magic bytes | 374 |
| Detected as anything else | 0 |
| Smallest | 806 bytes |
| Median | 23,251 bytes |
| Largest | 65,249 bytes |
| Over the 256 KiB per-entry cap | 0 |

So the cap has about four times headroom over the largest entry measured, and the media-type check
costs nothing on today's corpus while still refusing a format nobody has published.

**The media type is not declared, so it is detected.** The registry's `logo` property is bare base64
with no content type beside it. The type therefore comes from the decoded bytes: the live logo for
subject `c76ef54…42544544` decodes to bytes beginning `89504e470d0a1a0a`, the PNG signature.

**The schema already carries the table and its foreign key.**
`source/main/assets/assetMetadataDb.ts` creates `asset_image` with
`subject TEXT NOT NULL PRIMARY KEY REFERENCES asset_metadata (subject) ON DELETE CASCADE`,
`media_type TEXT NOT NULL`, `bytes BLOB NOT NULL`, `byte_length INTEGER NOT NULL` and
`fetched_at INTEGER NOT NULL`, plus the `asset_image_fetched_at` index that `task-012` will order by.
`task-006`'s spec asserts that inserting an image for a subject with no metadata row raises, so the
foreign key is live and an image can only exist for a subject the cache already knows.

**`node:sqlite` round-trips a BLOB as a `Uint8Array`.** Measured in `task-006`: a `Uint8Array`
written to a `BLOB` column comes back as a `Uint8Array` with identical bytes.

**The transport is already available and already capped.** `assetRegistryClient.ts` exports
`httpRegistryTransport`, `assetRegistryEndpoint`, `assetRegistryQueryUrl` and
`ASSET_REGISTRY_TIMEOUT_MS`, with a 1 MiB response cap. A single logo at the measured maximum is six
percent of that.

**The bulk query cannot ask for a logo.** `ASSET_REGISTRY_PROPERTIES` in `assetRegistryClient.ts` is
a module constant of five names and is the only thing `assetRegistryRequestBody` reads. This task
adds no parameter to it, so there is no call site that could pass `logo` into a bulk request.

**Nothing collides.** `grep -rn "assetImageStore\|asset_image" source tests` returns only
`assetMetadataDb.ts` and its spec.

## Files Expected To Change

- `source/main/assets/assetImageStore.ts` — new.
- `source/main/assets/assetImageStore.realfs.spec.ts` — new. The task graph names
  `assetImageStore.spec.ts`; this spec drives a real database file, so it takes the
  `<Unit>.realfs.spec.ts` name for the reason `task-006` recorded.
- `source/main/assets/assetMetadataDb.ts` — two methods added. Outside the task graph's
  `targetPaths` for this task, and inside `task-012`'s, which names
  `source/main/assets/assetDatabase.ts` for the same module under a different name. Recorded under
  Required Docs.
- `source/main/assets/assetMetadataDb.realfs.spec.ts` — cases for the two methods.
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json` — `task-011` status.
- `.agent/plans/asset-metadata-cache/task-plans/task-011*.md` — this plan and its two review logs.

## Implementation Approach

1. **The database owns its table.** `readImage(subject)` and `writeImage(row)` go on
   `AssetMetadataDatabase` beside the four accessors already there, wrapped the same way, so a
   failure degrades to no image rather than throwing. `writeImage` returns whether the row was
   stored, which is how a caller learns the foreign key refused it.

2. **Media type is detected from the bytes, never taken from the response.** A small magic-number
   check accepting PNG, JPEG, GIF and WebP. Everything else is refused, and the entry is not stored.

3. **SVG is refused explicitly and that is a decision, not an omission.** It is markup rather than
   raster bytes, it can carry script, and the renderer this would be displayed in runs with
   `nodeIntegration: true` (`source/main/windows/main.ts:51`). The measured cost of refusing it is
   zero: none of the 374 logos sampled is one. The controls that matter for a raster image are the
   byte cap and the type check, both of which are here.

4. **The fetch is one subject and one property.** A body of
   `{"subjects":["<subject>"],"properties":["logo"]}` posted through the shared transport at the
   shared timeout. A 200 is parsed and anything else yields nothing. There is no retry: a logo is
   cosmetic, and the next render can ask again.

   An entry whose subject is not the one requested is refused. The bulk client refuses the same
   thing for the same reason: a server answering a question nobody asked must not be able to create a
   row, and here the consequence would be one asset wearing another's logo.

5. **Concurrent fetches for one subject share a request.** An in-flight map keyed by subject returns
   the same promise to a second caller. A token list is thirty components each deciding independently
   whether to show a picture, so this is the ordinary case rather than a race to defend against.

6. **A subject with no logo is remembered for the process lifetime.** A response that answers without
   a `logo` property is recorded in an in-memory set, so a token row rendered repeatedly does not
   re-ask on every render. It is deliberately not a database row: `asset_resolution` is keyed by
   subject and records metadata resolution, and overloading it would make one subject's metadata
   state depend on whether its picture exists. The set is lost on restart, which is the right
   trade for a fact this cheap to rediscover.

7. **Order of checks: decode, cap, type, store, and the order is load-bearing.** The cap is applied
   to the decoded length because base64 length is not payload length. Decoding first is safe only
   because the transport's 1 MiB response cap has already bounded the input: `Buffer.from(x,
   'base64')` is lenient and skips characters it cannot read rather than refusing, so a malformed
   value decodes to something rather than failing, and it is the media-type check that then refuses
   whatever that turned out to be. Moving the cap check before the decode to save work would remove
   the bound that makes the decode safe.

8. **The row stores the decoded bytes, not the base64.** A `Uint8Array` into the `BLOB` column, with
   `byte_length` recorded alongside so `task-012` can sum a bound without reading every blob.

## Acceptance Criteria

Carried from the task graph, with the check that settles each.

1. **`yarn test:jest` passes.**
2. **The bulk query in `task-007` still does not request the `logo` property.** Asserted against the
   serialized bulk body, in the existing registry client spec, which already has that case.

Six this task adds to its own closure:

3. An entry over the per-entry cap is discarded and the subject records no image.
4. A non-image payload is refused and the subject records no image.
5. A stored entry round-trips byte for byte, including a byte sequence that is not valid UTF-8, which
   is what proves the column is a BLOB rather than text.
6. The single-subject request carries exactly one subject and exactly the `logo` property.
7. An image for a subject with no metadata row is refused by the foreign key and reported rather than
   throwing.
8. `compile`, `lint` and `i18n` are green from `nix build`, `package.json` and `yarn.lock` are
   unchanged, and there are no new `@ts-ignore` or `@ts-expect-error`.

## Verification Plan

**Media type detection.** One case per accepted type, built from its magic bytes, plus refusals for
an SVG document, a plain-text payload, an empty buffer and a buffer too short to carry any signature.

**The fetch.**
- The request body carries one subject and the single property `logo`, asserted on the serialized
  body.
- A 200 carrying a PNG stores the row, with `media_type` `image/png` and `byte_length` equal to the
  decoded length.
- A 200 carrying a payload over 256 KiB stores nothing, and `readImage` returns nothing afterwards.
- A 200 carrying an SVG stores nothing.
- A 200 with no `logo` property stores nothing, and a second fetch for the same subject makes no
  further request.
- A 200 carrying an entry for a different subject stores nothing.
- Two concurrent fetches for the same subject issue one request and both receive the stored row.
- A 404, a 500 and a transport failure each store nothing and are not retried within the call.
- A response that is not JSON stores nothing and does not throw.

**Storage.**
- A stored entry round-trips byte for byte. The fixture includes `0x00` and `0xff` bytes so a text
  round trip would corrupt it.
- Writing an image for a subject with no metadata row returns false and stores nothing.
- Reading an absent subject returns nothing.

**Commands.**
- `nix build '.#checks.x86_64-linux.jest' --no-link`
- `nix build '.#checks.x86_64-linux.compile' --no-link`
- `nix build '.#checks.x86_64-linux.lint' --no-link`
- `nix build '.#checks.x86_64-linux.i18n' --no-link`

## Risks and Open Questions

1. **A logo is stored whether or not its attestation verifies, and that is a choice.** The schema has
   no column to record an image's verification state, the PRD never gates an image on one, and a logo
   is a display value in the same class as a name. Gating would hide the logo for the 42 percent of
   registry subjects that carry no policy field at all. Verification would also not be the control
   that matters: a minter can sign their own malformed PNG, so what protects the renderer is the byte
   cap and the media-type check, both of which are here. Named so the project owner can say
   otherwise; nothing downstream depends on the answer.
2. **The negative cache is in memory and per process.** A subject with no logo is re-asked once per
   application run. At one request per subject per run this is not a volume problem, and the
   alternative, a row in `asset_resolution`, would tangle one subject's metadata state with whether
   its picture exists.
3. **The sample is 400 subjects of 7,977.** It says every logo in it is a PNG under 65 KiB. It does
   not say no registry subject anywhere publishes a GIF or a 300 KiB image, which is exactly why the
   type check and the cap exist rather than being replaced by an assumption.
4. **`writeImage` can be refused by the foreign key, and that is not an error.** It means the
   metadata row was evicted or never existed. The caller is told and nothing is logged as a failure.
5. No open questions beyond item 1.

## Required Docs, Research, and Tracking Updates

- Update `task-011`'s `status` in `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`
  to `completed` when the implementation review reads `approved`.
- Append to `task-011-plan-review.md` and `task-011-impl-review.md` as the cycle requires.
- No PRD change. The logo measurements agree with it and sharpen its median.
- One task-graph inconsistency, already recorded under `task-006` and repeated here because this is
  where it first bites: `task-011`'s `targetPaths` name only `assetImageStore.ts`, but the table it
  reads and writes belongs to `assetMetadataDb.ts`, which `task-012`'s `targetPaths` acknowledge
  under the name `assetDatabase.ts`. The module is not renamed; the two methods go where the table
  is.

## Review-Log Paths

- Planning review log: `.agent/plans/asset-metadata-cache/task-plans/task-011-plan-review.md`
- Implementation review log: `.agent/plans/asset-metadata-cache/task-plans/task-011-impl-review.md`

## Planning Status

`approved`

## Build Status

`completed`

## Current Outcome

- Plan drafted, critiqued once and revised. Approved for build execution.

## Final Outcome

- `task-011` complete. Reviewed and approved in `task-011-impl-review.md`.
- Measured over 400 registry subjects: 374 carry a logo, every one a PNG, 806 to 65,249 bytes with a
  median of 23,251, and none approaches the 256 KiB per-entry cap. Refusing SVG therefore costs
  nothing measurable.
- The media type is detected from the decoded bytes, because the registry declares none.
- A subject the registry answers without a logo is remembered for the process lifetime; a transport
  failure is not, because one is a fact and the other is an outage.
- Volume stays bounded at the transport rather than only in the store: the bulk query's property list
  is a module constant this task did not parameterise, so no call site can ask for a logo in bulk.
- Checks, all from `nix build`: `compile` exit 0, `lint` exit 0, `i18n` exit 0, `jest` 81 suites and
  1186 tests with 1183 passed and 3 skipped. `package.json` and `yarn.lock` are unchanged.

## Self-Review

- The media-type policy is decided from a measurement of 400 real logos rather than from the PRD's
  sentence about inline base64 PNG, and the measurement is what makes refusing SVG cost nothing.
- The one thing this task does that the PRD does not specify, storing an unverified logo, is argued
  and handed to the owner rather than assumed either way.
- The property that keeps logo volume bounded is structural, not a rule: the bulk query's property
  list is a constant this task does not parameterise.
