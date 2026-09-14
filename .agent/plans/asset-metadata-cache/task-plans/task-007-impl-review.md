Implementation: Iteration 1
Timestamp: 2026-09-14T21:50:00Z

Changes made:
- `source/main/assets/assetRegistryClient.ts`: new. Endpoint resolution, byte-sized batching, an HTTP transport behind an interface, one rule per status class, the terminating 413 split, response normalisation and one resolution row per distinct subject.
- `source/main/assets/assetRegistryClient.spec.ts`: new. Thirty-eight cases across seven groups.

Files touched:
- `source/main/assets/assetRegistryClient.ts`
- `source/main/assets/assetRegistryClient.spec.ts`
- `.agent/plans/asset-metadata-cache/task-plans/task-007.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-007-plan-review.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-007-impl-review.md`
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`

One finding the plan did not anticipate, and it will recur in every module in this phase:

The first implementation narrowed the transport result with `if (!result.ok)`, and `compile` failed with `TS2339: Property 'reason' does not exist on type 'RegistryTransportResult'` at both sites. `tsconfig.json:158` runs with `strict: false`, and under that setting TypeScript does not narrow a discriminated union by the truthiness of a boolean-literal discriminant. It does narrow by equality. Reproduced in isolation against this tree's TypeScript 4.9.5 with a four-line file and the repository's compiler options: `if (r.ok === false)` compiles and `if (!r.ok)` does not. Both call sites now compare with `===`, and a comment at `assetRegistryClient.ts:347-349` records why so the shorter form is not restored later as a tidy-up.

One detail decided during implementation:

The byte accounting for a batch pays for the separating comma only when the subject is not first in its batch, and that has to be computed after the decision to close the batch rather than before it. The first version computed the cost once, before the close, so the first subject of each batch after the first was charged a comma it did not have. The arithmetic now recomputes, and the spec asserts the invariant directly rather than the increments: every batch, serialized, is at most 6,144 bytes.

Verification run:

- `jest source/main/assets/assetRegistryClient --coverage=false` — 38 passed.
- Endpoint resolution: an override wins; the launcher value is used without one; the selfnode branch composes `http://127.0.0.1:41531` from the two constants and is asserted not to equal `https://tokens.cardano.org`, which is the failure the PRD says is silent; the mainnet literal applies when nothing else does. The query path is asserted for endpoints with and without a trailing slash.
- Batching: 100 maximum-length subjects produce more than one batch and account for all 100; 500 of them produce batches every one of which is at most 6,144 serialized bytes; a single subject twice the ceiling is sent alone rather than dropped; no subjects produce no batches.
- The request body carries exactly the five properties and the string `logo` does not appear in it.
- A subject supplied three times is requested once and resolves once.
- Status classes: 400, 401, 403, 404 and 429 each produce one call and a `failed` subject; 500 and 503 each produce two; a timeout and a network error each produce two; an over-sized response produces one and is not retried; a 200 carrying malformed JSON produces one and fails the batch without throwing.
- The 413 split: two subjects refused, then answered individually, produce three calls and two resolved subjects. A stub that refuses everything produces three calls and no more. A batch of one is not split. A split where one half succeeds and the other times out twice resolves one subject and fails the other, which is what proves a resolution belongs to a subject rather than to the original batch.
- Normalisation: an entry for a subject that was not requested is dropped and that subject records `unregistered`; after a split, an entry belonging to the other half is dropped by the half that did not ask for it; a property with a non-numeric `sequenceNumber` is dropped while the rest of the entry survives; a property whose signatures are bare strings is dropped; an entry with no `policy` is kept with `policy` null and resolves, because that is the ordinary state of a large share of the registry.
- Backoff: doubling from five minutes, flat at twenty-four hours from the first count that reaches it and the one after; a subject carrying an incoming failure count of 3 gets a longer instant than one at 0 from the same call; `retryAfter` equals the `now` handed in plus the backoff, which is what makes it an instant rather than a duration.
- Sequencing: 300 subjects produce more than one batch and the stub records no overlap, so no request began before the previous one resolved.

Live measurement backing the constants, taken 2026-09-14 against `https://tokens.cardano.org/metadata/query` with the five properties this client requests: 6,099 request bytes returned 200 with a 94,772-byte response, 8,067 and 8,190 returned 200, and 8,682, 9,912 and 11,142 returned 413. The cap sits between 8,190 and 8,682, consistent with 8,192, and the 6 KB ceiling leaves about 2 KB of headroom at the worst case measured. The largest response measured was 103,314 bytes against a 1 MiB read cap.

Checks, all four through Nix with both new files staged:
- `nix build '.#checks.x86_64-linux.compile' --no-link` — exit 0.
- `nix build '.#checks.x86_64-linux.lint' --no-link` — built `g6z0wz26vjdpnrpkbjx6qb9fk0cmdfsk-daedalus-lint.drv`, exit 0.
- `nix build '.#checks.x86_64-linux.i18n' --no-link` — exit 0.
- `nix build '.#checks.x86_64-linux.jest' --no-link -L` — built `9m2is34f8dmbnbsdrbvkzg8rma5vff8b-daedalus-jest.drv`, 78 suites passed, 1078 tests with 1075 passed and 3 skipped, exit 0. The previous state of this branch was 77 suites and 1040 tests, so one suite and thirty-eight tests were added and nothing else moved.

`nix fmt` was run and changed the two new files before the checks.

No new `@ts-ignore` and no new `@ts-expect-error`. `git diff HEAD -- package.json yarn.lock` produces nothing.

Deviations from the approved plan:
- None in scope or approach. The database boundary is where the plan put it, recorded under Risks before the work rather than discovered during it.

Outcome: Implementation complete and ready for review

Review of Iteration 1
Timestamp: 2026-09-14T21:58:00Z

Acceptance criteria, each against the evidence:

1. *Sized by serialized request body bytes with a 6 KB ceiling, and 100 maximum-length subjects produce more than one batch.* Met. The stronger assertion is the second one: every batch of 500 maximum-length subjects is measured after serialization, so the invariant is checked rather than the arithmetic that is supposed to produce it.

2. *A 413 splits the batch once and re-sends; a second 413 is recorded without a retry.* Met, with the base case the critique asked for: a batch of one is never split, so the recursion terminates on input as well as on depth.

3. *No 4xx is ever scheduled for retry, asserted per status code.* Met over five codes, each asserting the call count and not only the state, because a state of `failed` would look the same whether or not a retry happened.

4. *Subjects absent from a 200 response are recorded `unregistered`.* Met in substance. The rows are produced here and written by `task-010`; the division is argued in the plan from two other entries in the same task graph, and the row type comes from `task-006` so a state the schema refuses cannot be named.

5. *No timer schedules unsolicited work.* Met. The module contains no `setInterval`, and both `setTimeout` calls are inside work a caller asked for: the request budget and the single retry backoff, which the spec drives at zero.

6. *`yarn test:jest` passes against a stubbed transport.* Met.

7. *Selfnode resolves to the bundled mock.* Met as far as this task can settle it. The branch is unit-tested and asserted not to be the mainnet literal; the end-to-end case and its evidence are written out for `task-027`.

8-11. *Checks green, no new suppressions, over-sized response discarded, no logo in the body.* All met.

The narrowing finding is the one to carry. `strict: false` is repository-wide, so every discriminated union in this phase has the same trap, and it fails at compile rather than at runtime, which is the good version of it. `task-008` through `task-012` should reach for `=== false` from the start.

One thing this module deliberately does not do, worth restating so a later reader does not take it for a gap: it performs no address-range filtering. `AnchorFetchService` does, because a DRep anchor URL comes from the chain. The registry host is configuration today. When `task-031` turns it into a URL a user types, that question is worth asking on its own terms.

Summary: The client asks in bytes rather than in subjects, which is the finding the live measurement sharpened: the same subject count that returned 200 in the PRD's sample returned 413 here, because those subjects were longer. Every subject handed in comes back with an outcome, including the one the registry silently omits, which is the record that stops an NFT wallet re-asking forever. Nothing in the module can set `verified`, and nothing in it opens the database.

Decision: approved
