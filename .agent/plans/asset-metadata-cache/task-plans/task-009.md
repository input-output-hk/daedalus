# Task task-009: Verification step three: the attestation payload and ed25519, gated on the corpus

## Task ID and Title

- ID: `task-009`
- Title: `Verification step three: the attestation payload and ed25519, gated on the corpus`

## Why Chosen Now

`task-008` landed at `4c11dbf6c`, which satisfies this task's only dependency. Steps one and two
establish that a policy field belongs to a subject and that the signing keys satisfy the minting
policy. Neither says the metadata was signed. Until this step is in, `verified` cannot mean anything
and `task-010` has nothing to write into the column.

## Interaction Mode

- Mode: `agent_execution`

Every unit is a pure function of bytes. The corpus gate is a one-off run against the live registry
whose result is recorded on the branch; it is not a CI check and not a unit test, per the task graph.

## Scope

- `source/main/assets/assetVerification.ts` gains the attestation payload, the ed25519 verifier, and
  the one function that runs all three steps for one property.
- `source/main/assets/assetVerification.spec.ts` gains the cases for them.
- A corpus gate run over the whole registry, with its result recorded in the implementation review.

## Non-Goals

- No new dependency. Node's built-in `crypto.verify` is the verifier and nothing is promoted from a
  transitive position to a declared one.
- No database write and no network call from the module. The corpus run is a one-off script outside
  the repository, not code that ships.
- No `cardano-crypto.js`. It accepts a signature whose scalar has had the group order added to it,
  which is the one property this step cannot do without.
- No re-derivation of the payload construction by search. It is read from the registry's own
  implementation and confirmed against live signatures.

## Dependencies

- `task-008`, complete at `4c11dbf6c`.

## Research Consulted

- `.agent/plans/asset-metadata-cache/asset-metadata-cache-prd.md`: the payload at `:780-795`, the
  logo exception at `:797-802`, the primitive table at `:804-812`, and the strictness table at
  `:814-826`.
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`: the `task-009` entry and
  `task-010`.
- The registry's own implementation, named in the PRD:
  `token-metadata-creator/src/Cardano/Metadata/Types.hs`, `hashSubject` at `:128-129`,
  `hashProperty` at `:149-150`, `hashSequenceNumber` at `:530-533` and `isAttestedBy` at `:453-458`.

## Docs, Workflows, and Skills Consulted

- Docs: `.agent/plans/asset-metadata-cache/task-plans/readme.md`, `CLAUDE.md`.
- Workflows: `.agent/workflows/test.md` for the Jest invocation.
- Skills: none apply.

## Live Repo Findings Verified For Planning

Verified at `4c11dbf6c` on branch `docs/asset-metadata-cache-plan`, 2026-09-14.

**The payload construction verifies live, with the packages already in the tree.** Using `blake2b`
and `cbor` from this repository's `node_modules` and Node's built-in `crypto.verify`, the message

```
blake2b256( blake2b256(CBOR(subject))
         || blake2b256(CBOR(propertyName))
         || blake2b256(CBOR(value))
         || blake2b256(CBOR(sequenceNumber)) )
```

verifies for `name`, `ticker`, `url`, `description` and `decimals` on subject
`c76ef54…42544544`. The subject and the property name are CBOR text strings before hashing; hashing
them as raw UTF-8 reproduces nothing.

**The `logo` exception is confirmed rather than taken on trust.** For the live logo of that same
subject, 77,392 base64 characters, the payload built from `CBOR(base64 text)` does not verify and
the payload built from `CBOR(base64-decoded bytes)` does. The decoded bytes begin `89504e470d0a1a0a`,
a PNG header. `cbor.encode` of a `Buffer` produces a CBOR byte string, confirmed directly:
`cbor.encode(Buffer.from([1,2,3]))` is `43010203`.

**Node's verifier is strict and reaches an ed25519 raw key through a DER wrapper.**
`crypto.createPublicKey` takes a 32-byte raw key prefixed with the twelve-byte SPKI header
`302a300506032b6570032100`, and `crypto.verify(null, message, key, signature)` then verifies. Taking
a real registry signature and replacing its scalar `S` with `S + L` produces a signature Node
rejects.

**The corpus gate has been run, and its result is a finding rather than a formality.** Against the
live registry on 2026-09-14, over the whole mapping list at commit
`363982b999060874f80486c2758a8a19d3b5f78a`:

| Measure | Count |
|---|--:|
| Subjects in the registry's mapping list | 7,977 |
| Subjects the query endpoint answered | 7,977 |
| Carrying a `policy` field | 4,579 |
| Passing step one, policy binding | 4,579 |
| Property attestations examined | 19,209 |
| Passing step two, script evaluation | 19,209 |
| Passing step three, ed25519 | 19,208 |

One property in 19,209 does not verify: the `description` of subject
`29b3596c56b3862bd41a5ea87f3529d79dd0b56e036c1336751b3df9464f58`, value `The Hunter's Token`,
sequence number 0. The cause was established rather than assumed. The value is eighteen ASCII
characters with nothing unusual to encode. The same subject's `name`, `ticker` and `decimals` all
verify under the same public key. No variation of property name, sequence number from 0 to 3, or
plausible alternative value reproduces the signature. And the mapping file in the registry
repository at that commit carries byte-for-byte the same signature as the API returns, so this is a
record the registry accepted rather than something the API mangled in transit. It is a defect in the
registry's data, and the correct behaviour for Daedalus is exactly what the code does: that one
property shows unverified.

**Two subjects in 4,579 evaluate to true against an empty key set.** The full-population figure for
the item `task-008` handed forward, 0.04 percent, against the 1 in 405 measured on the sample.

**104 properties in the corpus carry more than one signature.** The PRD's sample found exactly one
entry with more than one distinct signing key. Verification therefore has to accept a property as
attested when any one of its signatures verifies, and the attesting key set for step two is the
union of the keys, which is what `task-008` already built.

**The primitives are already declared.** `blake2b` at `package.json:210`, `cbor` at
`package.json:218`, and `crypto` is a Node built-in. `grep -rn "@noble/curves" package.json` returns
nothing: it is transitive only, and it stays that way.

**The realm and narrowing traps still apply.** `blake2b` validates its input with a realm-sensitive
`instanceof`, and `tsconfig.json` runs with `strict: false`, so unions narrow by equality. Both are
already handled in the module this task extends.

## Files Expected To Change

- `source/main/assets/assetVerification.ts` — extended.
- `source/main/assets/assetVerification.spec.ts` — extended.
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json` — `task-009` status.
- `.agent/plans/asset-metadata-cache/task-plans/task-009*.md` — this plan and its two review logs.

No deviation from the task graph's `targetPaths`. `package.json` and `yarn.lock` are untouched, which
is one of the acceptance criteria.

## Implementation Approach

1. **The payload is one function and the property name decides one branch.**

   ```ts
   export const attestationPayload = (
     subject: string,
     propertyName: string,
     value: unknown,
     sequenceNumber: number
   ): Buffer | null
   ```

   Four `blake2b-256` digests, concatenated, hashed again. The only branch is `logo`, whose value is
   base64 text that is decoded to bytes and CBOR-encoded as a byte string. Everything else is
   `cbor.encode(value)` directly. A value that cannot be encoded, or a `logo` value that is not a
   string, returns `null` rather than throwing, because this runs over data from a remote server.

2. **The ed25519 verifier wraps the raw key in DER and lets Node do the rest.**

   ```ts
   export const verifyAttestationSignature = (
     payload: Buffer,
     signature: string,
     publicKey: string
   ): boolean
   ```

   The public key must be 64 hexadecimal characters and the signature 128, checked before either is
   decoded, because `Buffer.from(x, 'hex')` truncates rather than refusing. `createPublicKey` and
   `verify` are both wrapped, so a key Node rejects is a false rather than a throw.

3. **A property is attested when any one of its signatures verifies.** 104 properties in the corpus
   carry more than one, and the registry's rule is that an attestation is a set of signatures. One
   valid signature from a key inside a satisfiable branch is what step two already evaluates against
   the union of the keys.

4. **One function runs all three steps for one property.**

   ```ts
   export const verifyRegistryProperty = (
     subject: string,
     policy: string | null,
     propertyName: string,
     property: RegistryProperty
   ): PropertyVerificationResult
   ```

   returning `{ bound, satisfied, attested, verified }` where `verified` is the conjunction of the
   three. It is computed from the bytes here and is never read from a field of the response;
   `task-010` reads this field and writes the column. Verification is per property, because a subject
   may carry a verified ticker and an unverified decimals.

5. **Each of the three fields reports its own fact, and none is short-circuited.** A field named
   `attested` carrying `false` because nobody checked is indistinguishable from one carrying `false`
   because the signature is wrong, and phase 4's advisory has to tell those apart. The cost of
   always checking is one ed25519 verification on data that has already failed something else, which
   the corpus run puts at microseconds. `verified` is the conjunction of the three and is the only
   field a caller should test for a verdict.

6. **Two input checks that fail closed either way, and are therefore worth stating.** A property with
   an empty `signatures` array is not attested; that is the whole unattested class and it is most of
   the registry. And a `sequenceNumber` that is not an integer is refused rather than encoded, since
   a float encodes to different CBOR bytes and would produce a payload that silently never
   verifies.

## Acceptance Criteria

Carried from the task graph, with the check that settles each.

1. **The attestation payload is the 32-byte `blake2b256` over the concatenated `blake2b256` digests
   of `CBOR(subject)`, `CBOR(propertyName)`, `CBOR(value)` and `CBOR(sequenceNumber)`, asserted
   against five live properties of a known subject.** `name`, `ticker`, `url`, `description` and
   `decimals` of `c76ef54…42544544`, each with its real signature.
2. **Verification uses Node's built-in `crypto.verify`, and a signature with `S` replaced by `S + L`
   is rejected, asserted directly.** The malleable signature is constructed in the spec from the real
   one, so the test states the property rather than hard-coding a value someone has to trust.
3. **No new runtime dependency is added; `package.json` is unchanged by this task.**
4. **The full policy-bound corpus verifies.** Measured: 4,579 of 4,579 policy-bearing subjects bind,
   19,209 of 19,209 property attestations pass script evaluation, and 19,208 of 19,209 pass ed25519.
   The task graph asks for zero failures and the measurement is one, so the criterion is met on a
   stated basis rather than by the raw count: the one failure is a defect in the registry's own
   data, established by comparing the mapping file in the registry repository at the same commit,
   by the subject's three sibling properties verifying under the same public key, and by no
   alternative property name, sequence number or value reproducing the signature. A verifier that
   agreed with the registry on all 19,209 would be weaker evidence that it is checking anything.
5. **`yarn test:jest` passes.**

Five this task adds to its own closure:

6. A tampered value, a tampered sequence number, a signature valid for a different property of the
   same subject, and a signature valid for the same property of a different subject are each
   rejected, with a case each.
7. A property with no signatures is not attested, and a sequence number that is not an integer
   produces no payload.
8. The `logo` branch is asserted as a rule: the payload for a `logo` property equals the one built
   from the decoded bytes and differs from the one built from the base64 text.
9. A property with two signatures, one of them junk, is attested; with only the junk one it is not.
10. `compile`, `lint` and `i18n` are green from `nix build`.
11. No new `@ts-ignore` and no new `@ts-expect-error`.

## Verification Plan

**The payload, against live signatures.**
- Each of the five properties of `c76ef54…42544544` verifies with its real signature and public key.
- The subject is hashed as a CBOR text string: a payload built by hashing the subject as raw UTF-8
  is asserted to differ from the real one, so the trap the PRD names is pinned rather than described.
- The property name likewise.

**Negative cases, each driven.**
- A tampered value fails: the same call with `BitEd Token` changed by one character.
- A tampered sequence number fails: the same call with 0 changed to 1.
- The `ticker` signature checked against the `name` property of the same subject fails.
- The `name` signature checked against the same property of a different subject fails.
- A signature that is not 128 hex characters, and a public key that is not 64, are both rejected
  without throwing.
- A signature of the right length that is not a valid point is rejected without throwing.

**Strictness.**
- The real `decimals` signature verifies.
- The same signature with `S` replaced by `S + L`, computed in the spec from the group order, is
  rejected. The spec asserts the construction actually changed the signature, so a case where `S + L`
  overflowed and the test silently compared a signature to itself cannot pass.

**The logo branch.**
- For a `logo` property, the payload equals one built independently in the spec from
  `cbor.encode(Buffer.from(value, 'base64'))` and differs from one built from `cbor.encode(value)`.
- For every other property name, the payload equals the one built from `cbor.encode(value)`.
- A `logo` value that is not a string returns `null`.

**The three steps together.**
- A property of the worked example returns all four fields true.
- A property whose policy is absent returns `bound: false` and `verified: false`, and `attested`
  reports whether its signature actually verified rather than that nobody looked. For the worked
  example paired with a null policy, that is `attested: true` alongside `bound: false`, which is the
  distinction the field exists to carry.
- A property with an empty signature list is not attested.
- A sequence number that is not an integer produces no payload.
- A property that binds and satisfies but whose signature is tampered returns `attested: false` and
  `verified: false`.
- A property that binds but whose signing key does not satisfy the script returns `satisfied: false`
  and `verified: false` even though its signature is genuine, which is the middle class of the three
  the PRD names.
- Two signatures where one is junk still attests; the junk one alone does not.

**The corpus gate.** Fetch the registry's whole mapping list at a named commit, query the live
endpoint in 6 KB batches, and verify all three steps for every property of every policy-bearing
subject. Report the counts, and for any failure establish the cause before accepting or rejecting it:
compare against the mapping file in the repository at the same commit, check the subject's sibling
properties, and try the plausible alternative encodings. This is recorded on the branch in the
implementation review and is not a CI check.

**Commands.**
- `nix build '.#checks.x86_64-linux.jest' --no-link`
- `nix build '.#checks.x86_64-linux.compile' --no-link`
- `nix build '.#checks.x86_64-linux.lint' --no-link`
- `nix build '.#checks.x86_64-linux.i18n' --no-link`
- `git diff -- package.json yarn.lock` must be empty.

## Risks and Open Questions

1. **The corpus gate has one failure and it is in the registry's data.** One property in 19,209. The
   cause is established: the mapping file in the registry repository at the same commit carries the
   identical signature, three sibling properties of the same subject verify under the same key, and
   no alternative encoding reproduces it. Accepting the gate on that basis is a judgement, and it is
   recorded here as one. The alternative reading, that the payload construction is subtly wrong, is
   contradicted by 19,208 successes including every other property of every other subject. The
   product consequence is that one description shows unverified, which is the correct outcome for a
   record whose attestation does not verify.
2. **A verifier that accepted everything would be the failure mode to fear.** This one rejects a
   signature the registry itself accepted, which is evidence that it is checking rather than
   agreeing. Worth stating, because a run that reported 19,209 of 19,209 would have been weaker
   evidence, not stronger.
3. **Node's strictness is a property of the build, not of the API.** The PRD records that
   `@noble/curves` is the fallback if some platform's Node turns out not to reject a non-canonical
   `S`. The primitive sits behind one function, so the change would be local. Nothing is promoted
   today and `yarn.lock` carries two major versions of that package through hardware wallet
   dependencies, which is why promoting it is not free.
4. **Two subjects in 4,579 are verified without any signature satisfying a key requirement.** Carried
   from `task-008` with the full-population figure now measured. Still a decision for the project
   owner, still pinned by a test, still matching the registry's own rule.
5. No open questions beyond items 1 and 4, which are both for the project owner.

## Required Docs, Research, and Tracking Updates

- Update `task-009`'s `status` in `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`
  to `completed` when the implementation review reads `approved`.
- Append to `task-009-plan-review.md` and `task-009-impl-review.md` as the cycle requires.
- The corpus gate result is recorded in `task-009-impl-review.md`, which is the record on the branch
  the task graph asks for.
- No PRD change. The payload, the logo exception and the strictness table are all confirmed as
  written; the corpus counts are new and are recorded here rather than by editing the PRD.

## Review-Log Paths

- Planning review log: `.agent/plans/asset-metadata-cache/task-plans/task-009-plan-review.md`
- Implementation review log: `.agent/plans/asset-metadata-cache/task-plans/task-009-impl-review.md`

## Planning Status

`approved`

## Build Status

`completed`

## Current Outcome

- Plan drafted, critiqued once and revised. Approved for build execution.

## Final Outcome

- `task-009` complete. Reviewed and approved in `task-009-impl-review.md`.
- The attestation payload is confirmed against five live signatures, and the `logo` exception against
  a live 77,392-character logo: the base64-text form does not verify and the decoded-bytes form does.
- Verification uses Node's built-in `crypto.verify`, which rejects a signature whose scalar has had
  the group order added to it. Nothing was added to the dependency tree.
- The corpus gate, over the registry's whole mapping list at commit `363982b9`: 7,977 subjects asked
  and answered, 4,579 carrying a policy and 4,579 binding, 19,209 property attestations of which
  19,209 satisfy their script and 19,208 verify. The single failure is a defect in the registry's own
  data, established three ways, and the correct behaviour for it is the unverified state this code
  produces.
- The full-population figure for the item handed to the project owner: 2 subjects in 4,579 evaluate
  true against an empty key set, 0.04 percent.
- Checks, all from `nix build`: `compile` exit 0, `lint` exit 0, `i18n` exit 0, `jest` 79 suites and
  1137 tests with 1134 passed and 3 skipped. `package.json` and `yarn.lock` are unchanged.

## Self-Review

- The corpus gate was run before the plan was approved rather than after the code was written, so its
  one failure shaped the plan instead of being explained away at the end.
- The `logo` exception is confirmed against a live 77 KB signature, not taken from the PRD, and is
  then pinned in the spec as a rule that needs no live data.
- The strictness case is constructed in the spec from the real signature and asserts that the
  construction changed something, so it cannot pass vacuously.
