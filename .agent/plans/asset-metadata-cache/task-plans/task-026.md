## Task ID and Title

`task-026` — Jest coverage across the main-process modules.

## Why Chosen Now

Every module was tested by the task that built it, against the criteria that
task was written to. Nothing has yet read the PRD's Testing Strategy as a list
and checked it off against the suite, and nothing has looked at what the suite
does not reach.

## Interaction Mode

`agent_execution`.

## Scope

The Testing Strategy list in the PRD, item by item, for the items phases 1 to 6
are responsible for. Then the gaps a coverage run shows in the modules those
items are about, on both sides of the process boundary.

## Non-Goals

- **The phase 7 items on that list are not in scope and are named rather than
  skipped silently.** The metadata source URL validator,
  `getAssetMetadataSourceIdFromUrl`, pointer resolution and its three local
  checks, and the chain-row assertion all describe code that does not exist yet.
- No corpus validation. That is the gate `task-009` met, is not a unit test, and
  is not in CI.
- No Cucumber. The send-path scenarios are `task-040`'s and are complete.
- Helpers this plan never touched keep the coverage they have.
  `hasTokensLeftAfterTransaction`, `isTokenMissingInWallet`, `tokenHasBalance`
  and `filterAssets` in `utils/assets.ts` are pre-existing and unmodified.

## Dependencies

`task-010`, `task-012`.

## Research Consulted

- `asset-metadata-cache-prd.md:1448-1517`, the Testing Strategy, which is the
  checklist this task is measured against.

## Docs, Workflows, and Skills Consulted

- `perSystem/checks.nix:46-47`: `jest` and `cucumber-unit` run on every system
  except `x86_64-darwin`; the static checks run on `x86_64-linux` alone.

## Live Repo Findings Verified For Planning

Each Testing Strategy item, against the suite as it stands:

1. **CIP-14 against the eight golden vectors.** Covered.
   `utils/assetFingerprint.spec.ts:6-47` transcribes all eight from
   `TokenFingerprintSpec.hs:40-78`, including the empty asset name, a six-byte
   name and a 32-byte name, and drives each as its own case.
2. **The printable-ASCII predicate.** Covered. `utils/strings.spec.ts` has
   fourteen cases: both boundary bytes, both bytes outside them, a null byte, a
   high byte, valid UTF-8 outside ASCII, 32 random bytes, the empty name, an
   absent name, an odd-length hex string and a non-hex character.
3. **Policy and key binding, with a mismatched policy, a signature from a key
   the script does not require, and a missing `policy` field.** Covered:
   `assetVerification.spec.ts:296`, `:794` and `:311`.
4. **Attestation payload and ed25519, with a tampered value, a tampered sequence
   number and a signature valid for a different property.** Covered:
   `assetVerification.spec.ts:599`, `:610`, `:621`, plus `:631` for a different
   subject and `:580` for a malleable signature.
5. **Decimals resolution across all combinations.** Covered:
   `utils/assetDecimals.spec.ts`, nine cases.
6. **The disagreement helper with the third input.** Covered:
   `wallet-token/helpers.spec.ts:73-133`.
7. **The merge helper with a cold lookup.** Covered: `utils/assets.spec.ts:53`
   and `:62`.
8. **The database against a temporary file, `CHECK` and eviction.** Covered:
   `assetMetadataDb.realfs.spec.ts:231` and `:237` for the two source-conditioned
   rejections, and `assetImageStore.realfs.spec.ts:362` for both bounds crossed
   at once.

What a coverage run shows the suite does not reach, measured with
`jest --collectCoverageFrom` over the modules above on 2026-09-15:

9. **`httpRegistryTransport` has no coverage at all.**
   `assetRegistryClient.ts` is 75.94 percent of statements and 68.42 of
   functions, and the uncovered region is exactly `readResponse` at `:222-254`
   and `post` at `:256-310`. Every spec substitutes the transport, so the one
   piece of this design that opens a socket is the one piece nothing exercises:
   the response cap by declared length and by stream length, the timeout, the
   error paths, and the scheme choice that lets selfnode's plain-HTTP mock be
   reached at all.
10. **Seven failure paths in the database wrapper are unreached.**
    `assetMetadataDb.ts:300, 349-352, 412-415, 457-460, 497, 530-538`, one per
    accessor. The module's stated property is that nothing it does can fail
    startup, and nothing pins it. `close()` sets `_db` to null, so a closed
    wrapper returns early and never enters them; a handle that fails
    mid-operation is a different state and is the one they exist for.
11. **Both handler catches in the IPC module are unreached.**
    `assetMetadataChannel.ts:155-159` and `:182-185`. The existing "answers
    rather than rejecting when the database is gone" case closes the database,
    which the database swallows internally, so the handler's own catch never
    runs. The property those catches carry is the one the renderer's correlation
    depends on: every request gets exactly one attributable response.
12. **Three verification paths are unreached.** `assetVerification.ts:262`, the
    CBOR decode failure, which is a different path from the existing
    not-a-script case where the CBOR decodes to something that is not a script;
    `:298`, a non-string subject or property name; and `:361`, the crypto
    failure.
13. **`sortAssets` has no coverage.** `utils/assets.ts:94-157`, and
    `utils/assets.ts` overall is 55.55 percent of statements. It orders every
    token list on every surface, it takes three keys in two directions, and this
    branch changed it: `:110` and `:119` fall back to the empty string for a row
    whose fingerprint has not arrived, which is a state that could not occur
    before phase 3.
14. **Two factory functions and one `close` are unreached**, at
    `assetMetadataResolver.ts:179` and `:358` and `assetImageStore.ts:227`.
15. **Every renderer module named by the Testing Strategy already has a spec.**
    `assetFingerprint.ts`, `assetName.ts` and `assetDecimals.ts` are at 100
    percent; `strings.ts` at 95.65; `ipc/assetMetadataChannel.ts` at 97.95;
    `AssetsStore.ts` at 87.67; `assetDenominations.ts` at 97.05. The task's
    `targetPaths` name none of them, so the scope is widened and the graph
    corrected.

## Files Expected To Change

- `source/main/assets/assetRegistryClient.spec.ts`
- `source/main/assets/assetMetadataDb.realfs.spec.ts`
- `source/main/assets/assetVerification.spec.ts`
- `source/main/assets/assetMetadataResolver.realfs.spec.ts`
- `source/main/assets/assetImageStore.realfs.spec.ts`
- `source/main/ipc/assetMetadataChannel.realfs.spec.ts`
- `source/renderer/app/utils/assets.spec.ts`
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`
- the three review-log files for this task.

No source file changes. If a case cannot be written without changing the module
under test, that is a finding to record rather than a licence to change it.

## Implementation Approach

**The transport goes in the existing `assetRegistryClient.spec.ts`**, not in a
new file. The convention is one spec per module, and the `.realfs.` suffix marks
a spec that uses the real filesystem rather than a stub; a loopback socket is
neither. The group starts an `http.createServer` on `127.0.0.1:0`, so the port
is whatever the kernel gives it and nothing collides, and closes it after each
case.

**The database failure paths need a handle that is open to the wrapper and
closed to the engine.** The wrapper's own `close()` nulls its reference and every
accessor then returns early, which is the guard rather than the catch. Closing
the inner handle leaves the reference in place and makes the next `prepare`
throw, which is what a filesystem that goes away under a live process does.

**The two handler catches need a collaborator that throws**, because both the
database and the image store swallow their own failures. The spec builds the
handler with a resolver whose `request` throws and, separately, an image store
whose `fetch` rejects.

**`sortAssets` is driven as a comparator over a list**, not by calling it on
pairs, because what it is for is the order a list comes out in.

## Acceptance Criteria

1. Every negative case the PRD's Testing Strategy names for phases 1 to 6 has a
   spec, listed by name in this task's closing note, with the phase 7 items
   named as out of scope rather than omitted.
2. The renderer modules the Testing Strategy names are covered and named, and
   the task's `targetPaths` are corrected to include them.
3. No unreached failure path remains in the main-process modules, or the reason
   one remains is recorded.
4. `jest` and `cucumber-unit` pass from `nix build`, with `compile` and `lint`.
5. No source file changes.
6. No new `@ts-ignore` and no new `@ts-expect-error`; `package.json` and
   `yarn.lock` unchanged.

## Verification Plan

- A coverage run over the same file set before and after, quoted in the closing
  note, so the claim is a measurement rather than an impression.
- The transport cases assert the outcome the caller sees: `{ ok: true, status }`
  with the body, or `{ ok: false, reason }` with the reason named. Both cap paths
  are driven, because a server that declares its length and one that does not
  reach different code.
- The database cases assert every accessor answers, and assert the value it
  answers with, so a case cannot pass because the accessor threw somewhere the
  runner did not notice.
- The handler cases assert the response carries the request's own id, which is
  the part that matters: an unattributable response is worse than an empty one.
- The `sortAssets` cases assert the resulting order of a list of four, including
  one row with no fingerprint and no metadata, for each key and direction.

## Risks and Open Questions

- **A loopback server inside the Nix sandbox.** The sandbox gives a build a
  private network namespace with loopback configured, so binding `127.0.0.1:0`
  works, but this is the first spec in the repository to rely on it. Driven
  through `nix build '.#checks.x86_64-linux.jest'` rather than trusted.
- Nothing here needs a decision from the project owner.

## Required Docs, Research, and Tracking Updates

- `asset-metadata-cache-tasks.json`: `task-026.targetPaths` corrected to the
  spec files that exist and widened to the renderer; `task-026.status` to
  `completed`.

## Review-Log Paths

- `.agent/plans/asset-metadata-cache/task-plans/task-026-plan-review.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-026-impl-review.md`

## Planning Status

approved

## Build Status

completed

## Current Outcome

The Testing Strategy is checked off item by item, and the paths that answer when
something has gone wrong are driven rather than assumed.

## Final Outcome

Complete.

## Self-Review

The easy version of this task adds cases to the modules that already have the
most. The useful version asks which lines nothing reaches and why, and the answer
was consistent: every unreached line is a failure path. Those are the lines that
run on the day something breaks, and they were the ones with no test.
