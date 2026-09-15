Implementation: Iteration 1
Timestamp: 2026-09-15T18:40:00Z

Changes made, specs only:
- `source/main/assets/assetRegistryClient.spec.ts`: nine cases for the real
  transport, against a server on the loopback address.
- `source/main/assets/assetMetadataDb.realfs.spec.ts`: six cases for a handle
  that fails under the wrapper.
- `source/main/assets/assetVerification.spec.ts`: four cases for the failure
  paths.
- `source/main/assets/assetMetadataResolver.realfs.spec.ts`: five cases for a
  transport that throws, a database that throws, and the factory.
- `source/main/assets/assetImageStore.realfs.spec.ts`: one case for the factory.
- `source/main/ipc/assetMetadataChannel.realfs.spec.ts`: six cases, four for a
  metadata column the handler cannot use and two for a collaborator that throws.
- `source/renderer/app/utils/assets.spec.ts`: seven cases for `sortAssets`.

Files touched:
- the seven spec files above
- `.agent/plans/asset-metadata-cache/task-plans/task-026.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-026-plan-review.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-026-impl-review.md`
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`

`git diff HEAD --name-only` filtered for anything that is not a spec or a plan
returns nothing: no source file changed.

Three details decided during implementation:

**The transport cases live in `assetRegistryClient.spec.ts` rather than a new
file.** The convention is one spec per module, and `.realfs.` marks a spec that
uses the real filesystem; a loopback socket is neither a stub nor a file. The
server binds `127.0.0.1:0` so the kernel picks the port and nothing can collide.

**`closeAllConnections` is reached through the instance, not the declaration.**
It is Node 18.2 and newer and is not in the `@types/node` this repository pins,
so `compile` refused the direct call. Without it `close` waits for keep-alive
sockets and two cases hang. Written as a cast with the reason above it, not as a
suppression.

**Three drivers were wrong on the first attempt and are worth recording**,
because each wrong one would have passed while testing nothing:
- The CBOR-decode failure was driven with a policy field of `ff`, which returns
  `malformed` at the length guard rather than reaching the decoder. The field
  carries two bytes of array header and era tag that are stripped before hashing,
  so it has to be `8201ff`.
- The resolver's background guard was driven with a database whose `readMetadata`
  always throws. `request` reads the cache itself before queueing, so that threw
  synchronously out of `request` and never reached the queue. It now fails the
  second read, which is the queued work's.
- The database's own `close()` was the first choice for reaching its catch
  blocks. It nulls the wrapper's reference, so every accessor returns at its
  guard. The inner handle is closed instead, leaving a reference that is live to
  the wrapper and dead to the engine.

Verification run:

Coverage over the same file set before and after, `jest --collectCoverageFrom`,
2026-09-15:

| | statements | branches | functions | lines |
|---|---|---|---|---|
| main, before | 88.91 | 81.08 | 89.61 | 90.94 |
| main, after | 97.02 | 85.21 | 97.40 | 99.72 |
| `utils/assets.ts`, before | 55.55 | 28.16 | 60.00 | 58.57 |
| `utils/assets.ts`, after | 77.77 | 53.52 | 60.00 | 81.42 |

What remains unreached in the main-process modules, and why:
- `assetRegistryClient.ts:303`, the rejection handler on `readResponse`. That
  function is a `new Promise` with a single `resolve` and no `reject`, so the
  handler cannot run. Left in place and recorded rather than removed: removing a
  guard to make a number go up is the wrong trade.
- `assetMetadataDb.ts:270` and the branch-only lines the reporter names on
  `assetImageStore.ts`, `assetVerification.ts` and `assetMetadataChannel.ts` are
  declarations and default parameters, not paths.

What remains unreached in `utils/assets.ts` is `filterAssets`,
`hasTokensLeftAfterTransaction`, `isTokenMissingInWallet` and `tokenHasBalance`,
all pre-existing and untouched by this plan, and named in its Non-Goals so that
the coverage figure cannot be improved by testing unrelated code.
`AssetsStore.ts:349-410` is the router and analytics plumbing, also pre-existing.

Checks, through Nix with every change staged:
- `nix build '.#checks.x86_64-linux.compile' --no-link` — built
  `kl3wgd984jghfp7yls14srsdxqjsndji-daedalus-compile.drv`, exit 0, after the
  `closeAllConnections` failure described above.
- `nix build '.#checks.x86_64-linux.lint' --no-link` — built
  `isa8nrbf3qyhajm07djhl259nr1anpkq-daedalus-lint.drv`, exit 0.
- `nix build '.#checks.x86_64-linux.stylelint' --no-link` — built
  `v9wwc063k5r9bvka99ql7h9fi6hvgpj1-daedalus-stylelint.drv`, exit 0.
- `nix build '.#checks.x86_64-linux.i18n' --no-link` — built
  `77h09az941ivgn78lyrx31zxxyn5jz2a-daedalus-i18n.drv`, exit 0.
- `nix build '.#checks.x86_64-linux.jest' --no-link -L` — 89 suites passed, 1389
  tests with 1386 passed and 3 skipped, exit 0. The previous state of this branch
  was 89 suites and 1350 tests, so thirty-nine tests were added across seven
  existing suites and nothing else moved. **This is also the answer to the plan's
  one open risk: the loopback server binds and answers inside the Nix build
  sandbox.**
- `nix build '.#checks.x86_64-linux.cucumber-unit' --no-link -L` — 36 scenarios
  and 128 steps passed, unchanged.

`nix fmt` was run and changed four files before the checks.

No new `@ts-ignore` and no new `@ts-expect-error`.
`git diff HEAD -- package.json yarn.lock` produces nothing.

Deviations from the approved plan:
- None. The three wrong drivers are corrections made during the work, not
  departures from what the plan asked for.

Closing note: the Testing Strategy, item by item
-------------------------------------------------

Every negative case the PRD names, with the spec that holds it.

**CIP-14 fingerprints**, `utils/assetFingerprint.spec.ts`: the eight golden
vectors, each its own case; transposed arguments; an odd-length policy id; a
non-hex policy id; a non-hex asset name; an empty policy id; a policy id one byte
short and one byte long; a 32-byte asset name accepted and a 33-byte name
refused; the two arguments in the wrong order.

**The printable-ASCII predicate**, `utils/strings.spec.ts`: `0x20` and `0x7e`
accepted; `0x1f` and `0x7f` refused; a null byte; one non-printable byte among
printable ones; a high byte; bytes that are valid UTF-8 but outside ASCII; a
32-byte name of random bytes; the empty name; an absent name; an odd-length hex
string; a non-hex character.

**Policy and key binding**, `assetVerification.spec.ts`: "names both digests when
the policy belongs to a different subject"; "refuses a genuine signature from a
key the script does not require"; "reports an absent policy as absent rather than
as a mismatch"; "reports a malformed policy as malformed"; "reports a subject too
short to carry a policy id as malformed"; "reports bytes that hash correctly but
are not a script as not-a-script"; **new:** "reports bytes that are not CBOR at
all as not-a-script"; "binds but does not satisfy when the signing key is not
required"; "ignores a malformed key alongside a real one and fails on it alone".

**Attestation payload and ed25519**, `assetVerification.spec.ts`: "rejects a
tampered value"; "rejects a tampered sequence number"; "rejects a signature valid
for a different property of the same subject"; "rejects a signature valid for the
same property of a different subject"; "rejects a signature whose scalar has had
the group order added to it"; "refuses a logo value that is not a string";
"refuses a sequence number that is not an integer"; "rejects malformed inputs
without throwing"; **new:** "refuses a value CBOR cannot encode rather than
throwing"; **new:** "refuses a subject or a property name that is not a string";
**new:** "rejects a payload that is not bytes rather than throwing".

**Decimals resolution**, `utils/assetDecimals.spec.ts`: all four combinations,
plus a user setting of zero against a verified non-zero value, a verified value
of zero, a verdict with no number to format with, both spellings of an absent
setting, and a registry that published nothing but verified.

**The disagreement helper with the third input**,
`wallet-token/helpers.spec.ts`: "says nothing when the setting agrees with a
verified value"; "puts a disagreement with a verified value more strongly"; "puts
a disagreement with an unverified value more weakly"; "treats an absent verdict
as unverified"; "still says nothing when there is no published value to disagree
with".

**The merge helper with a cold lookup**, `utils/assets.spec.ts`: "keeps the
identity of a token whose subject has no cached row"; "leaves the registry fields
undefined when the subject has no cached row"; "never takes identity from the
lookup"; "derives an asset name that is not text without throwing".

**The database against a temporary file**,
`assetMetadataDb.realfs.spec.ts`: "rejects a registry row that carries a slot";
"rejects a chain row that carries a sequence number"; "rejects a subject that is
not the policy id followed by the asset name"; "rejects a verified value outside
0 and 1"; "rejects a decimals value of 21"; "rejects a negative decimals value";
"rejects a text value in the decimals column under STRICT"; "rejects a source
outside the two channels"; "rejects a state the schema does not declare";
"rejects an image for a subject with no metadata row, so the foreign key pragma
is live".

**Eviction against both bounds**, `assetImageStore.realfs.spec.ts`: "evicts the
oldest first when the entry bound is crossed"; "evicts nothing when the table is
exactly at the entry bound"; "evicts until the byte bound is satisfied and no
further"; "evicts one when the byte bound admits four of five"; "ends inside both
bounds when both are crossed"; "evicts nothing below the sweep floor, where
neither bound can be crossed".

**Not covered, and not this phase's**: the metadata source URL validator,
`getAssetMetadataSourceIdFromUrl`, pointer resolution over a recorded
`asset_info` and `tx_cbor` pair with its three local checks and their negative
cases, and the chain row asserting `decimals` is NULL and `verified` is 0. All
four describe phase 7 code that does not exist yet.

Outcome: Implementation complete and ready for review

Review of Iteration 1
Timestamp: 2026-09-15T18:48:00Z

Acceptance criteria, each against the evidence:

1. *Every negative case named for phases 1 to 6 has a spec, listed by name.* Met,
   in the closing note above, with the four phase 7 items named as out of scope
   rather than left out. Eight of the ten in-scope items were already covered,
   which is the right answer and is shown rather than assumed.

2. *The renderer modules are covered and named.* Met, and the `targetPaths` now
   say so. Three of them were already at 100 percent and one, `sortAssets`, was
   at nothing.

3. *No unreached failure path remains, or the reason is recorded.* Met. One
   remains, `assetRegistryClient.ts:303`, and the reason it cannot run is
   structural rather than a gap in the suite.

4-6. *Checks, no source changes, no suppressions, dependencies.* All met.

The finding worth naming is what the coverage run turned up. Every unreached line
in five modules was a failure path: the real transport, seven database catches,
two handler catches, three verification catches and the resolver's background
guard. Those lines exist because this design promises that a cache which cannot
answer degrades instead of failing, and until now that promise rested on reading
the code. It now rests on a suite that drives a closed handle, a throwing
collaborator, a server that never answers and a response too large to accept.

The judgement to weigh is the loopback server. It is the first spec here to want
one, and a spec that cannot run in CI is worse than no spec. It was therefore
driven through `nix build '.#checks.x86_64-linux.jest'` rather than argued from
what the sandbox is documented to provide.

`sortAssets` is the other thing worth naming. It orders every token list on
every surface and had no test at all, and this branch changed it: a row whose
fingerprint has not arrived sorts on the empty string. Seven cases now pin where
such a row lands under each key and direction, including the case that says the
quantity order is the one the user sees rather than the one the ledger holds.

Summary: The Testing Strategy is a checklist that has been checked. The suite
grew by thirty-nine cases and every one of them drives a path that runs when
something has gone wrong.

Decision: approved
