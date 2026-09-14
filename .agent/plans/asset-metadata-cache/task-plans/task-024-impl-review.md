Implementation: Iteration 1
Timestamp: 2026-09-15T16:05:00Z

Changes made:
- `source/renderer/app/api/assets/types.ts`, `domains/Asset.ts`,
  `utils/assets.ts` and `stores/AssetsStore.ts`: `hasImage` carried from the
  cache entry onto the merged row, the four edits `task-019` used for
  `recommendedDecimalsVerified`.
- `source/renderer/app/ipc/assetMetadataChannel.ts`: `requestAssetImageUrl`, one
  memoised promise per subject, resolving to a `data:` URL or null.
- `source/renderer/app/components/wallet/tokens/wallet-token/WalletTokenHeader.tsx`
  and `.scss`: the request in an effect, the image, and the box it is fitted in.
- `WalletTokenHeader.spec.tsx`: new, four cases.
- `assetMetadataChannel.spec.ts`: three cases.
- `AssetsStore.spec.ts`: three cases.

Files touched:
- the seven source files above and the three specs
- `.agent/plans/asset-metadata-cache/task-plans/task-024.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-024-plan-review.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-024-impl-review.md`
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`

Two details decided during implementation:

**The header spec builds its own provider tree rather than using
`createTestBed`.** One case re-renders, and `createTestBed` wraps its argument in
providers it does not export, so a re-render through it would replace the
provider tree instead of the component under it. This is the same limitation
`task-021` recorded when it dropped a re-render case; here the case is the point
of the task, so the tree is built from `TestDecorator` and the two feature
providers directly.

**`hasImage` on an unresolved row is `false` rather than left undefined.** The
cache holds no row for such a subject, so it certainly holds no logo for it, and
the value arrives with the row. Written as a comment at the site rather than
inferred from the absence of a property.

Verification run:

- `jest` over the three specs — 4 header, 16 channel, 25 store.
- **Criterion 2 is pinned by breaking the memo.** With the two lines that consult
  it removed, exactly two cases fail, "asks once for a subject however many rows
  want it" and "remembers that a subject has no logo and does not ask twice",
  and the control in the same group, "asks again for a subject it has not been
  asked about", keeps passing. So the memo cases cannot pass by never sending
  anything.
- The assertion is on the wire, not on a call count into the client: the fake
  `ipcRenderer` records what was sent, and the case asserts the subject list on
  `ASSET_IMAGE_CHANNEL-request` is exactly one long after two calls.
- The memo is module state and the suite cannot clear it without an export that
  exists only for tests, so each case that touches it uses a subject of its own.
  Stated in the spec beside the helper that mints them.
- **The flip case is pinned by breaking the dependency list.** With the effect
  keyed on `[subject]` alone, "asks as soon as the row it is drawn from says
  there is a logo" fails and the other three pass. That is the case a later tidy
  of the dependency array would otherwise silently remove.
- "No placeholder" is asserted as the absence of the element in two cases: a row
  whose `hasImage` is false, which also asserts nothing was requested at all, and
  a row whose request comes back with no logo.
- The store cases assert the flag through `getAssetTokenFromToken` as well as on
  the store, because a field the store sets and the merge helper drops passes
  every store-level assertion and reaches no component.

Checks, all six through Nix with every change staged, plus `storybook`:
- `nix build '.#checks.x86_64-linux.compile' --no-link` — built
  `g3cgvpndzvpqr88bvq126h74p4ny80dw-daedalus-compile.drv`, exit 0.
- `nix build '.#checks.x86_64-linux.lint' --no-link` — built
  `bd98j2snzp42rinysxx4di2bhghm5a6a-daedalus-lint.drv`, exit 0.
- `nix build '.#checks.x86_64-linux.stylelint' --no-link` — built
  `18x2x0fbd2pp38qi306vf1z4g5nbbsqy-daedalus-stylelint.drv`, exit 0.
- `nix build '.#checks.x86_64-linux.i18n' --no-link` — built
  `63zvz2kgakzizxiip8577b4w0lwrjk9g-daedalus-i18n.drv`, exit 0. A regression
  guard here: no message changed.
- `nix build '.#checks.x86_64-linux.jest' --no-link -L` — 89 suites passed, 1346
  tests with 1343 passed and 3 skipped, exit 0. The previous state of this branch
  was 88 suites and 1336 tests, so one suite and ten tests were added and nothing
  else moved.
- `nix build '.#checks.x86_64-linux.cucumber-unit' --no-link -L` — 36 scenarios
  and 128 steps passed, unchanged.
- `nix build '.#checks.x86_64-linux.storybook' --no-link` — exit 0.

`nix fmt` was run and changed one file before the checks.

No new `@ts-ignore` and no new `@ts-expect-error`.
`git diff HEAD -- package.json yarn.lock` produces nothing.

Deviations from the approved plan:
- None. The four `hasImage` edits and the memo's placement were both recorded in
  the plan before the work.

Outcome: Implementation complete and ready for review

Review of Iteration 1
Timestamp: 2026-09-15T16:12:00Z

Acceptance criteria, each against the evidence:

1. *A row with a logo renders it; a row without one is unchanged.* Met, and the
   negative half is asserted as the absence of the element rather than as the
   absence of a picture, which is the only form of "no placeholder" a test can
   check.

2. *At most one request per subject.* Met on the wire, with the memo removed to
   show which cases depend on it and a control that keeps passing.

3. *An `absent` answer renders nothing and is not asked again.* Met. This is the
   case that matters most in practice: a wallet holding tokens the registry has
   no picture for is the ordinary wallet, and a client that remembered only
   successes would ask on every scroll for exactly those rows.

4. *`hasImage` survives the merge.* Met through the merge helper.

5-6. *All six checks, plus storybook; suppressions; dependencies.* Met.

Two judgements worth naming.

**The bytes become a URL in the client, not in the component.** Doing it in the
render re-encodes 23 KiB per row per paint at the measured median. It also means
the component never learns that the transport carries bytes, so the surface that
renders a picture and the channel that fetches one can change independently.

**The memo has no expiry, and that is stated in the plan rather than papered
over.** One `data:` URL per held subject, each capped at 256 KiB by the store
that wrote it, is the same order of memory as the row list it belongs to. The
process holding it is the renderer, which goes away with the window. An eviction
policy here would be machinery for a bound the cache already enforces upstream.

What this does not do, so it is not mistaken for finished: only the token row
shows a logo. The send form, the transaction list and the settings dialog do not,
and nothing about the client is specific to the row, so a later surface gets the
same call and the same memo.

Summary: The image column has a reader. A row whose subject the cache holds a
logo for asks once, whatever happens to the row afterwards, and a row whose
subject has none asks once and is told so. Everything else about the row is
exactly as it was.

Decision: approved
