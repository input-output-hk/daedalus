# Task task-001: Resolve asset names by provenance and stop rendering non-text as text

## Task ID and Title

- ID: `task-001`
- Title: `Resolve asset names by provenance and stop rendering non-text as text`

## Why Chosen Now

`task-001` is the first task in phase 1 and has no dependencies. It is renderer-only: no cache, no
IPC channel, no network call and no new dependency. It is also the rung of the name-resolution order
that can be built before any of the other three rungs exist, because it reads bytes the wallet
already hands the renderer.

The security half is the reason it is scheduled ahead of the cache rather than alongside it. The
literal `ASCII: ` prefix at `source/renderer/app/components/assets/Asset.tsx:178` is today the only
thing that separates a name an issuer published from bytes whoever minted the asset chose. Any later
task that adds a published name to the same span inherits whatever marking exists at that point, so
the marking has to be correct before the cache starts filling that span.

## Interaction Mode

- Mode: `agent_execution`

Every acceptance criterion is checkable from the repository: three Jest specs, `yarn lint`,
`yarn compile` and `yarn test:jest`. Nothing needs a running node, a network fetch or an operator.

## Scope

- Add a printable-ASCII predicate over a hex-encoded asset name: decode, accept only if every
  decoded byte is in `0x20` to `0x7E` inclusive, reject the empty name.
- Add a name resolver that returns both the name and the provenance it came from, in the order the
  PRD fixes: registry ticker, registry name, decoded asset name when printable, nothing.
- Render a minter-chosen decoded name in a treatment visibly distinct from a published name, and
  give it an explanation the user can read without opening anything.
- Apply the same predicate to the decoded annotation on the asset-name parameter row in the pill's
  pop-over, which decodes the same bytes with the same unconditional UTF-8 decode.
- Cover the predicate boundaries, the resolution order, and the impersonation case with colocated
  Jest specs.

Revertible on its own. Reverting the commit restores the previous rendering exactly: the `ASCII: `
prefix, the `styles.ascii` colour swap and both unconditional decodes. No other phase-1 task reads
anything this task adds, and the three tasks that follow it in the phase touch
`utils/assets.ts`, `components/wallet/send-form/AssetInput.tsx` and four files this task does not
open.

## Non-Goals

- No cache, no SQLite, no IPC channel, no network request, no new runtime dependency.
- No CIP-25 or CIP-68 rung. That rung arrives with the chain channel in `task-035`; this task fixes
  the order it will slot into and leaves no placeholder behind.
- No change to `assetNameASCII` on `api/assets/types.ts:52`, to the two sites that populate it at
  `api/api.ts:3221` and `:3232`, or to the search predicate at `utils/assets.ts:269-285`. Search
  matching over raw bytes is a separate question from rendering, and narrowing it here would silently
  drop rows from search results.
- No fix for the `metadataNameChars` defect recorded under Live Repo Findings. It is real but it is
  not this task, and it changes nothing about provenance.
- No change to amount formatting, which is `task-003`'s and `task-021`'s ground.

## Dependencies

- None in the task graph. `task-001` has `"dependencies": []` in
  `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`.
- Practical dependency: a resolvable `node_modules` for `yarn lint`, `yarn compile` and
  `yarn test:jest`. The Nix dev shell is not required for any of the three.

## Research Consulted

- `.agent/plans/asset-metadata-cache/asset-metadata-cache-prd.md`, specifically the problem statement
  at `:31-38`, goal one at `:1094-1130`, the functional requirements at `:246-252`, the testing
  strategy at `:1454`, and the locked decision recorded at `:1641-1643`.
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`, the `task-001` entry.
- `.agent/plans/asset-metadata-cache/prompt.md`, outcome one.
- `.agent/plans/mithril-partial-sync/task-plans/task-001.md` as the section precedent.

## Docs, Workflows, and Skills Consulted

- Docs:
  - `.agent/plans/asset-metadata-cache/task-plans/readme.md`
  - `CLAUDE.md`, the conventions section
- Workflows:
  - `.agent/workflows/test.md` for the Jest invocation, with the caveat the trust map records: the
    pre-commit and pre-push hooks it describes do not exist.
- Skills:
  - `.agent/skills/i18n-messaging/SKILL.md` for the message schema, the `!!!` prefix rule and the
    `yarn i18n:manage` flow. Two new messages land in this task.
  - `.agent/skills/theme-management` was not loaded. The new style reuses
    `--theme-tokens-list-header-text-color`, which `Asset.scss:80` already binds, so no new theme
    variable is introduced and no theme file changes.

## Live Repo Findings Verified For Planning

Verified at `f18267927` on branch `docs/asset-metadata-cache-plan`, 2026-09-14.

**The defect, as it stands.**

- `source/renderer/app/components/assets/Asset.tsx:177-178` is
  `const name = metadata?.name || (assetName && \`ASCII: ${hexToString(assetName)}\`) || '';`. The
  registry `ticker` is not consulted at all, so the PRD's first rung is absent from the code rather
  than merely unfilled.
- `source/renderer/app/utils/strings.ts:18-19` is `Buffer.from(valueInHex, 'hex').toString()`, a
  UTF-8 decode with no validation. Measured with the repository's Node (v22.23.1):
  `Buffer.from('e29885','hex').toString()` returns `★`, and a 32-byte random asset name returns a
  string containing U+FFFD replacement characters.
- `Buffer.from('55534443zz', 'hex')` returns the 4 bytes `USDC`. Node stops at the first
  non-hex pair rather than throwing, so a malformed hex asset name silently decodes to a *prefix* of
  itself. A byte-range predicate alone would accept that prefix. The predicate therefore validates
  the hex string as well as the bytes.
- `Buffer.from('abc', 'hex')` returns 1 byte. Odd-length input truncates, same reasoning.

**Where the marking lives today.**

- `source/renderer/app/components/assets/Asset.tsx:204-214` renders the name into one `div` with
  `data-testid="assetName"`, and applies `styles.ascii` when `hasMetadataName` is false
  (`:209`).
- `source/renderer/app/components/assets/Asset.scss:79-81` is the whole of `styles.ascii`: a single
  colour swap to `--theme-tokens-list-header-text-color`. Colour alone is one channel, it is
  theme-dependent, and it is invisible in a monochrome screenshot or to a red-green colour-blind
  user. The literal `ASCII: ` prefix, not the class, is what actually carries the distinction today.
- `source/renderer/app/components/assets/Asset.tsx:199-203` renders the fingerprint in its own
  element, above the name, and is unaffected by any of this.

**Every surface that renders an asset name.** Traced by grepping for imports of
`components/assets/Asset`:

| Surface | Render site | Popover shown |
|---|---|---|
| Token list row | `components/wallet/tokens/wallet-token/WalletTokenHeader.tsx:78` | no, `hidePopOver` |
| Send form amount row | `components/wallet/send-form/AssetInput.tsx:113` | no, `hidePopOver` |
| Transaction list row | `components/wallet/transactions/Transaction.tsx:699` | yes |
| Send confirmation | `containers/wallet/dialogs/send-confirmation/DialogContentWithAssets.tsx:67` | yes |
| Hardware-wallet confirmation | `components/assets/AssetTransactionConfirmation.tsx:109` | yes |
| Asset dropdown | `components/widgets/forms/AssetsDropdown.tsx:69` | no, `hidePopOver` |

All six route the name through `Asset.tsx`'s `renderPillContent`. The three acceptance surfaces
named in the task graph, the token list, the send form and the transaction list, are the first three
rows. So one change at `Asset.tsx` reaches every one of them, and nothing else renders a resolved
name. Verified: `grep -rn "metadata?.name\|metadata.name" source/renderer --include=*.tsx` returns
only `Asset.tsx:82`, `:176` and `:178`.

- The token picker dialog reaches the same pill through
  `components/wallet/tokens/wallet-token-picker/WalletTokenPicker.tsx:158` and
  `wallet-token/WalletToken.tsx`, so it is covered by the same change.
- Three surfaces set `hidePopOver`, so the pop-over is not a reliable place to put the only
  explanation of the marking. The explanation has to be reachable from the pill itself.

**A second decode of the same bytes.**

- `source/renderer/app/components/assets/AssetContent.tsx:122-126` renders
  `(ASCII: {hexToString(value)})` under the asset-name parameter row. It is the same unconditional
  UTF-8 decode, so it shows replacement characters for exactly the assets `Asset.tsx` does. It is a
  surface that renders a decoded name and it is in scope for the same predicate. It is not in the
  task graph's `targetPaths`; the deviation is recorded under Files Expected To Change.

**Where the predicate can live.**

- `source/renderer/app/domains/Asset.ts:22-25` exposes `assetNameASCII` as a MobX `@computed` over
  `hexToString(this.assetName || '')`. The task graph suggests putting the predicate beside it.
  That placement does not work: `Asset.tsx:71` types its prop as `Asset` from
  `api/assets/types.ts:22-30`, which is a plain object type, and the values actually passed are
  `AssetToken` objects built by `utils/assets.ts:122-141` and `:74-97`, not instances of the domain
  class. A `@computed` on the domain class is unreachable from the component. The predicate goes in
  `utils/strings.ts`, which `domains/Asset.ts:4` already imports, so the decode is still written
  once.
- `api/assets/types.ts:22-30` (`Asset`) carries no `assetNameASCII`; `:49-56` (`Token`) carries it
  optionally; `AssetToken` at `:71` is their intersection. So the component cannot depend on
  `assetNameASCII` being present and must decode `assetName` itself.

**Existing test coverage.**

- `source/renderer/app/components/assets/Asset.spec.tsx:52-61` asserts the current string
  `'ASCII: Cointest'` and is the test this task rewrites. The three existing cases pass at
  `f18267927`: `yarn jest source/renderer/app/components/assets/Asset.spec.tsx --coverage=false`,
  3 passed.
- `jest.config.js:180-203` transforms SCSS with `jest-css-modules-transform`, so `styles.x` resolves
  to a real class string in jsdom and a class assertion is meaningful rather than always-truthy.
- `jest.config.js:156` is `testMatch: ['**/?(*.)+(spec|test).[tj]s?(x)']`, so a colocated
  `strings.spec.ts` and `assetName.spec.ts` are picked up with no config change.
- `jest.config.js:18` sets `collectCoverage: true` with no `coverageThreshold`, so coverage cannot
  fail the run.
- `grep -rn "assetName" ... | grep -i testid` returns only `Asset.tsx:206` and the three assertions
  in `Asset.spec.tsx`. No Cucumber feature and no other spec depends on the `assetName` test id, so
  splitting it by provenance breaks nothing.

**Build and i18n mechanics.**

- `package.json:46` makes `compile` run `precompile`, which is `typedef:sass`
  (`package.json:73`), regenerating `*.scss.d.ts`. `.gitignore:141` ignores those, so a new SCSS
  class needs no tracked declaration file but does need `yarn compile` to have run before the type
  resolves.
- `package.json:52-54`: `i18n:manage` is `i18n:extract` then `i18n:check`, and
  `translations/translation-runner.ts:3-12` runs `react-intl-translations-manager` over
  `source/renderer/app/i18n/locales` for `en-US` and `ja-JP`. New messages therefore change
  `translations/messages.json` and both locale files, and those changes must be committed for a
  re-run to be a no-op.
- `.stylelintrc` enables `order/properties-alphabetical-order`, so new SCSS declarations are written
  alphabetically.
- `source/renderer/app/i18n/locales/en-US.json:66-73` already carries the `assets.assetToken.param.*`
  family, which the two new ids extend rather than collide with.

**One defect found in passing, not fixed here.**

- `components/wallet/tokens/wallet-token/WalletTokenHeader.tsx:83` passes
  `metadataNameChars={get('name', asset.metadata, 0)}` using `lodash`'s `get`, whose signature is
  `get(object, path, default)`. The arguments are in `lodash/fp` order, so the object is the string
  `'name'` and the result is always the default `0`. `Asset.tsx:180-182` then takes the
  unellipsised branch. The prop is dead on the token list. Recording it, per `CLAUDE.md`; fixing it
  is not this task.

## Files Expected To Change

- `source/renderer/app/utils/strings.ts` — the printable-ASCII predicate and decoder.
- `source/renderer/app/utils/strings.spec.ts` — new. Predicate boundaries.
- `source/renderer/app/utils/assetName.ts` — new. The provenance resolver.
- `source/renderer/app/utils/assetName.spec.ts` — new. Resolution order.
- `source/renderer/app/components/assets/Asset.tsx` — consume the resolver, mark a minter-chosen
  name, add one message.
- `source/renderer/app/components/assets/Asset.scss` — the distinct treatment.
- `source/renderer/app/components/assets/Asset.spec.tsx` — rewritten cases, including the
  impersonation pair.
- `source/renderer/app/components/assets/AssetContent.tsx` — apply the predicate to the second
  decode, add one message.
- `source/renderer/app/components/assets/AssetContent.spec.tsx` — new. The accepted and the rejected
  decode on the asset-name parameter row.
- `translations/messages.json`, `source/renderer/app/i18n/locales/en-US.json`,
  `source/renderer/app/i18n/locales/ja-JP.json` — regenerated by `yarn i18n:manage`.
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json` — `task-001` status.
- `.agent/plans/asset-metadata-cache/task-plans/task-001*.md` — this plan and its two review logs.

Four deviations from the task graph's `targetPaths`, each recorded here rather than taken silently:

1. `utils/assetName.ts` is new rather than the resolver living inside `Asset.tsx`. The resolver is a
   pure function with four branches and an ordering that two later tasks extend; it is testable on
   its own and `AssetContent.tsx` needs the same predicate.
2. `AssetContent.tsx` is added, because it is the second surface that decodes asset-name bytes as
   text and the acceptance criterion says every surface.
3. `Asset.scss` is added, because the marking is a visual treatment and there is nowhere else for it.
4. `AssetContent.spec.tsx` is added, because deviation 2 changes rendering and an untested rendering
   change is how the `ASCII: ` prefix survived this long.

`domains/Asset.ts` is *not* changed, for the reason given under Live Repo Findings.

## Implementation Approach

1. **The predicate, in `utils/strings.ts`.**

   Add `hexToPrintableAsciiString(valueInHex: string): string | null`. It returns the decoded string
   only when the input is a well-formed even-length hex string of at least one byte and every decoded
   byte is in `0x20` to `0x7E` inclusive; otherwise `null`. The hex-shape check is not decoration:
   without it `'55534443zz'` decodes to `USDC` and passes the byte test, which is the same
   impersonation the marking exists to prevent. Decode with `latin1` so the byte-to-character mapping
   is exact rather than relying on `ascii`, which masks the high bit; over the accepted range the two
   agree, and `latin1` cannot launder a rejected byte into an accepted character.

   `hexToString` is left exactly as it is. Three callers outside this task depend on its current
   behaviour and none of them are rendering a name.

2. **The resolver, in `utils/assetName.ts`.**

   ```
   export enum AssetNameProvenance { RegistryTicker, RegistryName, MinterChosen }
   export type ResolvedAssetName = { name: string; provenance: AssetNameProvenance };
   export const resolveAssetName = (asset): ResolvedAssetName | null
   ```

   Order: `metadata.ticker`, then `metadata.name`, then `hexToPrintableAsciiString(assetName)`, then
   `null`. The CIP-25 and CIP-68 rung is documented in the module comment as sitting between the
   second and third, and no placeholder branch is written for it.

   Also export `isMinterChosen(resolved)`, so no caller re-derives the security-relevant question
   from the enum by hand.

   Input contract: the resolver takes a partial asset. `components/widgets/forms/AssetsDropdown.tsx:20`
   types its assets as `Array<Partial<AssetToken>>`, so `metadata` and `assetName` can both be
   absent at a real call site. A missing `metadata` is two absent rungs, a missing or empty
   `assetName` is a third, and the resolver returns `null` rather than throwing.

3. **The marking, in `Asset.tsx` and `Asset.scss`.**

   `renderPillContent` calls `resolveAssetName` once, keeps the existing ellipsis behaviour, and
   renders the name element with:

   - `data-testid="assetName"` for a published name and `data-testid="assetNameMinterChosen"` for a
     decoded one, so the distinction is structural and assertable rather than only visual;
   - `styles.minterChosen` alongside `styles.metadataName` for a decoded one, giving it a dashed
     outline, italics and the muted colour `styles.ascii` already used. Three independent visual
     channels, so the distinction survives a monochrome rendering and colour-blindness. The dashed
     outline is the load-bearing one: a published name is plain text, a minter-chosen name sits in an
     outlined chip;
   - a `title` attribute carrying one new message explaining that the name was decoded from bytes
     chosen by whoever minted the token and was not published in the registry. A `title` rather than
     a nested `PopOver`, because three of the six surfaces pass `hidePopOver` and because nesting a
     tippy instance inside the pill that is itself a tippy child is avoidable risk for no gain.

   `styles.ascii` is deleted rather than left in place, since nothing would reach it.

   The `ASCII: ` prefix is gone. The distinction is not.

4. **The second decode, in `AssetContent.tsx`.**

   The asset-name parameter row keeps showing the raw hex as its copyable value. Its decoded
   annotation renders only when the predicate accepts, and is worded as a minter-chosen name rather
   than as `ASCII:`, using one new message with a `{name}` placeholder. When the predicate rejects,
   the annotation is omitted entirely and the row shows the hex alone, which is the honest rendering
   of bytes that are not text.

5. **Messages.** Two, both following `namespace.context.messageKey`, both with `description`, both
   with `!!!`-prefixed `defaultMessage`:

   - `assets.assetToken.minterChosenName` in `Asset.tsx`, the pill's `title`.
   - `assets.assetToken.param.assetNameMinterChosen` in `AssetContent.tsx`, the pop-over annotation,
     carrying a `{name}` placeholder.

   Neither collides with the seven `assets.assetToken.*` ids already at
   `source/renderer/app/i18n/locales/en-US.json:66-73`, and neither is defined in both files, which
   is the pattern the existing shared ids follow. Run `yarn i18n:manage` and commit the regenerated
   `translations/messages.json` and both locale files, so a re-run is a no-op. The `ja-JP` entries
   land as `!!!`-prefixed placeholders, which the skill records as the expected state for a new key
   and which is called out in the handoff rather than left implicit.

6. **Specs.** Three files, as listed above. The impersonation pair is a component test, not a unit
   test, because the criterion is about what renders.

## Acceptance Criteria

Carried from the task graph, with the check that settles each.

1. **A minter-chosen decoded name renders in a treatment visually distinct from a registry or CIP-25
   name, on the token list, the send form and the transaction list.** Settled by the surface table
   under Live Repo Findings: all three route through `Asset.tsx:204-214`, and by an assertion that
   the decoded name carries `styles.minterChosen` and the published name does not.
2. **An asset whose name bytes spell an existing registry ticker is distinguishable from the real one
   without reading the fingerprint, demonstrated with a fixture for both.** Settled by a spec that
   renders two assets whose visible name text is identically `USDC`, one from `metadata.ticker` and
   one decoded from `0x55534443`, and asserts they land under different test ids, carry different
   classes, and that only the decoded one carries the explanatory `title`.
3. **No token row in the list or the send form shows replacement characters.** Settled by a spec
   asserting that a 32-byte random asset name renders no name element at all, and by the predicate
   rejecting every byte above `0x7E`, which is a superset of the bytes that produce U+FFFD.
4. **The predicate rejects the empty name and any byte outside `0x20` to `0x7E`, asserted per
   boundary.** Settled by `strings.spec.ts` with a case each for `0x1F`, `0x20`, `0x7E`, `0x7F`,
   `0x00`, a high byte, valid non-ASCII UTF-8, the empty string, odd-length hex and non-hex
   characters.
5. **`yarn lint`, `yarn compile` and `yarn test:jest` pass.** Run, with output recorded in the
   implementation review log. `yarn compile` did not pass and does not pass at the base commit
   either; the four errors are in files this task does not open and the review log records the
   stashed re-run that proves it.

Two criteria this task adds to its own closure:

6. `yarn i18n:manage` leaves the tracked artifacts unchanged when run a second time.
7. No new `@ts-ignore`.

## Verification Plan

Repository verification already done for planning is listed under Live Repo Findings. Execution
verification:

- `yarn jest source/renderer/app/utils/strings.spec.ts --coverage=false` — predicate boundaries.
  Negative cases driven explicitly, not implied: `0x1F` immediately below the floor, `0x7F`
  immediately above the ceiling, `0x00`, a high byte, `c3a9` which is valid UTF-8 and not ASCII, the
  empty string, `'abc'` for odd length, and `'55534443zz'` for the truncating-decode case. Each
  asserts `null`, not merely a falsy value.
- `yarn jest source/renderer/app/utils/assetName.spec.ts --coverage=false` — ticker beats name, name
  beats decoded, decoded appears only when printable, and the result is `null` when nothing resolves.
- `yarn jest source/renderer/app/components/assets/Asset.spec.tsx --coverage=false` — the four test
  cases named in the task graph plus the impersonation pair, plus an assertion that the string
  `ASCII:` appears nowhere in the rendered output, plus the resolver's partial-asset case driven
  through the component with neither `metadata` nor `assetName`.
- The same spec drives acceptance criterion one under each of the three prop shapes the named
  surfaces actually use, rather than inferring them from the render-site table: `hidePopOver small`
  as `AssetInput.tsx:113` passes them, `small={false}` with `metadataNameChars` as
  `WalletTokenHeader.tsx:78-89` passes them, and the bare `asset` prop as `Transaction.tsx:699`
  passes it. The marking must be present in all three.
- `yarn jest source/renderer/app/components/assets/AssetContent.spec.tsx --coverage=false` — the
  pop-over annotation appears for a printable asset name and is absent for a non-printable one,
  with the raw hex still rendered in both.
- `yarn lint`
- `yarn compile`
- `yarn test:jest` in full, to catch a spec elsewhere that depended on the old string.
- `yarn i18n:manage`, then `git status --porcelain` twice: once to collect the regenerated artifacts
  for the commit, once after committing to confirm a clean tree.
- `yarn stylelint` for the new SCSS, since the alphabetical-order rule is easy to trip and cheap to
  check.
- `grep -rn "ASCII" source/renderer/app/components/assets/` to confirm the literal is gone from both
  components.

If any of these cannot run because `node_modules` is unavailable, the review log says so plainly and
the task does not close.

## Risks and Open Questions

1. **Promoting `ticker` to the first rung changes what registered tokens display today.** The PRD
   says the first three rungs are "simply absent" until the cache lands
   (`asset-metadata-cache-prd.md:1096-1097`), but that is not true of this tree: `metadata` is
   populated now by the existing poll, so a token with a registry ticker will start showing the
   ticker where it showed the name. `utils/formatters.ts:93-96` already appends the ticker to the
   formatted amount, so such a row will read `HOSKY … 1,234 HOSKY`. This is the order the PRD and the
   task graph both specify and it is implemented as specified; the duplication is named here so the
   owner can decide whether the amount should drop its ticker suffix once the pill carries it. That
   is a question for `task-016`, not a reason to deviate here.
2. **A character-level marking cannot be imitated away, but a typographic one can be missed.** A
   minter can choose any printable ASCII, so any marking made of characters inside the name span is
   forgeable. The marking is therefore outside the name text: a container style and a test id, not a
   prefix string. The residual risk is a user who does not notice an outline. Its real cost is
   bounded: the fingerprint is still rendered beside the name at `Asset.tsx:199-203` and remains the
   identity. One marking, stated once, is the proportionate control; stacking a second warning icon
   onto the same pill, which already carries one for decimals, would make both easier to ignore.
3. **The `title` attribute is a native tooltip, so it has a browser-controlled delay and does not
   appear on touch.** Daedalus is a desktop Electron application with no touch surface, and the pill
   pop-over already relies on hover. Acceptable.
4. **Deleting `styles.ascii` is a visible change for anyone who themed against it.** It is a CSS
   module class, local to `Asset.scss`, with no other reader: `grep -rn "styles.ascii" source`
   returns one line. No risk.
5. **The `ja-JP` entries ship as `!!!` placeholders.** Per
   `.agent/skills/i18n-messaging/SKILL.md`, that is the expected state for a new key and the
   follow-up translation work is named explicitly in the handoff rather than left silent.
6. **Open question for the owner, not blocking:** whether the decoded-name treatment should also
   suppress the name in the `sortAssets` `'token'` comparison at `utils/assets.ts:176-193`, which
   sorts on `metadata.name` only and therefore already sorts every minter-chosen name by fingerprint.
   Current behaviour is already the conservative one, so nothing changes here.

## Required Docs, Research, and Tracking Updates

- Update `task-001`'s `status` in `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`
  to `completed` when the implementation review reads `approved`.
- Append to `task-001-plan-review.md` and `task-001-impl-review.md` as the cycle requires.
- No PRD change. The PRD's goal-one section already describes exactly what is built, and the one
  place where it is optimistic about the first three rungs being absent is recorded under Risks
  rather than edited, since the PRD is under review on a separate branch.
- No `.agent/` documentation correction is needed for this task. The trust-map divergences that touch
  it, the Electron version and the `yarn manage:translations` name in the pull-request template, are
  already recorded in `CLAUDE.md` and belong to `task-028` and to the contributor-documentation
  branch.

## Review-Log Paths

- Planning review log: `.agent/plans/asset-metadata-cache/task-plans/task-001-plan-review.md`
- Implementation review log: `.agent/plans/asset-metadata-cache/task-plans/task-001-impl-review.md`

## Planning Status

`approved`

## Build Status

`completed`

## Current Outcome

- Planning and implementation complete, reviewed and approved in
  `task-001-impl-review.md`.
- The `ASCII: ` prefix is gone from both places that decoded asset-name bytes as text. The
  distinction it carried is not: a minter-chosen name now renders in a dashed outline, in italics
  and in a muted colour, under its own test id, with a tooltip saying where the name came from and
  that the fingerprint, not the name, identifies the token.
- A name resolves in the order the PRD fixes: registry ticker, registry name, printable decoded
  asset name, nothing. The chain rung is documented where it will slot in and left unwritten.
- The predicate rejects the empty name, every byte outside `0x20` to `0x7E`, and a malformed hex
  string. The last of those is beyond the task graph's wording and is there because
  `Buffer.from('55534443zz', 'hex')` returns the four bytes `USDC` instead of throwing.

## Final Outcome

- `task-001` complete. All five acceptance criteria from the task graph are met except the
  `yarn compile` clause, which fails on four errors in `utils/crypto.ts` and
  `utils/dataSerialization.ts` that fail identically at `f18267927` with this change stashed. The
  change introduces no type error and cannot clear those four.
- Checks: `yarn test:jest` 73 suites and 962 tests passed with 3 skipped; `yarn lint` exit 0;
  `yarn stylelint` exit 0; `yarn prettier:check` exit 0; `yarn i18n:manage` idempotent on a second
  run.
- Carried forward for the tasks that fill the first three rungs: the pill and the formatted amount
  will both carry the ticker once the cache lands, because `utils/formatters.ts:93-96` already
  appends it to the amount. Naming the duplication is `task-016`'s to resolve.
- Carried forward for `task-035`: `utils/assetName.ts` is where the CIP-25 and CIP-68 rung goes, and
  anything it adds must return a provenance that `isMinterChosenAssetName` reports as published.

## Self-Review

- The plan implements the ordering the PRD fixes, including the rung that changes present-day
  behaviour, and names that consequence rather than quietly implementing a narrower order.
- The marking is one mechanism with three visual channels plus a structural test id, not a stack of
  independent warnings. The residual risk is named once with its cost.
- The predicate validates the hex shape as well as the byte range. That is beyond the letter of the
  task graph's wording and is justified by a measured Node behaviour that would otherwise defeat the
  whole point of the task.
- Scope held: no cache, no IPC, no network, no dependency, no change to search, to amounts, or to
  `assetNameASCII`.
