## Task ID and Title

`task-032` — The asset metadata source settings page.

## Why Chosen Now

`task-031` stores a selection, probes it and reads it back, and nothing can
change it. The setting is the reason the channel is acceptable at all: the PRD's
disclosure argument rests on the user being able to repoint the channel or leave
it, and that is only true once there is a surface.

## Interaction Mode

`agent_execution`.

## Scope

One settings category: a `Select` over the three source types, a URL input shown
for `custom` only, a description that changes with the selection, and the route
and menu entry that reach it.

## Non-Goals

- No sync gate. `StakePoolsSettings` disables its select while the node is
  syncing because SMASH feeds the stake pool list, which is unusable then.
  Nothing here is unusable during a sync: the probe tolerates an unknown local
  tip by design (`task-031`), and the chain channel is a name on a token.
- No "changes have been saved" derivation from a loading flag.
  `StakePoolsSettings.getDerivedStateFromProps` exists because its update is a
  round trip to cardano-wallet. There is no server-side write here.
- No new widget. `Select` and `InlineEditingInput` are what the SMASH page uses.
- The `direct` option is rendered and cannot be chosen. It is not hidden: the
  enum carries it from the start so the shape of the choice is visible, which is
  locked decision 11.

## Dependencies

`task-031`.

## Research Consulted

- `asset-metadata-cache-prd.md:878-891`, the surface, and its note that the
  asset copy is smaller than the 405-line SMASH component because it has no sync
  gate and no server-side setting to push.
- `asset-metadata-cache-prd.md:229`, locked decision 11.
- `asset-metadata-cache-prd.md:910-928`, the disclosure, which is what the
  descriptions have to say plainly rather than around.

## Docs, Workflows, and Skills Consulted

- `.agent/skills/i18n-messaging/SKILL.md`. Fifteen messages are added, so its
  rules on ids, descriptions and the `!!!` marker are the ones that decide
  whether the `i18n` check passes.
- `.agent/workflows/frontend.md` is **not** followed on component style: report
  01 records that it shows `useContext(IntlContext)`, which is not in the
  repository, and `composes:`, which is used zero times. The neighbouring
  settings components are the pattern instead.

## Live Repo Findings Verified For Planning

1. **Every settings category is a class component with `contextTypes`.**
   `StakePoolsSettings.tsx:151-154`, and the same in `SecuritySettings`,
   `GeneralSettings` and `DisplaySettings`. react-intl is pinned at 2.9.0, which
   predates hooks, so this is the pattern rather than a preference.
2. **`react-polymorph` supports a disabled option and ignores clicks on it.**
   `node_modules/react-polymorph/lib/components/Options.js:163` returns early
   for `option.isDisabled`, `:288` skips it in keyboard navigation, and
   `lib/skins/simple/OptionsSkin.js:81` gives it the theme's `disabledOption`
   class. So `direct` is rendered, styled as unavailable and unselectable
   without a branch in this component.
3. **`InlineEditingInput` takes the validator and the two error paths.**
   `components/widgets/forms/InlineEditingInput.tsx:42-60`: `isValid`,
   `valueErrorMessage` for the shape of the value, and `errorMessage` for one
   the caller supplies. The first is the pattern check, the second is the probe's
   refusal.
4. **The route, the menu and the router are one line each.**
   `routes-config.ts:46-55` holds eight settings routes, `Routes.tsx:136-140`
   wires `STAKE_POOLS` to its page, and
   `components/settings/menu/SettingsMenu.tsx:39-44` renders the item with a
   `className` matching its route.
5. **`SettingsMenu.scss` has no per-item class rules.** `grep` for `stakePools`
   in it returns nothing, so the `className` on `SettingsMenuItem` is a hook for
   tests and not a style. A new item needs no stylesheet change there.
6. **There is no spec for any settings category today.** Nine components, zero
   specs. The harness to copy is `tests/_utils/TestDecorator`, which supplies
   the real `en-US.json` through an `IntlProvider` and the polymorph theme, as
   `WalletTokenHeader.spec.tsx:18` uses it.
7. **`.scss.d.ts` files are tracked.** `StakePoolsSettings.scss.d.ts` is in the
   repository and `yarn compile` regenerates it, so a new stylesheet means a new
   tracked declaration file in the same commit.
8. **`isSelfnode` is read at module scope in the SMASH component**
   (`StakePoolsSettings.tsx:148`) to omit the IOHK preset there. The equivalent
   here would omit `koios` on selfnode, where `koiosUrl` is undefined. It is not
   copied: an option with no URL cannot be submitted, and `task-030` made the
   absence a type rather than a special case. Recorded as a deliberate
   difference.

## Files Expected To Change

- `source/renderer/app/components/settings/categories/AssetMetadataSettings.tsx` (new)
- `source/renderer/app/components/settings/categories/AssetMetadataSettings.scss` (new)
- `source/renderer/app/components/settings/categories/AssetMetadataSettings.scss.d.ts` (new, generated)
- `source/renderer/app/components/settings/categories/AssetMetadataSettings.spec.tsx` (new)
- `source/renderer/app/containers/settings/categories/AssetMetadataSettingsPage.tsx` (new)
- `source/renderer/app/routes-config.ts`
- `source/renderer/app/Routes.tsx`
- `source/renderer/app/components/settings/menu/SettingsMenu.tsx`
- `source/renderer/app/components/settings/menu/SettingsMenu.messages.ts`
- the four translation artifacts, regenerated
- `.agent/plans/asset-metadata-cache/asset-metadata-cache-tasks.json`
- the three review-log files for this task.

## Implementation Approach

**The component is the SMASH one with the sync gate and the saved-label
derivation removed.** What is left is a description, a `Select`, a conditional
`InlineEditingInput` and a per-type description. State is the URL being edited,
which is what makes `custom` show its input before anything has been submitted.

**Choosing a preset submits immediately; choosing `custom` does not.** The same
shape as `handleOnSelectSmashServerType`: a preset has a URL, so selecting it is
the whole action, and `custom` has nothing to submit until the user types
something. The error is reset on every type change, so a refusal from one
attempt does not sit under the next.

**Two validation layers, and they say different things.** `isValid` runs
`ASSET_METADATA_URL_VALIDATOR` and its message names the reason: not `https://`,
or a character the pattern does not admit. `errorMessage` carries whatever
`task-031`'s probe refused with, which is a fact about the instance rather than
about the string.

**The descriptions state the disclosure rather than talk around it.** The
selected index learns which of these tokens the wallet holds. That is the reason
the setting exists, and a settings page that did not say so would be the wrong
place to have put it.

**The menu item goes between Wallets and Stake Pools**, because the settings
menu is ordered from the wallet outwards and this is a wallet-data setting
rather than a staking one.

## Acceptance Criteria

1. The preset is selected when the stored URL is the default, and `custom` is
   selected when it is anything else.
2. Selecting `custom` reveals the URL input; selecting a preset hides it and
   submits the preset URL.
3. A URL the validator rejects renders the validator's message and is not
   submitted.
4. A URL the probe refused renders the refusal, and the two refusals render
   differently from each other.
5. The `direct` option is rendered, is marked unavailable, and selecting it
   submits nothing.
6. The category appears in the settings menu and `ROUTES.SETTINGS.ASSET_METADATA`
   resolves to the page.
7. `compile`, `lint`, `stylelint`, `jest`, `i18n` and `cucumber-unit` pass from
   `nix build`.
8. No new `@ts-ignore` and no new `@ts-expect-error`; `package.json` and
   `yarn.lock` unchanged.

## Verification Plan

- The component spec renders through `TestDecorator`, so every assertion is on
  text the real `en-US.json` produces. A message with a malformed id or a
  missing entry fails the case rather than rendering its own default.
- Criteria 1 and 2 are driven by rendering with the default URL and with a
  custom one, and asserting on the presence of the input rather than on internal
  state.
- Criterion 3 drives a submit with `http://` and asserts both halves: the
  message appears **and** the submit handler was not called. Asserting only the
  message would pass an implementation that rendered the error and submitted
  anyway.
- Criterion 4 renders with each of the two `ApiError` codes and asserts the two
  sentences differ, which is what the three-valued check in `task-031` was for.
- Criterion 5 asserts the option is present and that clicking it calls nothing.
- Criterion 6 is asserted by the route constant and the menu item's presence,
  and by `compile`, which is what catches a route wired to a component that does
  not exist.
- All six Nix checks. `stylelint` because a stylesheet is added, `i18n` because
  fifteen messages are.

## Risks and Open Questions

- **Fifteen messages, none of them translated.** They land in `ja-JP.json` with
  the `!!!` marker, which is what that marker records. A Japanese user sees
  English on this page until a translation round covers it, as they do for the
  eleven messages phases 1 to 5 added.
- **The copy is the disclosure.** If it is wrong or vague, the mitigation the
  PRD leans on is weaker than the PRD says. It is the part of this task worth
  reviewing hardest, and it goes through an editing pass before the commit.
- Nothing here needs a decision from the project owner.

## Required Docs, Research, and Tracking Updates

- `asset-metadata-cache-tasks.json`: `task-032.targetPaths` widened by the
  stylesheet declaration and the spec; `task-032.status` to `completed`.

## Review-Log Paths

- `.agent/plans/asset-metadata-cache/task-plans/task-032-plan-review.md`
- `.agent/plans/asset-metadata-cache/task-plans/task-032-impl-review.md`

## Planning Status

approved

## Build Status

completed

## Current Outcome

A user can see where on-chain metadata pointers are read from, and change it.

## Final Outcome

Complete.

## Self-Review

The risk in copying a 405-line component is copying the parts that exist for
reasons this one does not have. Three were found and left out: the sync gate,
the saved-label derivation from a loading flag, and the selfnode omission of the
default preset. Each is argued in the non-goals or the findings rather than
dropped silently, because a reviewer comparing the two files will ask about all
three.
