# Storybook coverage: current state

Measured against `3eaa4f17fac50a959057143904d488303fe4922b` on branch `feat/drep-discovery`,
2026-09-10.

Every count is followed by the command that produced it. Where a figure already in circulation
turned out to be wrong, the corrected value and the predicate behind it are given.

## 1. Summary of the migration surface

Every count in this note is measured over the 84 story files, not over every file under
`storybook/stories`. The support modules those stories import carry knob calls of their own, so the
whole-corpus knob figures are higher and are given alongside the story-file ones below.

| Measure | Value |
|---|--:|
| Story files (`*.stories.ts(x)` and `*.story.tsx`) | 84 |
| Story files under `storybook/stories/` | 80 |
| Story files living in `source/` | 4 |
| Files calling `storiesOf()` | 69 |
| `storiesOf()` calls | 73 |
| Distinct sidebar panel titles | 53 |
| Top-level sidebar groups | 15 |
| Story registrations (`.add()`) | 272 |
| Files using Component Story Format (`export default {}`) | 0 |
| Total lines in story files | 11,105 |
| `@ts-ignore` inside story files | 229 across 48 files |

```
$ find . -path ./node_modules -prune -o -type f -name '*.stories.*' -print -o -type f -name '*.story.*' -print | wc -l
84
$ grep -rn "storiesOf(" --include=*.stories.tsx --include=*.stories.ts --include=*.story.tsx storybook source | wc -l
73
$ grep -rn "\.add(" --include=*.stories.tsx --include=*.stories.ts --include=*.story.tsx storybook source | grep -v "moment()" | wc -l
272
```

The figure of 82 story files that has been quoted elsewhere is low by two. The correct predicate
matches both naming conventions: 80 files sit under `storybook/stories/`, and four more live beside
the components they exercise:

- `source/renderer/app/components/profile/analytics/Analytics.stories.tsx`
- `source/renderer/app/components/wallet/tokens/wallet-token-picker/WalletTokenPicker.stories.tsx`
- `source/renderer/app/features/discreet-mode/ui/DiscreetValue.story.tsx`
- `source/renderer/app/features/discreet-mode/ui/discreet-toggle/DiscreetModeToggle.story.tsx`

All four use `storiesOf()` as well.

The raw `.add(` grep returns 275; three of those are `moment().add(35, 'hour')` in
`storybook/stories/staking/DelegationSteps.stories.tsx:211`,
`storybook/stories/staking/Epochs.stories.tsx:10` and
`storybook/stories/staking/Undelegate.stories.tsx:108`. The real registration count is 272, of
which 270 are reachable. The other two are in `storybook/stories/staking/Legacy.stories.tsx`,
which no barrel imports.

### Stories are enumerated by hand, not by glob

`storybook/main.ts:8` declares a single entry:

```js
stories: ['../storybook/stories/index.ts'],
```

Nothing is auto-discovered. `storybook/stories/index.ts` imports seven barrels and a list of
individual story modules; those barrels import further story modules; some story modules import
their siblings for the story functions they export. A new story file is invisible until someone
adds it to a barrel, and a file dropped from a barrel disappears silently rather than failing a
build. `storybook/stories/staking/Legacy.stories.tsx` is dead for exactly this reason.

## 2. Container and screen coverage

### Only two containers have a story

Traversing every story file's import graph, continuing through `storybook/` support modules and
stopping at the `source/` boundary, exactly two modules under `source/renderer/app/containers/`
are reached:

| Container | Reached from |
|---|---|
| `source/renderer/app/containers/static/AboutDialog.tsx` | `storybook/stories/nodes/about/About.stories.tsx:5` |
| `source/renderer/app/containers/wallet/dialogs/send-confirmation/SendConfirmation.view.tsx` | `storybook/stories/wallets/send/WalletSend.stories.tsx` |

There are 110 `.tsx` files under `source/renderer/app/containers`, 105 of them excluding the five
`*.spec.tsx`. So 103 of the 105 are never imported by a story.

```
$ find source/renderer/app/containers -name '*.tsx' | wc -l
110
$ find source/renderer/app/containers -name '*.tsx' ! -name '*.spec.tsx' | wc -l
105
```

`About.stories.tsx` is the only story that mounts a real container. It does so by hand-assembling a
mock store object in `storybook/stories/nodes/_utils/props.ts` and spreading it as props
(`About.stories.tsx:12-14`). Every other story renders presentational components with literal props.

The suite is a component catalog, not a screen catalog. The container layer, where MobX
injection, store wiring and route parameters live, has no Storybook coverage beyond those two
files, and a rewrite that ports story-for-story leaves that unchanged.

### What the stories do cover

Measured at the component layer, 154 of 359 non-test component files are reachable from at least
one story:

| Component directory | `.tsx` files | with a story | without |
|---|--:|--:|--:|
| `components/wallet/` | 114 | 54 | 60 |
| `components/staking/` | 63 | 25 | 38 |
| `components/widgets/` | 48 | 18 | 30 |
| `components/governance/` | 28 | 5 | 23 |
| `components/voting/` | 23 | 10 | 13 |
| `components/loading/` | 18 | 8 | 10 |
| `components/settings/` | 10 | 8 | 2 |
| `components/sidebar/` | 8 | 3 | 5 |
| `components/profile/` | 6 | 3 | 3 |
| `components/notifications/` | 6 | 3 | 3 |
| `components/assets/` | 6 | 2 | 4 |
| `components/layout/` | 5 | 3 | 2 |
| `components/news/` | 5 | 3 | 2 |
| `components/status/` | 5 | 3 | 2 |
| `components/navigation/` | 3 | 1 | 2 |
| `components/analytics/` | 3 | 0 | 3 |
| `components/knownIssues/` | 2 | 1 | 1 |
| `components/splash/` | 1 | 1 | 0 |
| `components/dapp/` | 1 | 1 | 0 |
| `components/appUpdate/` | 1 | 1 | 0 |
| `components/chain-storage/` | 1 | 1 | 0 |
| `components/hardware-wallet/` | 1 | 0 | 1 |
| `components/static/` | 1 | 0 | 1 |
| **total** | **359** | **154** | **205** |

The `governance/` row has four sidebar panels and 62 story registrations against 5 of its 28
components reached directly. The governance stories render a small number of large composites
(`DRepDirectory`, `DRepDetail`, `Delegation`, `GovernanceWallets`) and drive their internals through
props and fixtures instead of mounting the leaves individually.

## 3. Reachable screens, enumerated from the router

The route table is `source/renderer/app/routes-config.ts` (56 lines, `ROUTES`) and the router is
`source/renderer/app/Routes.tsx`. Screens outside the router are mounted by
`source/renderer/app/containers/Root.tsx` and `source/renderer/app/App.tsx`.

Coverage below is "does any story render the component this container renders", one hop from the
container. It is deliberately generous: it credits a screen whose parts are storied even when the
assembled screen is not.

| Route | Container | Story coverage of its rendered components |
|---|---|---|
| `/` | redirect to `/wallets` | n/a |
| `/profile/initial-settings` | `InitialSettingsPage` | partial (`TopBarLayout` missing) |
| `/profile/terms-of-service` | `TermsOfUsePage` | partial (`TermsOfUseForm`, `TopBarLayout` missing) |
| `/profile/analytics` | `AnalyticsConsentPage` | partial (`TopBarLayout` missing) |
| `/profile/data-layer-migration` | `DataLayerMigrationPage` | partial (`CenteredLayout` missing) |
| `/wallets/add` | `WalletAddPage` | partial (`WalletBackupDialog` missing) |
| `/wallets/:id` layout | `Wallet` | partial (`LoadingSpinner`, `RestoreNotification` missing) |
| `/wallets/:id/summary` | `WalletSummaryPage` | partial (`WalletTransactionsList`, `WalletNoTransactions` missing) |
| `/wallets/:id/send` | `WalletSendPage` | covered |
| `/wallets/:id/receive` | `WalletReceivePage` | covered |
| `/wallets/:id/tokens` | `WalletTokensPage` | covered |
| `/wallets/:id/transactions` | `WalletTransactionsPage` | covered |
| `/wallets/:id/settings` | `WalletSettingsPage` | covered |
| `/wallets/:id/utxo` | `WalletUtxoPage` | covered |
| `/settings` layout | `Settings` | covered via `storybook/stories/settings/utils/SettingsWrapper.tsx:7-8` |
| `/settings/general` | `GeneralSettingsPage` | **none** |
| `/settings/wallets` | `WalletsSettingsPage` | covered |
| `/settings/stake-pools` | `StakePoolsSettingsPage` | covered |
| `/settings/terms-of-service` | `TermsOfUseSettingsPage` | covered |
| `/settings/support` | `SupportSettingsPage` | covered |
| `/settings/display` | `DisplaySettingsPage` | covered |
| `/settings/security` | `SecuritySettingsPage` | covered |
| `/paper-wallet/create-certificate` | `PaperWalletCreateCertificatePage` | partial (`ConfirmationDialog` missing); no entry point, `SidebarStore.ts:123` |
| `/staking` layout | `Staking` | partial (`StakingUnavailable`, `DelegationSetupWizardDialog` missing) |
| `/staking/countdown` | `StakingCountdownPage` | covered; no entry point, `SidebarStore.ts:124` |
| `/staking/delegation-center` | `DelegationCenterPage` | partial (`DelegationSetupWizardDialog` missing) |
| `/staking/stake-pools` | `StakePoolsListPage` | partial (`StakePoolsRankingLoader`, `DelegationSetupWizardDialog` missing) |
| `/staking/rewards` | `StakingRewardsPage` | covered |
| `/staking/epochs` | `StakingEpochsPage` | covered |
| `/staking/info` | `StakingInfoPage` | covered; route disabled, `stakingConfig.ts:104` |
| `/redeem-itn-wallets` | `RedeemItnRewardsContainer` | **none** (`LoadingOverlay` has no story; the five dialogs it delegates to do) |
| `/voting` layout | `Voting` | covered |
| `/voting/registration` | `VotingRegistrationPage` | partial (`VotingNoWallets`, `VotingRegistrationDialog` missing) |
| `/governance` layout | `Governance` | covered |
| `/governance` index | `GovernanceRootRedirect` | n/a, renders no component |
| `/governance/dashboard` | `GovernanceWalletsPage` | covered |
| `/governance/delegate` | `VotingGovernancePage` | covered |
| `/governance/dreps` | `DRepDirectoryPage` | covered |
| `/governance/favorites` | `DRepDirectoryPage` | covered |
| `/governance/dreps/:drepId` | `DRepDetailPage` | covered |

Screens mounted outside the router:

| Mount point | Container | Story coverage |
|---|---|---|
| `Root.tsx:65` | `SplashNetworkPage` | covered (`SplashNetworkFlight`); flight builds only |
| `Root.tsx:92` | `LoadingPage` | **none** for the page itself; its three branches are covered |
| `LoadingPage.tsx:92` | `SyncingConnectingPage` | covered |
| `LoadingPage.tsx:21` | `NoDiskSpaceErrorPage` | covered |
| `LoadingPage.tsx:22` | `SystemTimeErrorPage` | covered |
| `LoadingPage.tsx:85` | `MithrilSyncContainer` | covered |
| `LoadingPage.tsx:60` | `ChainStorageContainer` | covered |
| `Root.tsx:73` | `AppUpdateContainer` | covered |
| `App.tsx:83` | `AboutDialog` | covered, and is one of the two container-level stories |
| `App.tsx:86` | `DaedalusDiagnosticsDialog` | covered |
| `App.tsx:90` | `ToggleRTSFlagsDialogContainer` | **none** |
| `App.tsx:93` | `RTSFlagsRecommendationOverlayContainer` | covered |
| `App.tsx:94` | `NotificationsContainer` | covered |
| `App.tsx:96` | `NewsFeedContainer` | covered |
| `App.tsx:97` | `NewsOverlayContainer` | covered |
| `MainLayout.tsx:116` | `TransferFundsPage` | renders containers only; the two dialogs it reaches have stories |
| `MainLayout.tsx:117` | `AssetSettingsDialogContainer` | covered |
| `MainLayout.tsx:109` | `TopBarContainer` | covered |

### Reachable screens with no story at all

Three.

**General Settings** (`/settings/general`). `GeneralSettingsPage.tsx:3` renders `GeneralSettings`,
which has no story. `storybook/stories/settings/general/General.stories.tsx:10` reaches past it and
renders `ProfileSettingsForm` directly, which is what `GeneralSettings.tsx:3` renders. The form is
covered; the screen composite that supplies its props is not.

**Redeem ITN Rewards** (`/redeem-itn-wallets`). `RedeemItnRewardsContainer.tsx:8` renders
`LoadingOverlay`, which has no story. The six dialogs the container delegates to are all storied in
`storybook/stories/staking/RedeemItnWallets.stories.tsx`, but the container's own loading state is
not.

**Toggle RTS Flags dialog** (`App.tsx:90`). `ToggleRTSFlagsDialog` has no story anywhere. Its
sibling `RTSFlagsRecommendationOverlay` does, in
`storybook/stories/news/AlertsOverlay.stories.tsx`. This is the only screen in the application with
no story coverage of any kind.

Beyond those, the recurring pattern is that layout and empty-state components go unstoried:
`TopBarLayout`, `CenteredLayout`, `WalletNoTransactions`, `VotingNoWallets`, `StakingUnavailable`,
`RestoreNotification`, `StakePoolsRankingLoader`, `LoadingSpinner`. `DelegationSetupWizardDialog`
is missing from three separate containers, though its constituent step dialogs are storied in
`storybook/stories/staking/DelegationSteps.stories.tsx`.

## 4. Dead stories

### Orphaned: `storybook/stories/staking/Legacy.stories.tsx`

The only story file unreachable from `storybook/stories/index.ts`. Nothing imports it, so its two
registrations (`StakingChart` / "Tooltip only" and "Chart with Tooltips") never appear in the
sidebar. `tsc` compiles it, because the type checker takes every file in the repository, but the
Storybook build never loads it.

The components it imports are dead too:

```
$ grep -rn "staking/legacy" --include=*.ts --include=*.tsx . --exclude-dir=node_modules
storybook/stories/staking/Legacy.stories.tsx:5:import StakingChart from '.../components/staking/legacy/StakingChart';
storybook/stories/staking/Legacy.stories.tsx:6:import StakingChartTooltip from '.../components/staking/legacy/StakingChartTooltip';
```

`source/renderer/app/components/staking/legacy/` holds eight components with their stylesheets, and
this orphaned story file is the only thing in the repository that references any of them. The last
two commits to touch it were the TypeScript conversion sweep.

### Stories for features disabled by a flag

Four cases. None of these are broken stories; each renders a component that a user cannot reach.

**Paper wallet certificate creation.** The route exists at `Routes.tsx:163-166` and the flow is
mounted at `MainLayout.tsx:111`. The only navigation affordance is the sidebar category, and
`source/renderer/app/stores/SidebarStore.ts:123` sets it false unconditionally:

```js
[categories.PAPER_WALLET_CREATE_CERTIFICATE.name]: false,
```

`MainLayout.tsx:26-29` opens the first dialog on that sidebar click. The only other opener,
`PaperWalletCreateCertificatePage.tsx:142-145`, is the flow's own back-navigation and cannot start
it. `storybook/stories/wallets/paperWallets/PaperWallets.stories.tsx` registers five stories for
the five dialogs of a flow with no entry point.

**Staking info page.** `source/renderer/app/config/stakingConfig.ts:104` sets
`IS_STAKING_INFO_PAGE_AVAILABLE = false`, which gates both the route (`Routes.tsx:201`) and the
navigation tab (`containers/staking/Staking.tsx:115`).
`storybook/stories/staking/Staking.stories.tsx:176` and `:193` register "Info" and "Info Countdown"
for it, and the story's decorator passes `showInfoTab` unconditionally at
`Staking.stories.tsx:79`, so the tab is visible in Storybook and nowhere else.

**Staking countdown.** `SidebarStore.ts:124` sets `STAKING_DELEGATION_COUNTDOWN` false. Two panels,
`Decentralization / Countdown` in both `Staking.stories.tsx:92` and `CountdownParty.stories.tsx:12`,
document a screen with no route into it.

**Legacy wallet notification.** `source/renderer/app/config/walletsConfig.ts:44` sets
`IS_BYRON_WALLET_MIGRATION_ENABLED = false`, and `TopBar.tsx:95-98` gates `LegacyNotification`
behind it. `storybook/stories/wallets/legacyWallets/LegacyNotification.stories.tsx` documents it.
The same flag is the only affordance that reaches `TransferFundsPage`: `onTransferFunds` originates
at `TopBarContainer.tsx:72`, is passed to `TopBar.tsx:101`, and is consumed only by
`LegacyNotification.tsx:142`, so the transfer-funds flow storied in `TransferFunds.stories.tsx` is
also unreachable in the running application.

One case is partial.
`storybook/stories/wallets/settings/WalletSettingsScreen.stories.tsx:326` renders
`UndelegateWalletConfirmationDialog` inside the wallet settings screen, but
`WalletSettings.tsx:209` returns null for that box because
`walletsConfig.ts:45` sets `IS_WALLET_UNDELEGATION_ENABLED = false`. The dialog itself remains
reachable through the delegation center (`DelegationCenterPage.tsx:118-119`), so the component is
live and only this story's framing is stale.

### Stories referencing symbols that no longer exist

None. Every relative import in all 84 story files and all 44 non-story `storybook/` modules resolves
to a file on disk, and every named import resolves to an export in its target. `yarn compile`
reports zero errors under `storybook/`.

Stories are type-checked, which is what has kept these imports current.

### Orphaned support modules

Three files under `storybook/` are referenced by nothing:

- `storybook/stories/staking/StakingWrapper.tsx`
- `storybook/stories/wallets/_utils/defaultWalletProps.tsx`
- `storybook/stories/wallets/_utils/HardwareWalletWithNavigationLayout.tsx`

The `defaultWalletProps` symbol found by a naive grep is a local constant in
`source/renderer/app/components/wallet/tokens/wallet-token/WalletToken.spec.tsx:15`, unrelated to
the file.

### Unused dependency

`storybook-addon-swc@1.1.7` is declared at `package.json:175` and referenced nowhere else in the
repository. `storybook/main.ts:69-96` configures `swc-loader` by hand instead. It can be dropped
without replacement.

## 5. Per-file inventory

Columns: `sOf` is `storiesOf()` calls, `.add` is story registrations, `.addDec` is `.addDecorator()`
calls, `wState` is `@dump247/storybook-state`, `act` is `@storybook/addon-actions`, `SDec` and
`SProv` are direct imports of `StoryDecorator` and `StoryProvider`, `ts-ig` is `@ts-ignore` count.
Knob abbreviations: `wK` `withKnobs`, `bool` `boolean`, `num` `number`, `sel` `select`, `rad`
`radios`, `btn` `button`, `obj` `object`, `opts` `optionsKnob`.

| File | sOf | .add | .addDec | knob types | wState | act | SDec | SProv | ts-ig | LoC | exercises |
|---|--:|--:|--:|---|:-:|:-:|:-:|:-:|--:|--:|---|
| `src:components/profile/analytics/Analytics.stories.tsx` | 1 | 1 | 2 | wK |  |  | y |  | 0 | 11 | AnalyticsConsentForm |
| `src:components/wallet/tokens/wallet-token-picker/WalletTokenPicker.stories.tsx` | 1 | 1 | 2 | wK |  |  | y | y | 0 | 77 | WalletTokenPicker |
| `src:features/discreet-mode/ui/DiscreetValue.story.tsx` | 1 | 2 | 2 | bool wK |  |  | y | y | 2 | 45 | DiscreetValue |
| `src:features/discreet-mode/ui/discreet-toggle/DiscreetModeToggle.story.tsx` | 1 | 1 | 1 | wK |  | y |  |  | 0 | 33 | DiscreetModeToggle |
| `assets/Asset.stories.tsx` | 1 | 1 | 2 | bool num text wK |  | y | y |  | 7 | 63 | Asset |
| `assets/AssetSettingsDialog.stories.tsx` | 1 | 3 | 2 | num wK |  | y | y | y | 3 | 62 | AssetSettingsDialog |
| `common/ItemsDropdown.stories.tsx` | 1 | 4 | 2 | bool num sel text wK | y | y | y | y | 14 | 267 | AssetsDropdown, ItemsDropdown, WalletsDropdown +1 |
| `common/Widgets.stories.tsx` | 1 | 10 | 2 | bool date num text wK |  | y | y | y | 6 | 257 | VerticalSeparator, BigButtonForDialogs, ButtonLink +6 |
| `dapps/TransactionRequest.stories.tsx` | 1 | 2 | 2 | bool num sel wK | y | y | y | y | 1 | 214 | DappTransactionRequest, Notification |
| `governance/DRepDetail.stories.tsx` | 1 | 15 | 2 | num sel wK |  | y | y | y | 0 | 408 | DRepDetail |
| `governance/DRepDirectory.stories.tsx` | 1 | 20 | 2 | num sel wK | y | y | y | y | 0 | 731 | DRepDirectory, DRepDirectoryList +8 |
| `governance/Delegation.stories.tsx` | 1 | 24 | 2 | bool num sel text wK | y | y | y | y | 0 | 715 | SidebarLayout, TopBar, Navigation +8 |
| `governance/GovernanceWallets.stories.tsx` | 1 | 3 | 1 | - |  | y | y | y | 0 | 110 | GovernanceWallets |
| `loading/chain-storage/ChainStorageLocationPicker.stories.tsx` | 1 | 6 | 1 | wK |  |  | y |  | 0 | 112 | ChainStorageLocationPicker (via harness) |
| `loading/mithril/MithrilBootstrap.stories.tsx` | 1 | 2 | 1 | wK |  |  | y |  | 0 | 153 | MithrilBootstrap (via harness) |
| `loading/mithril/MithrilDecisionView.stories.tsx` | 1 | 3 | 1 | wK |  |  | y |  | 0 | 95 | MithrilDecisionView (via harness) |
| `loading/mithril/MithrilErrorView.stories.tsx` | 1 | 2 | 1 | wK |  |  | y |  | 0 | 54 | MithrilErrorView |
| `loading/mithril/MithrilPartialSyncDialogue.stories.tsx` | 1 | 5 | 1 | wK |  | y | y |  | 0 | 135 | SyncingConnectingMithrilPrompt |
| `loading/mithril/MithrilPartialSyncOverlay.stories.tsx` | 1 | 15 | 1 | wK |  | y | y |  | 0 | 388 | MithrilSyncOverlay |
| `loading/mithril/MithrilProgressView.stories.tsx` | 1 | 3 | 1 | wK |  |  | y |  | 0 | 100 | MithrilProgressView |
| `navigation/Sidebar.stories.tsx` | 1 | 7 | 2 | sel wK |  | y | y | y | 15 | 255 | Sidebar |
| `navigation/SidebarCategory.stories.tsx` | 1 | 2 | 1 | - |  | y | y |  | 1 | 31 | SidebarCategory |
| `navigation/SidebarWalletsMenu.stories.tsx` | 1 | 2 | 2 | wK |  | y | y | y | 4 | 149 | SidebarWalletsMenu |
| `news/AlertsOverlay.stories.tsx` | 1 | 2 | 1 | sel wK |  | y | y |  | 3 | 119 | RTSFlagsRecommendationOverlay, AlertsOverlay |
| `news/AppUpdateOverlay.stories.tsx` | 1 | 1 | 2 | bool num rad wK |  | y | y |  | 2 | 89 | AppUpdateOverlay |
| `news/IncidentOverlay.stories.tsx` | 1 | 3 | 2 | sel wK |  | y | y |  | 1 | 68 | IncidentOverlay |
| `news/NewsFeed.stories.tsx` | 1 | 3 | 1 | bool num sel wK |  | y | y |  | 4 | 108 | NewsFeed |
| `nodes/about/About.stories.tsx` | 1 | 1 | 1 | wK |  |  | y |  | 0 | 16 | AboutDialog (container) |
| `nodes/environment/TopBarEnvironment.stories.tsx` | 1 | 3 | 2 | bool wK |  | y | y | y | 10 | 144 | SidebarLayout, TopBar +6 |
| `nodes/errors/Errors.stories.tsx` | 1 | 2 | 1 | wK |  |  | y |  | 1 | 18 | re-exports sibling stories |
| `nodes/errors/NoDiskSpaceError.stories.tsx` | 0 | 0 | 0 | text |  |  |  |  | 3 | 17 | NoDiskSpaceError |
| `nodes/errors/SystemTimeError.stories.tsx` | 0 | 0 | 0 | bool num |  | y |  |  | 0 | 18 | SystemTimeError |
| `nodes/splash/Splash.stories.tsx` | 1 | 1 | 1 | - |  |  | y |  | 0 | 11 | SplashNetworkFlight |
| `nodes/status/Diagnostics.stories.tsx` | 2 | 8 | 2 | - |  | y | y |  | 0 | 156 | DaedalusDiagnostics, MithrilPartialSyncConfirmation +1 |
| `nodes/status/Status.stories.ts` | 0 | 0 | 0 | - |  |  |  |  | 0 | 2 | barrel only |
| `nodes/syncing/Syncing.stories.tsx` | 1 | 3 | 1 | wK |  |  | y |  | 0 | 20 | re-exports sibling stories |
| `nodes/syncing/SyncingConnecting.stories.tsx` | 0 | 0 | 0 | bool num rad |  | y |  |  | 4 | 163 | SyncingConnecting |
| `nodes/updates/DataLayerMigration.stories.tsx` | 0 | 0 | 0 | - |  | y |  |  | 0 | 8 | DataLayerMigrationForm |
| `nodes/updates/Updates.stories.tsx` | 1 | 1 | 1 | wK |  |  | y |  | 0 | 14 | re-exports sibling stories |
| `notifications/Notifications.stories.tsx` | 1 | 3 | 2 | bool btn num text wK | y | y | y |  | 0 | 184 | InlineNotification, Notification |
| `settings/general/General.stories.tsx` | 1 | 7 | 1 | bool num | y | y |  |  | 5 | 138 | DisplaySettings, SecuritySettings +5 |
| `settings/language/Language.stories.tsx` | 1 | 2 | 1 | - | y | y | y |  | 0 | 36 | InitialSettings |
| `staking/CountdownParty.stories.tsx` | 1 | 1 | 2 | bool wK |  | y | y |  | 3 | 71 | TopBar, StakingInfoCountdown +3 |
| `staking/DelegationCenter.stories.tsx` | 0 | 0 | 0 | num |  | y |  |  | 19 | 401 | DelegationCenter |
| `staking/DelegationSteps.stories.tsx` | 0 | 0 | 0 | bool num |  | y |  |  | 8 | 244 | DelegationStepsChooseStakePoolDialog +5 |
| `staking/Epochs.stories.tsx` | 0 | 0 | 0 | date num |  |  |  |  | 3 | 45 | StakingEpochs |
| `staking/Legacy.stories.tsx` | 1 | 2 | 1 | - |  |  | y |  | 0 | 57 | StakingChart, StakingChartTooltip (orphaned) |
| `staking/RedeemItnWallets.stories.tsx` | 0 | 0 | 0 | bool num sel |  | y |  |  | 8 | 180 | NoWalletsDialog, Step1ConfigurationDialog +4 |
| `staking/Rewards.stories.tsx` | 0 | 0 | 0 | - |  | y |  |  | 1 | 30 | StakingRewards |
| `staking/StakePools.stories.tsx` | 0 | 0 | 0 | bool num sel |  | y |  |  | 3 | 125 | StakePools |
| `staking/StakePoolsTable.stories.tsx` | 0 | 0 | 0 | num |  | y |  |  | 2 | 95 | StakePoolsSearch, StakePoolsTable |
| `staking/Staking.stories.tsx` | 3 | 23 | 3 | bool date num wK |  | y | y | y | 10 | 296 | StakingCountdown, StakingInfo +3 |
| `staking/Undelegate.stories.tsx` | 0 | 0 | 0 | bool num |  |  |  |  | 3 | 112 | UndelegateWalletConfirmationDialog +1 |
| `voting/Voting.stories.tsx` | 2 | 10 | 4 | bool num sel wK |  | y | y | y | 1 | 208 | VotingInfo, VotingFooterLinks +6 |
| `wallets/addWallet/Add.stories.tsx` | 1 | 1 | 0 | bool |  |  |  |  | 1 | 36 | WalletAdd |
| `wallets/addWallet/AddWallet.stories.ts` | 0 | 0 | 0 | - |  |  |  |  | 0 | 6 | barrel only |
| `wallets/addWallet/Create.stories.tsx` | 1 | 2 | 1 | - |  | y |  |  | 1 | 22 | WalletCreateDialog |
| `wallets/addWallet/Import.stories.tsx` | 1 | 1 | 1 | - |  | y |  |  | 0 | 18 | WalletFileImportDialog |
| `wallets/addWallet/Restore.stories.tsx` | 1 | 4 | 1 | sel |  | y |  |  | 4 | 146 | ConfigurationDialog, MnemonicsDialog +2 |
| `wallets/addWallet/RestoreOld.stories.tsx` | 1 | 1 | 1 | bool |  | y |  |  | 0 | 22 | WalletRestoreDialog |
| `wallets/export/WalletExportToFile.stories.tsx` | 1 | 3 | 1 | - |  | y |  |  | 3 | 42 | ExportWalletToFileDialog |
| `wallets/hardwareWallets/HardwareWallets.stories.tsx` | 1 | 10 | 1 | - |  | y |  |  | 10 | 144 | WalletConnectDialog |
| `wallets/import/WalletImportFile.stories.tsx` | 1 | 2 | 1 | bool num sel |  | y |  |  | 3 | 82 | WalletImportFileDialog +2 |
| `wallets/legacyWallets/LegacyNotification.stories.tsx` | 1 | 1 | 2 | bool text wK |  | y | y |  | 1 | 23 | LegacyNotification |
| `wallets/legacyWallets/TransferFunds.stories.tsx` | 1 | 2 | 2 | bool num sel wK |  | y |  | y | 7 | 103 | TransferFundsStep1Dialog, TransferFundsStep2Dialog |
| `wallets/paperWallets/PaperWallets.stories.tsx` | 1 | 5 | 1 | - |  | y | y |  | 3 | 70 | InstructionsDialog, PrintDialog +3 |
| `wallets/receive/WalletReceive.stories.tsx` | 1 | 3 | 1 | bool num sel |  | y |  |  | 4 | 147 | WalletReceiveDialog, WalletReceiveRandom +2 |
| `wallets/send/WalletSend.stories.tsx` | 1 | 6 | 1 | bool num |  | y |  |  | 10 | 489 | SendConfirmation.view (container), WalletSendForm |
| `wallets/setPassword/SetWalletPassword.stories.tsx` | 1 | 2 | 1 | bool wK |  |  | y |  | 0 | 107 | ChangeSpendingPasswordDialog, SetWalletPassword |
| `wallets/settings/PublicKeyQRCode.stories.tsx` | 1 | 1 | 1 | - |  |  |  |  | 0 | 18 | ICOPublicKeyQRCodeDialog |
| `wallets/settings/WalletDelete.stories.tsx` | 1 | 4 | 1 | - |  | y | y |  | 0 | 112 | WalletSettingsRemoveConfirmationDialog |
| `wallets/settings/WalletPublicKey.stories.tsx` | 1 | 1 | 2 | bool wK |  | y |  |  | 1 | 23 | WalletPublicKeyDialog |
| `wallets/settings/WalletRecoveryPhraseVerification.stories.tsx` | 1 | 1 | 1 | bool obj opts sel wK |  | y | y |  | 5 | 100 | WalletRecoveryPhraseVerificationWidget |
| `wallets/settings/WalletSettings.stories.tsx` | 1 | 1 | 1 | - |  |  |  |  | 1 | 18 | barrel plus one registration |
| `wallets/settings/WalletSettingsScreen.stories.tsx` | 0 | 0 | 0 | bool num sel text |  | y |  |  | 9 | 445 | ChangeSpendingPasswordDialog +10 |
| `wallets/settings/WalletUnpair.stories.tsx` | 1 | 2 | 1 | - |  | y | y |  | 0 | 68 | WalletSettingsRemoveConfirmationDialog |
| `wallets/summary/WalletSummary.stories.tsx` | 1 | 1 | 1 | bool num sel text |  | y |  |  | 4 | 259 | WalletSummary |
| `wallets/summary/WalletSummaryHeader.stories.tsx` | 1 | 1 | 1 | bool num text wK |  |  | y | y | 0 | 56 | WalletSummaryHeader |
| `wallets/tokens/WalletTokens.stories.tsx` | 1 | 1 | 2 | bool text wK | y | y |  |  | 0 | 171 | WalletTokens |
| `wallets/tokens/WalletTokensList.stories.tsx` | 1 | 1 | 2 | bool text wK | y | y | y | y | 8 | 188 | WalletTokensList |
| `wallets/transactions/Transaction.stories.tsx` | 1 | 1 | 2 | bool num sel text wK |  | y | y | y | 2 | 214 | Transaction |
| `wallets/transactions/TransactionMetadata.stories.tsx` | 1 | 1 | 0 | - |  |  |  |  | 0 | 8 | TransactionMetadataView |
| `wallets/transactions/TransactionsList.stories.tsx` | 1 | 2 | 3 | sel wK |  | y |  |  | 5 | 248 | WalletTransactions |
| `wallets/transactions/Utxo.stories.tsx` | 1 | 1 | 1 | num |  |  |  |  | 0 | 62 | WalletUtxo |

### Counts per API

| API | Files | Call sites |
|---|--:|--:|
| `storiesOf()` from `@storybook/react` | 69 | 73 |
| `.add()` | 69 | 272 |
| `.addDecorator()` | 67 | 99 |
| `.addParameters()` | 0 | 0 |
| `@storybook/addon-knobs` imported | 64 | 364 knob calls, in story files only; 75 files and 396 calls once the support modules are counted |
| `@storybook/addon-actions` imported | 58 | 495 `action()` calls |
| `@storybook/addon-links` imported | 2 story files, 4 support modules, `storybook/main.ts` | 7 |
| `@dump247/storybook-state` imported | 9 story files, 1 support module | 10 |
| `StoryDecorator` imported directly | 48 | |
| `StoryDecorator` reached transitively | 68 | |
| `StoryProvider` imported directly | 19 | |
| `StoryProvider` reached transitively | 37 | |
| `StoryLayout` reached transitively | 22 | |
| DaedalusMenu addon imported directly | 1 story file, 1 support module | |

Knob types in use, by number of files importing each and by call sites:

| Knob | Files | Call sites |
|---|--:|--:|
| `withKnobs` | 44 | n/a, decorator |
| `boolean` | 36 | 165 |
| `number` | 32 | 96 |
| `select` | 21 | 44 |
| `text` | 13 | 50 |
| `date` | 3 | 3 |
| `radios` | 2 | 2 |
| `button` | 1 | 2 |
| `object` | 1 | 1 |
| `optionsKnob` | 1 | 1 |

Four knob types account for 355 of the 364 call sites in story files: `boolean`, `number`, `text` and `select`.
The remaining nine call sites are `date` (3), `radios` (2), `button` (2), `object` (1) and
`optionsKnob` (1), each with a direct argument-type equivalent.

Fifteen files register no stories of their own. They export story functions for a sibling to
register, or are pure barrels:

`nodes/errors/NoDiskSpaceError.stories.tsx`, `nodes/errors/SystemTimeError.stories.tsx`,
`nodes/status/Status.stories.ts`, `nodes/syncing/SyncingConnecting.stories.tsx`,
`nodes/updates/DataLayerMigration.stories.tsx`, `staking/DelegationCenter.stories.tsx`,
`staking/DelegationSteps.stories.tsx`, `staking/Epochs.stories.tsx`,
`staking/RedeemItnWallets.stories.tsx`, `staking/Rewards.stories.tsx`,
`staking/StakePools.stories.tsx`, `staking/StakePoolsTable.stories.tsx`,
`staking/Undelegate.stories.tsx`, `wallets/addWallet/AddWallet.stories.ts`,
`wallets/settings/WalletSettingsScreen.stories.tsx`.

`DelegationCenter.stories.tsx` is 401 lines and
`WalletSettingsScreen.stories.tsx` is 445, between them holding 28 of the 229 `@ts-ignore`
directives. Under the current API they are ordinary modules that happen to export components; under
CSF the same split is expressed differently, and each is a decision about where the fixture data
should live rather than a mechanical rename.

The four highest-volume files are `governance/DRepDirectory.stories.tsx` (731 lines, 20 stories),
`governance/Delegation.stories.tsx` (715, 24), `wallets/send/WalletSend.stories.tsx` (489, 6) and
`wallets/settings/WalletSettingsScreen.stories.tsx` (445, 0 direct). Together with
`governance/DRepDetail.stories.tsx` (408) and `staking/DelegationCenter.stories.tsx` (401) they are
3,189 lines, 29 percent of the story corpus in six files.

## 6. What `storybook/addons/` does

`storybook/addons/DaedalusMenu/` is 193 lines across four TypeScript files, plus a 39-line
stylesheet. It adds a toolbar control that switches three global dimensions, and it is the only
mechanism by which a story renders in anything other than the first theme, English, and Windows
metrics.

The three dimensions come from `storybook/stories/_support/config.ts`:

- **theme**, nine values (`config.ts:11-21`): Cardano, DarkBlue, LightBlue, DarkCardano,
  FlightCandidate, Yellow, White, IncentivizedTestnet, ShelleyTestnet
- **locale**, two values (`config.ts:34-37`): `en-US`, `ja-JP`
- **operating system**, three values (`config.ts:39-43`): Windows, Linux, Mac, which map to minimum
  window heights of 641px, 660px and 700px (`config.ts:46-50`) because the application chrome
  differs per platform

It is built from four pieces:

**`register.tsx`** registers a `types.TOOL` addon against `@storybook/addons`, rendering
`<DaedalusMenu api={api} />` into the manager toolbar.

**`DaedalusMenu.tsx`** is the toolbar UI: three groups of buttons, one per dimension. On click, it
emits `daedalusMenu/updateParam` on the manager API (`DaedalusMenu.tsx:44-47`). On receiving
`daedalusMenu/paramUpdated`, it writes the value to component state, to the parent frame's URL hash
(`setHashParam`, `:56-62`), to `sessionStorage` (`:53`), and to Storybook's query params via
`api.setQueryParams` (`:54`). Persisting to three places at once is what makes a selection survive a
reload and makes a story URL shareable with its theme and locale attached.

**`index.ts`** is the preview-side half. It takes `addons.getChannel()` at module scope and exposes
`setInitialState`, `updateParam` and `onReceiveParam`. It also re-emits: `channel.on(
'daedalusMenu/updateParam', ...)` emits `daedalusMenu/paramUpdated` (`index.ts:12-14`), which is
what closes the loop back to the toolbar.

**`DaedalusMenuStyles.ts`** is an inline style object, and `DaedalusMenu.css` its stylesheet.

The consumer is `storybook/stories/_support/StoryWrapper.tsx`, registered as the single global
decorator at `storybook/preview.tsx:8`. It subscribes with `onReceiveParam` on mount
(`StoryWrapper.tsx:40`), pushes its own initial state back so the toolbar starts in sync (`:41`),
and on each change re-renders `ThemeManager`, `WindowSizeManager` and `IntlProvider` around the
story (`:64-84`). It also passes `osName`, `locale` and `currentTheme` to the story **as props**
(`:77-81`), which is why so many story functions have the signature `(props) => ...` or
`(_, props) => ...` and read `props.currentTheme`. Four story files carry comments saying they
depend on this: `governance/DRepDetail.stories.tsx:67`,
`governance/DRepDirectory.stories.tsx:248`,
`loading/mithril/MithrilPartialSyncDialogue.stories.tsx:30` and
`nodes/status/Diagnostics.stories.tsx:88`.

One story writes back to the toolbar rather than only reading from it:
`storybook/stories/settings/general/General.stories.tsx:7` imports `updateParam` and calls it at
`:97`, so picking a theme inside the "Themes" story moves the toolbar selection too.

### What a modern equivalent must provide

1. A toolbar control with three independent selectors, nine by two by three values.
2. A preview-side decorator that reads the current selection and rebuilds `ThemeManager`,
   `WindowSizeManager` and `IntlProvider` around every story.
3. Delivery of the selection to the story itself, not only to the surrounding providers. Storybook's
   modern globals arrive through the story context rather than as props, so every story function
   that currently reads `props.currentTheme`, `props.osName` or `props.locale` changes signature.
   A `storiesOf` to CSF transform that only rewrites the registration calls will not catch these
   signatures.
4. URL and session persistence, if the shareable-link behavior is to be kept. This is currently
   hand-rolled in `DaedalusMenu.tsx:53-62`; modern Storybook globals are already URL-encoded, so
   this code has a direct replacement rather than a port.
5. A write path from a story back to the toolbar, for the one call site in `General.stories.tsx`.

Points 1, 2, 4 and 5 map onto standard toolbar globals and a global decorator. Point 3 does not, and
should be costed separately.

`storybook/preview-head.html` is zero bytes and can be deleted.

## 7. Type checking, linting and formatting

### Stories are type-checked

`tsconfig.json` declares no `include` and no `files`, only `"exclude": ["node_modules"]`
(`tsconfig.json:103`). TypeScript therefore takes every `.ts`/`.tsx` file under the repository root.
Confirmed against the resolved configuration:

```
$ ./node_modules/.bin/tsc --showConfig | python3 -c "..."
total files in tsc program: 1644
storybook/ files: 124
story files in program: 84
```

All 84 story files and all 124 files under `storybook/` are in the program. `yarn compile` is
`tsc --noEmit` (`package.json` scripts) and is a required check, wrapped `x86_64-linux` only, at
`perSystem/checks.nix:55`.

Running it now:

```
$ ./node_modules/.bin/tsc --noEmit 2>&1 | grep -c "error TS"
4
```

All four are in `source/renderer/app/utils/crypto.ts` (three) and
`source/renderer/app/utils/dataSerialization.ts` (one), all `Buffer`/`Uint8Array` assignability
complaints characteristic of a `@types/node` resolved outside the Nix development shell. Zero errors
are reported under `storybook/` and zero in any story file.

Two qualifications on how much protection this gives:

`tsconfig.json:79-85` sets `"strict": false`, `"noImplicitAny": false` and
`"noImplicitThis": false`, with `strictNullChecks` commented out. Prop objects can omit required
fields and pass.

The story files carry 229 `@ts-ignore` directives across 48 of the 84 files, concentrated in
`staking/DelegationCenter.stories.tsx` (19), `navigation/Sidebar.stories.tsx` (15),
`common/ItemsDropdown.stories.tsx` (14), `staking/Staking.stories.tsx` (10),
`nodes/environment/TopBarEnvironment.stories.tsx` (10),
`wallets/hardwareWallets/HardwareWallets.stories.tsx` (10) and
`wallets/send/WalletSend.stories.tsx` (10). Most carry the `ts-migrate(NNNN) FIXME` marker left by
the JavaScript-to-TypeScript conversion, so they suppress errors nobody has read. Keeping them
preserves unknown breakage. Removing them surfaces real prop mismatches, which needs to be
budgeted.

The practical consequence for risk: a story that fails to compile fails CI today, so the rewrite
cannot silently break the type contract between a story and its component. What it can silently
break is anything the 229 suppressions are hiding, and anything `strict: false` lets through.

### Stories are linted, but nothing is enforced

`package.json` defines `lint` as
`eslint --format=node_modules/eslint-formatter-pretty source storybook utils --ext .ts,.tsx`, so
`storybook/` is in scope, and `lint` is a required check at `perSystem/checks.nix:54`.

```
$ ./node_modules/.bin/eslint storybook --ext .ts,.tsx -f json
files linted: 124
errors: 0  warnings: 457
```

Every finding is a warning, so the check passes regardless. The top rules are
`@typescript-eslint/ban-ts-comment` (261, tracking the `@ts-ignore` count),
`@typescript-eslint/no-empty-function` (81), `@typescript-eslint/no-explicit-any` (51),
`no-unused-vars` (26) and `@typescript-eslint/no-unused-vars` (16). `.eslintrc` has no
storybook-specific override; `.eslintignore` does not mention `storybook/`.

`eslint-plugin-storybook` appears in neither `package.json` nor `.eslintrc`, so no rule flags a
deprecated API or a malformed story.

### Stories are formatted

`perSystem/formatter.nix:75-79` lists `storybook/**/*.{js,ts,tsx,scss,json}` in the prettier
includes, and `.prettierignore` un-ignores `!storybook/`. `nix fmt` covers them. Formatting is not a
`checks.nix` entry, but `check:all` includes `prettier:check`.

`perSystem/formatter.nix:48` excludes `.agent`, so files under that directory are not formatted by
treefmt.

### The required build check

`storybook:build` is `build-storybook -c storybook -o dist/storybook` and is a required check at
`perSystem/checks.nix:78`, wrapped `x86_64-linux` only. Because `main.ts` names a single entry file,
this check verifies that `storybook/stories/index.ts` and everything it transitively imports
compiles and bundles. It says nothing about story files not in that graph, which is why
`staking/Legacy.stories.tsx` can be orphaned without any check noticing. `tsc` compiles it; the
Storybook build never sees it.

## 8. Toolchain constraints on the rewrite

Pinned versions, from `package.json`:

| Package | Version |
|---|---|
| `@storybook/react`, `addons`, `addon-actions`, `addon-links`, `core`, `builder-webpack5`, `manager-webpack5` | 6.4.22 |
| `@storybook/addon-knobs` | 6.4.0 |
| `@dump247/storybook-state` | 1.6.1 |
| `storybook-addon-swc` | 1.1.7 (declared, never referenced) |
| `react`, `react-dom` | 16.14.0 |
| `react-intl` | 2.9.0 |
| `mobx` | 5.15.7 |
| `mobx-react` | 6.3.1 |
| `react-polymorph` | 1.0.4 |
| `typescript` | 4.9.5 |
| `webpack` | 5.106.2 |
| `@swc/core` | 1.10.18 |
| `swc-loader` | 0.1.15 |

Four properties of the current setup constrain what a target version can be.

**Legacy decorators are mandatory and are configured in three places.** `tsconfig.json:17` sets
`experimentalDecorators: true`, `storybook/main.ts:79` sets the SWC parser's `decorators: true`, and
`storybook/main.ts:84` sets `legacyDecorator: true`. `storybook/main.ts:87` adds
`useDefineForClassFields: false`. Inline comments there record that MobX 5 breaks under TC39 stage 3
decorators, and that class fields must use assignment so MobX prototype setters can intercept
initialization. Any builder change has to preserve all four settings.

`tsconfig.json:24` sets `useDefineForClassFields: true` while `storybook/main.ts:87` sets it
`false`, so the type checker and the bundler disagree about class field semantics. The bundler's
value is the one that takes effect at runtime and the one MobX 5 requires.

**`storiesOf()` is the entire API.** All 73 registrations use it; zero files use Component Story
Format. `storiesOf` was removed in Storybook 8, so the target version determines whether this is a
rewrite of every file or a staged migration. Storybook 7 still accepts it behind a compatibility
import, which makes a two-step route available.

**Knobs and `@dump247/storybook-state` have no forward path.** `@storybook/addon-knobs` was
deprecated in Storybook 6.3 and removed in 7. Its replacement, `argTypes` and `args`, is not a
drop-in: knobs are called inside the render function and re-read on every render, whereas args are
declared outside it. Every one of the 396 knob call sites in the corpus moves out of the story body.
`@dump247/storybook-state` is a third-party addon last published for Storybook 5 era APIs; the ten
files using it need local `useState` or a render-function wrapper instead.

**React 16.14.0 is the ceiling.** Storybook 8 supports React 16 through its `react` renderer. The
constraint is elsewhere: React 16, `react-intl@2.9.0` and `react-polymorph@1.0.4` together fix the
component tree in place. `react-intl` at 2.9.0 predates the hooks API entirely, which is why
`storybook/stories/_support/StoryWrapper.tsx:3-18` still calls `addLocaleData`. The Storybook
rewrite has to work against the components as they are.

## 9. Corrections to previously circulated figures

| Claim | Measured | Note |
|---|---|---|
| 82 story files | 84 | 80 under `storybook/stories/`, 4 in `source/` |
| 72 files import addon-knobs | 64 | of 84 story files; 44 import `withKnobs` specifically. Across the whole corpus, including the support modules the stories import, it is 75 files and 396 knob calls |
| 10 files use `withState` | 10 | 9 story files plus `settings/utils/helpers.tsx` |
| 110 containers | 110 `.tsx`, 105 excluding specs | 132 files in the directory: 110 `.tsx`, 17 `.ts`, 5 `.scss` |
| 359 components | 359 | `.tsx`, excluding `.spec`, `.stories`, `.story` |
| Config in `storybook/`, not `.storybook/` | confirmed | `storybook/main.ts`, `preview.tsx`, `preview-head.html` (0 bytes), `addons/` |
| `storybook:build` is a required CI check | confirmed | `perSystem/checks.nix:78`, `x86_64-linux` only |
| Stories use `storiesOf()`, not CSF | confirmed | 73 `storiesOf()` calls, 0 CSF default exports |

## 10. Open questions for the project owner

1. **Delete or migrate the four dead-feature story sets?** Paper wallet certificate creation
   (5 stories), staking info (2), staking countdown (2 panels), legacy wallet notification and
   transfer funds (3). All document screens no user can reach. Migrating them costs rewrite effort
   and keeps a misleading catalog; deleting them is a product statement that the features are
   gone. The flags are one-line reversions, so the question is whether any of these are coming back.

2. **Delete `staking/Legacy.stories.tsx` and `components/staking/legacy/`?** The story is orphaned
   from the sidebar and is the only reference to eight otherwise-unreferenced components. Neither
   the story nor the directory is reachable from the application or from any check.

3. **Target Storybook 7 or 8?** 7 accepts `storiesOf` behind a compatibility import and allows the
   84 files to move in tranches. 8 forces all 272 registrations across in one change. Knobs are
   removed in 7 either way, so the choice is whether the `storiesOf` conversion can be separated
   from the knob conversion.

4. **Keep the manual barrel or move to a glob?** The current single-entry arrangement is what let a
   story file go dead without any check failing. A glob would have made it a build error. Moving to
   a glob changes sidebar ordering, which is currently implicit in barrel import order.

5. **Should the rewrite add container-level stories?** Only 2 of 105 containers have one today, and
   both are incidental. Adding them means building mock store fixtures for containers that inject
   MobX stores, which is a larger body of work than the port itself. Settling this before the port
   starts avoids moving 272 stories into a shape that then has to change again.

6. **Remove the 229 `@ts-ignore` directives during the rewrite, or carry them?** Carrying them is
   cheaper and preserves the current CI signal exactly. Removing them will surface prop mismatches
   that have been suppressed since the TypeScript conversion, some of which are likely real defects
   in the components rather than the stories.

7. **Reconcile `useDefineForClassFields`?** `tsconfig.json:24` says true, `storybook/main.ts:87`
   says false. The bundler value is correct for MobX 5. The type checker is currently checking
   against semantics the runtime does not use.
