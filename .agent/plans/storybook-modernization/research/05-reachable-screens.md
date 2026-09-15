# Reachable screens, enumerated from the router

Measured against `3eaa4f17fac50a959057143904d488303fe4922b` on branch `feat/drep-discovery`,
2026-09-10. Companion to `01-current-coverage.md` in this directory, which measured the story
corpus at the same commit.

This list is the coverage target for container-level stories. It is built from the route
configuration, not from directory names. Two properties have to hold for it to be usable as a
target: every screen on the list must be reachable in a shipped build, and every screen left off
must be genuinely unreachable. Section 4 gives the exclusions with the evidence that puts each one
outside the list, so the second property is auditable rather than asserted. Three decisions the
project owner has since taken are recorded inline in sections 4.3 and 4.4, beside the evidence each
rests on.

## 1. Where the routes are defined

| File | What it holds |
|---|---|
| `source/renderer/app/routes-config.ts` | `ROUTES`, 56 lines, path strings only |
| `source/renderer/app/Routes.tsx` | the `<Switch>`, 265 lines, every `component=` binding |
| `source/renderer/app/containers/Root.tsx` | six pre-router branches that replace the whole content area |
| `source/renderer/app/App.tsx` | seven overlays mounted as siblings of `<Router>` |
| `source/renderer/app/containers/MainLayout.tsx` | three content dialogs and the top bar, mounted for every in-shell screen |

`Routes.tsx:47` wraps everything in `withRouter` and mounts `<Root>` inside a single
`<Route path={ROUTES.ROOT}>`. `Root.tsx:59-99` decides whether the router's children render at all,
so a route match is a necessary but not sufficient condition for a screen appearing.

Routing uses `react-router-dom` 5.2.0 with hash history. Two v5 semantics matter for this analysis.
A `<Route>` whose `children` is an element rather than a function renders `null` when the path does
not match. A `<Switch>` renders only the first child whose `path` matches, reading `path` off the
child's props, which is why the custom `TrackedRoute` (`source/renderer/app/analytics/TrackedRoute.tsx:9-22`)
participates correctly: it spreads the `computedMatch` that `Switch` injects straight into a real
`<Route>`.

Because the packaged application has no address bar, a route with no in-app navigation affordance is
not reachable by a user. Reachability here means an affordance exists: a sidebar category, a
navigation tab, a `goToRoute` trigger, a `history.push`, an OS menu item, or a store branch that
mounts the container directly.

## 2. Counts

| Measure | Value |
|---|--:|
| `.tsx` files under `containers/`, excluding `*.spec.tsx` | 105 |
| Of those, statically reachable from `Routes.tsx`, `App.tsx` or `Root.tsx` | 105 |
| Reachable screens (the coverage target) | 49 |
| Distinct containers backing those 49 screens | 48 |
| Route destinations excluded as unreachable | 8 |
| Pure redirects, rendering no UI | 2 |
| Sub-screen units: dialog, wizard-step and dialog-fragment containers | 47 |

```
$ find source/renderer/app/containers -name '*.tsx' ! -name '*.spec.tsx' | wc -l
105
```

Two of these counts move once the decisions in sections 4.3 and 4.4 are executed. Deleting
`StakingEpochsPage.tsx` takes the container count to 104 and the unreachable route destinations to 7
containers rather than 8. Deleting the `/redeem-itn-wallets` binding takes section 4 to six excluded
routes rather than eight. The coverage target does not move: it stays at 49 screens and 48
containers, because `/staking/epochs` was never on it and `RedeemItnRewardsContainer` is mounted by
`Root.tsx:68-70` rather than by a route.

**The single most useful number is 48 against 105.** Every container in the directory is imported
somewhere in the tree, so "unreferenced container" is not the gap. The gap is that 57 of the 105 are
not screens: 47 are dialogs, wizard steps or fragments of a dialog, 8 are route destinations a user
cannot reach, and 2 are redirects. Defining coverage as router-reachable screens rather than as all
containers removes 54 percent of the container count from scope before any story is written.

One container serves two screens. `DRepDirectoryPage` is bound to both `/governance/dreps` and
`/governance/favorites` (`Routes.tsx:243-254`) and branches on `location.pathname` at
`DRepDirectoryPage.tsx:133-137`, so the two views are separate coverage targets from one file. That
is why 48 containers yield 49 screens.

## 3. The reachable screens

Stores are the store keys the container reads, extracted from `this.props.stores` destructuring and
direct `stores.x` access. `Story today` is measured at two layers: container means a story mounts
the container itself, component means a story renders what the container renders.

### 3.1 Route destinations (26)

| Route | Container | Stores required | Story today | Gap |
|---|---|---|---|---|
| `/profile/initial-settings` | `profile/InitialSettingsPage` | app, networkStatus, profile | component: `InitialSettings` storied; `TopBarLayout` not | container story; layout wrapper |
| `/profile/terms-of-service` | `profile/TermsOfUsePage` | app, networkStatus, profile | none at either layer | whole screen, including `TermsOfUseForm` |
| `/profile/analytics` | `profile/AnalyticsConsentPage` | app, networkStatus, profile | component: `AnalyticsConsentForm` storied | container story; only container using hooks, needs a real Provider |
| `/wallets/add` | `wallet/WalletAddPage` | hardwareWallets, uiDialogs, walletMigration, wallets | component: `WalletAdd` storied | container story; 6 dialog branches (`WalletAddPage.tsx:72-94`) |
| `/wallets/:id/summary` | `wallet/WalletSummaryPage` | addresses, app, assets, currency, profile, staking, transactions, uiDialogs, wallets | component: `WalletSummary` storied; `WalletNoTransactions` not | container story; empty state |
| `/wallets/:id/send` | `wallet/WalletSendPage` | app, assets, hardwareWallets, profile, transactions, uiDialogs, wallets | component: `WalletSendForm` storied | container story; token picker and confirmation states |
| `/wallets/:id/receive` | `wallet/WalletReceivePage` | addresses, app, hardwareWallets, profile, sidebar, uiDialogs, walletSettings, wallets | component: `WalletReceiveRandom`, `WalletReceiveSequential`, `WalletReceiveDialog` storied | container story; random and sequential branches |
| `/wallets/:id/tokens` | `wallet/WalletTokensPage` | app, assets, profile, wallets | component: `WalletTokens` storied | container story |
| `/wallets/:id/transactions` | `wallet/WalletTransactionsPage` | addresses, app, assets, profile, transactions, wallets | component: `WalletTransactions` storied | container story |
| `/wallets/:id/settings` | `wallet/WalletSettingsPage` | app, hardwareWallets, profile, uiDialogs, walletSettings, wallets | component: `WalletSettings` storied with 10 dialogs | container story; hardware-wallet branch (`WalletSettingsPage.tsx:107`) |
| `/wallets/:id/utxo` | `wallet/WalletUtxoPage` | app, transactions, walletSettings, wallets | component: `WalletUtxo` storied | container story |
| `/settings/general` | `settings/categories/GeneralSettingsPage` | profile | none at either layer; `GeneralSettings` has no story | whole screen |
| `/settings/wallets` | `settings/categories/WalletsSettingsPage` | app, currency, profile | component storied | container story |
| `/settings/stake-pools` | `settings/categories/StakePoolsSettingsPage` | app, networkStatus, staking | component storied | container story |
| `/settings/terms-of-service` | `settings/categories/TermsOfUseSettingsPage` | app, profile | component storied | container story |
| `/settings/support` | `settings/categories/SupportSettingsPage` | app, profile | component storied | container story |
| `/settings/display` | `settings/categories/DisplaySettingsPage` | profile | component storied | container story |
| `/settings/security` | `settings/categories/SecuritySettingsPage` | none; reads the discreet-mode feature context | component storied | container story; needs `DiscreetModeFeatureProvider`, not stores |
| `/staking/delegation-center` | `staking/DelegationCenterPage` | app, networkStatus, profile, staking, uiDialogs, wallets | component: `DelegationCenter`, `DelegationCenterNoWallets` storied; `DelegationSetupWizardDialog` not | container story; no-wallets and two dialog states |
| `/staking/stake-pools` | `staking/StakePoolsListPage` | app, networkStatus, profile, staking, uiDialogs, wallets | component: `StakePools` storied; `StakePoolsRankingLoader` not | container story; ranking state |
| `/staking/rewards` | `staking/StakingRewardsPage` | app, staking, wallets | component: `StakingRewards` storied | container story |
| `/governance/dashboard` | `governance/GovernanceWalletsPage` | app, governance, wallets | component: `GovernanceWallets` storied | container story |
| `/governance/dreps` | `governance/DRepDirectoryPage` | governance, networkStatus, profile, wallets | component: `DRepDirectory` storied, 20 registrations | container story; four `refreshState` values (`DRepDirectoryPage.tsx:139`) |
| `/governance/favorites` | `governance/DRepDirectoryPage` | governance, networkStatus, profile, wallets | component storied | container story for the `view="favorites"` branch |
| `/governance/dreps/:drepId` | `governance/DRepDetailPage` | app, governance, networkStatus, wallets | component: `DRepDetail` storied, 15 registrations | container story; route param and three `detailRefreshState` values |
| `/governance/delegate` | `voting/VotingGovernancePage` | app, governance, hardwareWallets, networkStatus, staking, voting, wallets | component: `VotingPowerDelegation` storied | container story; `VotingUnavailable` branch (`VotingGovernancePage.tsx:77`) |

`/governance/delegate` has no navigation tab. It is reached from the directory and detail screens,
which push it at `DRepDirectoryPage.tsx:80` and `DRepDetailPage.tsx:90`.

### 3.2 Layout shells that own a route (5)

Each renders chrome plus `children`. They are separate coverage targets because each has a state in
which it replaces its children entirely.

| Mount | Container | Stores required | Story today | Gap |
|---|---|---|---|---|
| `Routes.tsx:75` `/wallets` | `wallet/Wallet` | app, uiDialogs, walletSettings, wallets | component: `WalletWithNavigation` storied via `storybook/stories/wallets/_utils/WalletWithNavigationLayout.tsx` | container story; no-active-wallet spinner and `RestoreNotification` (`Wallet.tsx:65-88`) |
| `Routes.tsx:119` `/settings` | `settings/Settings` | app, networkStatus, router | component: `SettingsLayout`, `SettingsMenu` storied via `storybook/stories/settings/utils/SettingsWrapper.tsx:7-8` | container story |
| `Routes.tsx:167` `/staking` | `staking/Staking` | app, networkStatus, staking, uiDialogs | component: `StakingWithNavigation` storied; `StakingUnavailable` not | container story; the not-synced branch (`Staking.tsx:96-104`) |
| `Routes.tsx:224` `/governance` | `voting/Governance` | app | component: `GovernanceWithNavigation` storied | container story |
| every in-shell screen | `MainLayout` | app, networkStatus, profile, router, sidebar, wallets | component: `Sidebar`, `SidebarLayout` storied via `storybook/stories/_support/StoryLayout.tsx` | container story; it is the only consumer of `stores.router.location` (`MainLayout.tsx:100`) |

### 3.3 Full-surface screens mounted outside the router (9)

`Root.tsx` returns these instead of the router's children, so no route matches while they are on
screen.

| Mount | Container | Stores required | Story today | Gap |
|---|---|---|---|---|
| `Root.tsx:65` | `splash/SplashNetworkPage` | app | component: `SplashNetworkFlight` storied | container story; flight builds only |
| `Root.tsx:69` | `staking/RedeemItnRewardsContainer` | app, networkStatus, staking, wallets | none; `LoadingOverlay` has no story, the five step dialogs do | container story and the loading state |
| `Root.tsx:73` | `appUpdate/AppUpdateContainer` | app, appUpdate | component: `AppUpdateOverlay` storied | container story |
| `Root.tsx:92` | `loading/LoadingPage` | backend, networkStatus | none for the page; its branches are storied | container story; three `loadingPhase` branches plus two overlays |
| `LoadingPage.tsx:92` | `loading/SyncingConnectingPage` | app, appUpdate, backend, networkStatus, newsFeed, profile | component: `SyncingConnecting` storied | container story |
| `LoadingPage.tsx:21` | `loading/NoDiskSpaceErrorPage` | networkStatus | component storied | container story |
| `LoadingPage.tsx:22` | `loading/SystemTimeErrorPage` | app, networkStatus, profile | component storied | container story |
| `LoadingPage.tsx:85` | `loading/MithrilSyncContainer` | app, backend | component: five Mithril views storied | container story |
| `LoadingPage.tsx:60` | `loading/ChainStorageContainer` | backend | component storied | container story |

### 3.4 Overlays and chrome mounted above the router (9)

| Mount | Container | Stores required | Story today | Gap |
|---|---|---|---|---|
| `App.tsx:83` | `static/AboutDialog` | app | **container**, `storybook/stories/nodes/about/About.stories.tsx:12-14` | the props fixture is three fields deep; nothing reusable |
| `App.tsx:86` | `status/DaedalusDiagnosticsDialog` | app, backend, networkStatus | component: `DaedalusDiagnostics` storied | container story |
| `App.tsx:90` | `knownIssues/ToggleRTSFlagsDialogContainer` | networkStatus | none at either layer | whole screen; the only one with no coverage of any kind |
| `App.tsx:93` | `knownIssues/RTSFlagsRecommendationOverlayContainer` | networkStatus, profile | component storied | container story |
| `App.tsx:94` | `notifications/NotificationsContainer` | uiNotifications | component: `Notification` storied | container story |
| `App.tsx:96` | `news/NewsFeedContainer` | app, appUpdate, networkStatus, newsFeed, profile | component: `NewsFeed` storied | container story |
| `App.tsx:97` | `news/NewsOverlayContainer` | app, newsFeed, profile | component: `IncidentOverlay`, `AlertsOverlay` storied | container story |
| `MainLayout.tsx:109` | `TopBarContainer` | app, appUpdate, networkStatus, newsFeed, sidebar, staking, wallets | component: `TopBar` storied | container story; also needs the discreet-mode feature context |
| `MainLayout.tsx:117` | `assets/AssetSettingsDialogContainer` | assets, uiDialogs | component storied | container story |

### 3.5 Screens with more than one meaningful state

Each of these becomes several stories rather than one. They are the states verified in source, not
an exhaustive enumeration of every conditional.

| Screen | States | Evidence |
|---|---|---|
| `Root` | splash, redeem, app update, pass-through, loading, add-wallet | `Root.tsx:59-99` |
| `loading/LoadingPage` | chain storage, Mithril bootstrap or sync, syncing, disk-space overlay, system-time overlay | `LoadingPage.tsx:20-95` |
| `/wallets` shell | no active wallet, restoring, normal | `Wallet.tsx:65-88` |
| `/wallets/add` | 6 dialog branches plus the bare screen | `WalletAddPage.tsx:72-94` |
| `/wallets/:id/summary` | transactions, loading first page, restoring, empty | `WalletSummaryPage.tsx:136-171` |
| `/wallets/:id/settings` | hardware wallet, software wallet, 10 dialogs | `WalletSettingsPage.tsx:107`, `:167-191` |
| `/staking` shell | not synced, countdown, normal | `Staking.tsx:96-119` |
| `/staking/delegation-center` | no wallets, list, undelegate dialog, delegation wizard | `DelegationCenterPage.tsx:84-126` |
| `/staking/stake-pools` | list, ranking in progress, delegation wizard | `StakePoolsListPage.tsx:111-114` |
| `/governance/dreps` | Idle, Loading, Loaded, Failed, plus directory and favorites views | `DRepDirectoryPage.tsx:133-141` |
| `/governance/dreps/:drepId` | Loading, Loaded, Failed | `DRepDetailPage.tsx:29-32`, `:123` |
| `/governance/delegate` | unavailable, delegation form, confirmation dialog | `VotingGovernancePage.tsx:77`, `:131` |

## 4. Excluded, with evidence

Eight route destinations are defined in `Routes.tsx` and cannot be reached. They are listed here
rather than dropped silently so the exclusion can be re-checked when a flag moves. Two of the eight
are being removed from the repository rather than carried as exclusions; 4.3 and 4.4 record those
decisions in place.

### 4.1 Gated by a compile-time flag

**`/staking/info`, `staking/StakingInfoPage`.** `source/renderer/app/config/stakingConfig.ts:104`
sets `IS_STAKING_INFO_PAGE_AVAILABLE = false`. The constant gates both the route
(`Routes.tsx:201`) and the navigation tab (`Staking.tsx:115`).

**`/paper-wallet/create-certificate`, `wallet/PaperWalletCreateCertificatePage`.** The route exists
at `Routes.tsx:163-166` and the dialog flow is also mounted at `MainLayout.tsx:111`. The only
affordance is the sidebar category, and `source/renderer/app/stores/SidebarStore.ts:123` sets it
false unconditionally. The sidebar entry would not navigate anyway: `MainLayout.tsx:26-29`
intercepts that category and opens the first dialog instead. The five dialog containers under
`containers/wallet/dialogs/paper-wallet-certificate/` fall with it.

**Legacy wallet notification and `wallet/TransferFundsPage`.**
`source/renderer/app/config/walletsConfig.ts:44` sets `IS_BYRON_WALLET_MIGRATION_ENABLED = false`,
which gates `LegacyNotification` at `TopBar.tsx:95-98`. The transfer-funds flow has exactly one
entry point: `onTransferFunds` originates at `TopBarContainer.tsx:72-73`, reaches `TopBar.tsx:101`,
and is consumed only by `LegacyNotification.tsx:142`. `TransferFundsPage` itself returns null
unless `wallets.transferFundsStep` is non-zero (`TransferFundsPage.tsx:24`), and the only writer of
that field is `WalletsStore.ts:918-929`, reached only from that one trigger. The two step containers
under `containers/wallet/dialogs/transfer-funds/` fall with it.

### 4.2 Gated by a runtime condition that no shipping network satisfies

**`/staking/countdown`, `staking/StakingCountdownPage`.** Two paths could reach it and neither
fires. The sidebar category is disabled at `SidebarStore.ts:124`. The redirect at `Staking.tsx:35`
is conditioned on `staking.showCountdown()`, which returns `networkStatus.isShelleyPending`
(`StakingStore.ts:659-662`), set at `NetworkStatusStore.ts:515` to
`currentTimeStamp < shelleyActivationTimeStamp`. Shelley's epoch start is in the past on every
network the application ships against, so the value is false. This exclusion is weaker than a flag:
a local network configured with a future Shelley era start would make the screen appear. The
screen's primary action also navigates to `/staking/info` (`StakingCountdownPage.tsx:39`), which is
the flag-gated route above, so the flow is incomplete even where it can be reached.

### 4.3 No navigation affordance exists

**`/staking/epochs`, `staking/StakingEpochsPage`. Decided: remove it, route, container and
components.** The route is live at `Routes.tsx:196-200`, but the navigation item is commented out at
`source/renderer/app/components/staking/navigation/StakingNavigation.tsx:64-67`, and
`ROUTES.STAKING.PAGE` is a `goToRoute` target rather than a `<Switch>` binding
(`containers/staking/Staking.tsx:71-78`), so nothing else can land on the path. The container also
renders fixture data rather than store data: `StakingEpochsPage.tsx:5-6` imports
`config/stakingPreviousEpoch.dummy.json` and `config/stakingCurrentEpoch.dummy.json` and reads no
store at all.

The reason for deleting it rather than restoring the navigation item is that the information already
reaches users. `components/staking/delegation-center/DelegationCenterHeader.tsx` renders the current
epoch with its slot counts, a countdown to the next epoch (`:154`, `:165-172`) and the sentence at
`:43-45` telling the user when a delegation change takes effect;
`components/staking/delegation-center/DelegationCenter.tsx:61-67` mounts it at the top of
`/staking/delegation-center`. It takes its data from the delegation center's own props and is
entirely independent of the deleted page. The only thing that exists nowhere else is the epoch
progress bar at `StakingEpochs.tsx:124-135`, which renders on the deleted page alone and is
unreachable today, so no user loses anything visible.

That the container reads no store is also what separates this from `/voting` below. A stub wired to
dummy JSON is not a suspended feature: there is nothing behind the missing affordance to bring back.

What goes with it: the binding at `Routes.tsx:196-200` and its import at `:20`,
`ROUTES.STAKING.EPOCHS` at `routes-config.ts:13`, `containers/staking/StakingEpochsPage.tsx`, the
seven files under `components/staking/epochs/`, the commented navigation item at
`StakingNavigation.tsx:64-67` and the `messages.epochs` entry at `:24-28` that was its only reader,
both dummy JSON fixtures, which nothing else imports, and
`storybook/stories/staking/Epochs.stories.tsx` with the registration that imports it at
`Staking.stories.tsx:17` and `:172`. Twelve message ids fall out of the locale files, the eleven
under `staking.epochs.*` and `staking.navigation.epochs`.

**`/voting` and `/voting/registration`, `voting/Voting` and `voting/VotingRegistrationPage`.**
`ROUTES.VOTING` is referenced in exactly three places: the two `Routes.tsx` bindings at `:215` and
`:219`, and the `isVotingPage` computed at `VotingStore.ts:735`. There is no sidebar category for
voting: `source/renderer/app/config/sidebarConfig.ts:23-64` defines `WALLETS`,
`PAPER_WALLET_CREATE_CERTIFICATE`, `STAKING_DELEGATION_COUNTDOWN`, `STAKING`, `SETTINGS`,
`NETWORK_INFO` and `GOVERNANCE`, and the voting icon imported at `sidebarConfig.ts:15` now belongs
to the governance category. No `goToRoute`, no `history.push` and no OS menu item targets either
path. `VotingRegistrationDialogContainer` falls with the page.

Decided: the screens stay unreachable and stay off the coverage target, and the stories are kept.
Catalyst is suspended rather than retired and may return, so the owner is unwilling to remove it.
`storybook/stories/voting/Voting.stories.tsx`, 10 registrations across two `storiesOf` calls,
converts to CSF with the rest of the corpus, and so does every other story that renders a component
under `components/voting/`, which today means `storybook/stories/governance/Delegation.stories.tsx`.
This is a deliberate exception to the rule applied to the four flag-disabled story sets, which are
deleted: those features were judged gone, Catalyst is judged suspended. The observable state is the
same in both cases and the difference is a product judgment, not a measurement.

**`/profile/data-layer-migration`, `profile/DataLayerMigrationPage`.** The only navigation into it
is `ProfileStore._redirectToDataLayerMigrationScreenIfMigrationHasNotAccepted`, defined at
`ProfileStore.ts:521-550`. That function is referenced nowhere else in the source tree and does not
appear in the `registerReactions` array at `ProfileStore.ts:186-193`, which registers
`_updateBigNumberFormat`, `_redirectToInitialSettingsIfNoLocaleSet`,
`_redirectToAnalyticsScreenIfNotConfirmed`, `_redirectToTermsOfUseScreenIfTermsNotAccepted`,
`_redirectToMainUiAfterTermsAreAccepted` and `_redirectToMainUiAfterDataLayerMigrationIsAccepted`.
The redirect never runs, so the screen never appears.

### 4.4 A route that cannot match

**`/redeem-itn-wallets`.** `Routes.tsx:209-213` places the `TrackedRoute` for
`ROUTES.REDEEM_ITN_REWARDS` inside `<Route path={ROUTES.STAKING.ROOT}>`, as a sibling of
`<Staking>`. Under react-router 5, a `<Route>` with element children renders `null` when its own
path does not match, so the inner route is only evaluated while the location already starts with
`/staking`, where it can never match. Navigating to `/redeem-itn-wallets` finds no matching child in
the outer `<Switch>` and renders nothing. Nothing navigates there in any case: `ROUTES.REDEEM_ITN_REWARDS`
appears only in `routes-config.ts:4` and `Routes.tsx:211`.

`RedeemItnRewardsContainer` is nevertheless a reachable screen and stays on the list. It is mounted
by `Root.tsx:68-70` when `staking.redeemStep` is non-null, set by `StakingStore._onRedeemStart`
(`StakingStore.ts:890-893`) in response to the OS menu item at `source/main/menus/osx.ts:40-46` and
`source/main/menus/win-linux.ts:44`, which sends `ITN_REWARDS_REDEMPTION` over
`showUiPartChannel` (`source/main/utils/buildAppMenus.ts:40-43`) and is handled at
`AppStore.ts:147`. The route binding is dead; the screen is not.

Decided: delete the binding. `Routes.tsx:209-213` goes, with the import at `:25` that nothing else
uses and `ROUTES.REDEEM_ITN_REWARDS` at `routes-config.ts:4`, whose only reader is `Routes.tsx:211`.
Moving it out to the top-level `<Switch>` would give the screen a second entry point nothing asks
for, and leaving it in place keeps a route that can never match, which is a trap for anyone who
later tries to link to the screen: the link resolves, nothing renders, and no error is reported.
The screen and the coverage target are unaffected.

### 4.5 Dead sub-screen containers behind a store default

Not route destinations, but they belong with the exclusions because they will otherwise be counted
as work.

`WalletsStore.ts:169` sets `createWalletUseNewProcess = false` and the only mutator,
`_togglecreateWalletUseNewProcess` at `WalletsStore.ts:426-428`, is not registered against any
action. `WalletAddPage.tsx:49-54` therefore always opens the old dialog, `createWalletStep` stays
null, and seven containers are unreachable: `wallet/dialogs/WalletCreateDialogContainer` and the six
under `wallet/dialogs/wallet-create/`.

`WalletsStore.ts:197` sets `restoreWalletUseNewProcess = true` and nothing mutates it, so the four
containers under `wallet/dialogs/wallet-restore/` are live and
`wallet/dialogs/WalletRestoreDialogContainerOld` is not.

Sixteen of the 47 sub-screen containers are unreachable on these grounds: 7 wallet-create, 1
old restore, 5 paper wallet certificate, 2 transfer funds, 1 voting registration dialog.

## 5. Dialogs and modal flows with no route of their own

Thirty-one sub-screen containers are reachable. The question for each is whether it is a distinct
coverage target or a state of the screen that opens it. The rule applied: a container with its own
store reads and its own submit path is a distinct target, because a story of the parent cannot
exercise it without first driving the parent's state machine. A dialog implemented as a plain
component rendered inline from the parent's render is a state of the parent.

| Flow | Containers | Opened from | Verdict |
|---|--:|---|---|
| Wallet create, old path | 1 | `/wallets/add` (`WalletAddPage.tsx:76`) | distinct target |
| Wallet backup | 1 | `/wallets/add` (`:81`) | distinct target |
| Wallet restore, new path | 5 | `/wallets/add` (`:86`) | distinct target; 1 dialog container plus 4 steps |
| Wallet import | 3 | `/wallets/add` (`:89`) | distinct target; 1 dialog container plus 2 steps |
| Hardware wallet connect | 1 | `/wallets/add` (`:92`) | distinct target |
| Wallet settings dialogs | 8 | `/wallets/:id/settings` (`WalletSettingsPage.tsx:167-191`) | distinct target each; all eight are passed as elements and mounted by `WalletSettings` |
| Send confirmation | 6 files | `/wallets/:id/send` (`WalletSendPage.tsx:90-92`) | 1 distinct target; the other five are fragments of one view and belong to its stories |
| Delegation setup wizard | 1 | delegation center and stake pools (`DelegationCenterPage.tsx:124`, `StakePoolsListPage.tsx:113`) | distinct target, reached from two screens |
| Undelegate wallet | 0 | delegation center and wallet settings | counted in the wallet settings row; reached from two screens |
| Redeem ITN rewards | 5 | `RedeemItnRewardsContainer` (`:53`) and its two guard branches (`:49`, `:52`) | distinct target per step |
| Asset settings | 0 | summary and tokens, via `MainLayout.tsx:117` | counted as an overlay screen in 3.4 |
| Token picker | 0 | `/wallets/:id/send` (`WalletSendPage.tsx:104-106`) | state of the parent; `WalletTokenPicker` is a component, not a container |
| Receive address dialog | 0 | `/wallets/:id/receive` (`WalletReceivePage.tsx:244-245`) | state of the parent |
| Voting power confirmation | 0 | `/governance/delegate` (`VotingGovernancePage.tsx:131`) | state of the parent |
| Paper wallet certificate | 5 | unreachable, see 4.1 | excluded |
| Transfer funds | 2 | unreachable, see 4.1 | excluded |
| Voting registration | 1 | unreachable, see 4.3 | excluded |

That gives 31 reachable sub-screen targets, of which 26 are distinct coverage targets once the five
send-confirmation fragments are folded into their view.

## 6. What a container story actually requires

The state-management prose under `.agent/system/` does not describe the current code. The following
is read from source.

### 6.1 The store map and how it is built

`source/renderer/app/stores/index.ts:33-57` declares `storeClasses`, 23 entries.
`stores/index.ts:58-83` declares the `StoresMap` type, 24 entries: the same 23 plus `router`, a
`RouterStore` from `mobx-react-router` that is passed in rather than constructed.

`setUpStores` is at `stores/index.ts:94-143` and takes four parameters, not the two the prose
describes:

```ts
export const setUpStores = action(
  (
    api: Api,
    actions: ActionsMap,
    router: RouterStore,
    analyticsTracker: AnalyticsTracker
  ): StoresMap => {
```

It constructs every store with `new StoreSubClass(api, actions, analyticsTracker)`
(`stores/index.ts:101-105`), assigns them into a single `observable({...})`, then runs two passes
over all of them: `store.configure(stores)` and `store.initialize()` (`stores/index.ts:138-141`).

The base class is `stores/lib/Store.ts`. Its constructor takes three arguments
(`Store.ts:13-17`), `configure` assigns the whole store map onto `this.stores` (`Store.ts:25-27`),
and `initialize` calls `setup()` then starts every registered reaction (`Store.ts:31-35`).

Two consequences for a fixture. First, `setUpStores` is unusable in a story: it needs a real `Api`,
and `initialize()` starts the reactions that poll the node. Second, stores reference each other
through `this.stores`, so a mock that only supplies one store will fail as soon as a computed
crosses into another. A fixture has to be a map, not a store.

### 6.2 How stores reach a container

Two mechanisms, and they are not equivalent.

**`@inject('stores', 'actions')` with `@observer`, used by 47 of the 48 screen containers.**
`App.tsx:62` mounts `<Provider stores={stores} actions={actions}>` and the decorator pulls both out
of context. `source/renderer/app/types/injectedPropsType.ts:6-10` types the props as
`any | StoresMap`, so nothing type-checks the shape of what a story passes.

The decisive detail is that mobx-react 6.3.1 prefers explicit props over the Provider. Its
`grabStoresByName` returns early for any store name already present in `nextProps`, so
`<SomeContainer stores={mock} actions={mock} />` renders without a Provider and without throwing.
This is how the one existing container story works:
`storybook/stories/nodes/about/About.stories.tsx:12-14` passes `aboutDialogProps` as props, and
`storybook/stories/nodes/_utils/props.ts:3-21` is a 19-line literal supplying two fields of
`stores.app` and one action.

**Hooks, used by one screen container.** `profile/AnalyticsConsentPage.tsx:10-11` calls
`useStores()` and `useActions()`. Both read `React.useContext(MobXProviderContext)` directly
(`source/renderer/app/hooks/useStores.ts:5-7`). There is no prop fallback, so this container needs a
real `<Provider>`. A harness built on props alone cannot serve it.

The recommendation is to standardize on a real `<Provider>` for every container story, not on props.
It is the only mechanism that serves both, it matches how the application mounts the tree, and it
means a nested container reached from a parent's render receives the same stores instead of
`undefined`. Several screens do render nested containers: `WalletSettingsPage` mounts eight,
`WalletAddPage` five, `LoadingPage` five, `MainLayout` three.

### 6.3 The other providers a screen needs

Store injection is not the whole wrapper. Measured across the 48 screen containers:

| Requirement | Screens | Provided today by |
|---|--:|---|
| `<Provider stores actions>` | 48 | `storybook/stories/_support/StoryProvider.tsx:273-277`, with three partial stores |
| `IntlProvider` | 48 | `storybook/stories/_support/StoryWrapper.tsx:70-76`, registered globally at `storybook/preview.tsx:8` |
| react-polymorph `ThemeProvider` | 48 | `storybook/stories/_support/StoryDecorator.tsx:25-30`, opt-in per story |
| `ThemeManager` and `WindowSizeManager` | 48 | `StoryWrapper.tsx:67-69`, global |
| `intlShape` via `contextTypes` | 5 | satisfied by `IntlProvider` |
| `injectIntl` | 1 | satisfied by `IntlProvider` |
| A `<Router>`, for `withRouter` or `history` | 5 | **nothing provides one today** |
| `stores.router.location` | 2 | requires a `RouterStore` stub in the store map |
| `AnalyticsProvider` for `withAnalytics` | 3 | **nothing provides one today** |
| `DiscreetModeFeatureProvider` | 2 | `StoryProvider.tsx:279` |
| `BrowserLocalStorageBridge` | 2 | `StoryProvider.tsx:278` |

The five `withRouter` screens are `voting/Governance`, `governance/GovernanceWalletsPage`,
`governance/DRepDirectoryPage`, `governance/DRepDetailPage` and `voting/VotingGovernancePage`.
`DRepDetailPage.tsx:56` reads `this.props.match.params.drepId`, so its story needs a router seeded
with a real path, not a stub object.

The three `withAnalytics` screens are `WalletSummaryPage`, `WalletSendPage` and
`StakePoolsListPage`. `withAnalytics` calls `useAnalytics()` (`components/analytics/withAnalytics.tsx:16`),
which reads a context, so a provider is required rather than a prop.

## 7. Can one harness serve most containers

Yes, and this is the finding that decides whether the work scales linearly.

The reason is structural: every container reads the same shape. There is one `StoresMap`, one
`ActionsMap`, one injection decorator, and no container constructs a store or an API client of its
own. The variation between containers is which keys of the map they read and which fields of those
keys, not how they obtain them. Measured across the 48 screen containers, the mean is 3.2 store keys
each and the maximum is 9 (`WalletSummaryPage`).

Store usage is also concentrated. Across the 48:

| Store | Screens reading it |
|---|--:|
| app | 35 |
| profile | 23 |
| networkStatus | 20 |
| wallets | 16 |
| uiDialogs | 9 |
| staking | 7 |
| assets, backend, hardwareWallets | 5 each |
| appUpdate, newsFeed, transactions, walletSettings | 4 each |
| addresses, sidebar | 3 each |
| currency, governance, router | 2 each |
| uiNotifications, voting, walletMigration | 1 each |
| walletBackup, walletsLocal, window | 0 |

Four stores account for most of the surface. A harness that supplies a credible `app`, `profile`,
`networkStatus` and `wallets`, plus an `actions` map whose every trigger is a recorded no-op, covers
the common dependency of nearly every screen. Per-screen work is then a partial override of two or
three keys.

The shape the harness has to provide:

1. A plain object keyed by the 24 `StoresMap` names, each defaulting to an object whose fields are
   the observable defaults of the real store rather than the real store instance. Constructing real
   stores is not viable: `Store.initialize()` starts reactions (`Store.ts:31-35`) and several
   constructors expect a live `Api`.
2. The real `ActionsMap` from `source/renderer/app/actions`, which `StoryProvider.tsx:9` already
   imports and which is inert without listeners, or a recorded stand-in if stories should log
   triggers.
3. Request-shaped objects wherever a container reads `...Request.isExecuting`, `.error`,
   `.wasExecuted`, `.result` or `.isExecutingFirstTime`. This pattern appears on roughly a dozen
   screens, `InitialSettingsPage.tsx:39` and `WalletSummaryPage.tsx:137` among them, and it is the
   single most common cause of a null dereference in a naive fixture.
4. A `MemoryRouter` seeded with the screen's path, and a `RouterStore` stub whose `location` matches
   it, so `MainLayout.tsx:100` and `Settings.tsx:21-28` agree with `withRouter`.
5. `AnalyticsProvider` with a no-op tracker.
6. The existing `DiscreetModeFeatureProvider` and `BrowserLocalStorageBridge`, already wired at
   `StoryProvider.tsx:278-279`.

Four screens need setup beyond a store override and are the exceptions to the shared harness:

- `SecuritySettingsPage` reads no stores at all. It needs only the discreet-mode feature context
  (`SecuritySettingsPage.tsx:7`).
- `AnalyticsConsentPage` needs a real Provider rather than props, per 6.2.
- `DRepDetailPage` needs a router seeded with a `:drepId` and a resolved `fetchDRep` promise
  (`DRepDetailPage.tsx:56-65`), because the entry arrives through component state rather than the
  store.
- `LoadingPage` and its five branches key off `backend.loadingPhase`
  (`LoadingPage.tsx:41-88`), a field with no representation in the current `StoryProvider`.

The existing `StoryProvider` is a partial instance of the thing needed. `StoryProvider.tsx:220-264`
supplies three stores with about a dozen fields between them, and `StoryProvider.tsx:273-277` already
mounts the real `<Provider>` with the real `actions`. Extending it to the full store map is a
continuation of what is there, not a new mechanism.

## 8. Tranches

Sequenced so that each tranche's harness additions are a superset of the one before, and so the
first tranche proves the pattern on screens with the fewest dependencies.

**Tranche 1, profile and app only. 8 screens.** `DisplaySettingsPage`, `GeneralSettingsPage`,
`TermsOfUseSettingsPage`, `SupportSettingsPage`, `SecuritySettingsPage`, `AboutDialog`,
`SplashNetworkPage`, `AssetSettingsDialogContainer`. Harness: store map, actions, intl, theme.
`GeneralSettingsPage` and `SecuritySettingsPage` are the right first two, being the screen with no
story at all and the screen with no store dependency.

**Tranche 2, add networkStatus. 8 screens.** `InitialSettingsPage`, `TermsOfUsePage`,
`AnalyticsConsentPage`, `WalletsSettingsPage`, `NoDiskSpaceErrorPage`, `SystemTimeErrorPage`,
`ToggleRTSFlagsDialogContainer`, `RTSFlagsRecommendationOverlayContainer`. Harness adds the
network-status defaults and request objects. `AnalyticsConsentPage` proves the Provider path;
`ToggleRTSFlagsDialogContainer` closes the one screen with no coverage of any kind.

**Tranche 3, add backend. 5 screens.** `LoadingPage`, `SyncingConnectingPage`,
`MithrilSyncContainer`, `ChainStorageContainer`, `DaedalusDiagnosticsDialog`. Harness adds
`backend.loadingPhase` and the Mithril fields. `LoadingPage` is the first screen with several
branches worth separate stories.

**Tranche 4, add newsFeed, appUpdate and uiNotifications. 4 screens.** `NewsFeedContainer`,
`NewsOverlayContainer`, `AppUpdateContainer`, `NotificationsContainer`.

**Tranche 5, chrome: add sidebar and router. 4 screens.** `MainLayout`, `TopBarContainer`,
`Settings` and `Governance`. Harness adds `MemoryRouter`, the `RouterStore`
stub and `AnalyticsProvider`. This tranche unblocks everything after it, because every remaining
screen renders inside `MainLayout`.

**Tranche 6, wallets. 9 screens.** `Wallet` shell, `WalletAddPage`, `WalletSummaryPage`,
`WalletSendPage`, `WalletReceivePage`, `WalletTokensPage`, `WalletTransactionsPage`,
`WalletSettingsPage`, `WalletUtxoPage`. Harness adds `wallets`, `transactions`, `assets`,
`addresses`, `walletSettings`, `walletMigration`, `hardwareWallets`, `currency`, `uiDialogs`.
This is the largest tranche by fixture volume: the wallet fixtures at
`storybook/stories/_support/StoryProvider.tsx:20-129` are a starting point but cover only the
`Wallet` domain shape, not `transactions` or `assets`.

**Tranche 7, staking. 6 screens.** `Staking` shell, `DelegationCenterPage`, `StakePoolsListPage`,
`StakingRewardsPage`, `StakePoolsSettingsPage`, `RedeemItnRewardsContainer`. Harness adds `staking`.
Stake pool fixtures already exist in the staking stories.

**Tranche 8, governance and voting. 5 screens.** `GovernanceWalletsPage`, `DRepDirectoryPage`
directory view, `DRepDirectoryPage` favorites view, `DRepDetailPage`, `VotingGovernancePage`.
Harness adds `governance` and `voting`, and the router has to carry a `:drepId`. The governance
stories already carry the richest fixtures in the corpus, 62 registrations across four files, so the
data exists and only the store wrapper is new.

Tranches 1 to 5 are 29 of the 49 screens and introduce every harness mechanism. Tranches 6 to 8 are
20 screens and introduce only data.

## 9. Estimate risks

**The harness, not the screens, is the schedule.** Tranches 1 to 5 build the mechanism. If the store
map, the router stub and the analytics provider land cleanly, the remaining 19 screens are fixture
data against a fixed wrapper. If they do not, every screen pays the cost again.

**Request objects are the most likely repeated failure.** Roughly a dozen screens read
`isExecuting`, `error`, `wasExecuted`, `result` or `isExecutingFirstTime` off a request field. A
fixture that omits one of these throws inside `render` rather than degrading, because the containers
read through without guarding. This is cheap to solve once, in the harness default, and expensive to
solve 12 times.

**`strict: false` gives no help.** `tsconfig.json:79-85` disables `strict` and `noImplicitAny`, and
`injectedPropsType.ts:6-10` types `stores` as `any | StoresMap`. An incomplete fixture compiles. The
first signal of a wrong fixture is a runtime failure in the story, which is why the harness needs
its own defaults rather than relying on the type checker.

**Multi-state screens multiply the count.** The 49 screens are not 49 stories. The 12 screens in
section 3.5 carry between two and six meaningful states each; the rest are mostly one or two. A
figure in the range of 90 to 120 stories is a reasonable planning estimate for full state coverage
of the 49 screens, but it is an estimate, and it is sensitive to how much dialog state is folded into
a parent's stories rather than given its own.

**Nested containers make a parent story mount a subtree.** `WalletSettingsPage` renders eight
containers, `WalletAddPage` five, `LoadingPage` five, `MainLayout` three. A props-based fixture
stops at the first nested container, which receives no stores. Only a real `<Provider>` renders the
subtree, and that means the parent's fixture must satisfy every child's store reads too. This is the
strongest argument for the Provider approach and the largest hidden cost if the props approach is
taken first and reworked later.

**The prior art is one 19-line literal.** `storybook/stories/nodes/_utils/props.ts` is the only
existing container fixture, and it covers the simplest container in the application. Nothing in the
corpus demonstrates the pattern at the scale of a wallet screen, so the first tranche-6 story is the
real proof and should be scheduled before the tranche is committed to.

**Flags can move.** Four of the six exclusions that remain in section 4 are one-line reversions. If
any of those features return, the screen count and the harness both grow. Section 4 exists so that the
check is a grep rather than a re-derivation.

## 10. Decisions this note does not make

Three questions this note originally left open have since been decided by the project owner and are
recorded in sections 4.3 and 4.4: the fate of `/staking/epochs`, the fate of `/voting` and
`/voting/registration`, and what to do with the misplaced `/redeem-itn-wallets` binding. Two
questions are still open.

1. Whether the unreachable sub-screen containers listed in 4.5 and 4.1 should be deleted along with
   the story sets that document them, or kept against a flag flip. Of the 16, the voting
   registration dialog container is already answered: 4.3 keeps voting, so it stays, which leaves 15
   behind compile-time flags and store defaults.
2. Whether dialog containers that are distinct targets under section 5 belong in the same coverage
   number as screens, or in a second tier with its own target.
