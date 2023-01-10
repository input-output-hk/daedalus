# Changelog

## vNext

### Features

- Added custom dimension to track legacy and hardware wallets ([PR 3091](https://github.com/input-output-hk/daedalus/pull/3091))
- Implemented CPU name grouping mechanism for analytics ([PR 3089](https://github.com/input-output-hk/daedalus/pull/3089))
- Implemented rounding RAM size in analytics ([PR 3088](https://github.com/input-output-hk/daedalus/pull/3088))

### Chores

- Update `cardano-node` to 1.35.4 for the Preview network respin ([PR 3082](https://github.com/input-output-hk/daedalus/pull/3082))
- Updated new Catalyst API URL ([PR 3087](https://github.com/input-output-hk/daedalus/pull/3087))
- Fixed Storybook state for General stories ([PR 3064](https://github.com/input-output-hk/daedalus/pull/3064))

## 5.1.1

### Chores

- Increased minimum free space required from 2gb to 4gb ([PR 3071](https://github.com/input-output-hk/daedalus/pull/3071))
- Fixed Language components in Storybook to have it's own mocked store ([PR 3063](https://github.com/input-output-hk/daedalus/pull/3063))
- Updated `cardano-node` configs for Preprod respin ([PR 3070](https://github.com/input-output-hk/daedalus/pull/3070))
- Fixed the properties and providers for the Send stories ([PR 3047](https://github.com/input-output-hk/daedalus/pull/3047))
- Fixed locale in Storybook ([PR 3062](https://github.com/input-output-hk/daedalus/pull/3062))
- Switched from Hydra’s `release.nix` to Cicero’s `flake.nix` ([PR 3060](https://github.com/input-output-hk/daedalus/pull/3060))

## 5.1.0

### Chores

- Updated `cardano-wallet` to v2022-10-06 ([PR 3045](https://github.com/input-output-hk/daedalus/pull/3045))
- Changed Japanese translations in delegate screen ([PR 3044](https://github.com/input-output-hk/daedalus/pull/3044))
- Updated trezor-connect to v9 ([PR 3038](https://github.com/input-output-hk/daedalus/pull/3038))
- Added analytics data collection ([PR 2927](https://github.com/input-output-hk/daedalus/pull/2927), [PR 2989](https://github.com/input-output-hk/daedalus/pull/2989), [PR 3003](https://github.com/input-output-hk/daedalus/pull/3003), [PR 3028](https://github.com/input-output-hk/daedalus/pull/3028))
- Updated LedgerJS to 5.1.0 ([PR 3036](https://github.com/input-output-hk/daedalus/pull/3036))

## 5.0.0

### Features

- Added new Mnemonic input component ([PR 2979](https://github.com/input-output-hk/daedalus/pull/2979))
- Updated Terms of Service ([PR 3009](https://github.com/input-output-hk/daedalus/pull/3009))

### Fixes

- Fixed blank screen when opening ITN rewards ([PR 3030](https://github.com/input-output-hk/daedalus/pull/3030))
- Ensured non-recommended decimal place setting alert is correctly shown ([PR 3007](https://github.com/input-output-hk/daedalus/pull/3007))
- Disabled the possibility to choose a syncing wallet for ITN rewards and delegation ([PR 3015](https://github.com/input-output-hk/daedalus/pull/3015))

### Chores

- Updated cardano-node to 1.35.3; added `vasil-dev`, `preprod`, `preview` variants ([PR 3025](https://github.com/input-output-hk/daedalus/pull/3025))
- Updated cardano-node to 1.35.2 ([PR 3021](https://github.com/input-output-hk/daedalus/pull/3021))
- Fix `darwin-launcher.go` to replace its process image with `cardano-launcher` (binary), and not swallow `stdout` ([PR 3023](https://github.com/input-output-hk/daedalus/pull/3023))
- Updated cardano-node to 1.35.1 ([PR 3012](https://github.com/input-output-hk/daedalus/pull/3012))

## 4.12.0

### Fixes

- Fixed downloaded installer being left in Downloads after latest update installs ([PR 2941](https://github.com/input-output-hk/daedalus/pull/2941))
- Fixed incorrect amount of token sent ([PR 2962](https://github.com/input-output-hk/daedalus/pull/2962))

### Chores

- Made Windows installer more resilient w.r.t. auto-updates ([PR 3017](https://github.com/input-output-hk/daedalus/pull/3017))
- Added OS-architecture tuple to installer file names to help with releases ([PR 3016](https://github.com/input-output-hk/daedalus/pull/3016))
- Added Vasil-supported cardano-wallet ([PR 3001](https://github.com/input-output-hk/daedalus/pull/3001))
- Upgraded webpack to version 5 ([PR 2772](https://github.com/input-output-hk/daedalus/pull/2772))

## 4.11.0

### Fixes

- Fixed incorrect behaviour of creating new wallet when paired incorrect hardware wallet during address verification ([PR 2906](https://github.com/input-output-hk/daedalus/pull/2906))
- Fixed phrasing of insufficient funds for tokens message ([PR 2966](https://github.com/input-output-hk/daedalus/pull/2966))
- Improved error handling for incorrect passphrase and incorrect hardware wallet error ([PR 2860](https://github.com/input-output-hk/daedalus/pull/2860))

### Features

- Added support for Ledger Nano S Plus ([PR 2975](https://github.com/input-output-hk/daedalus/pull/2975))
- Support of Apple AArch64 chip ([PR 2684](https://github.com/input-output-hk/daedalus/pull/2684))

### Chores

- Upgraded webpack to version 5 ([PR 2772](https://github.com/input-output-hk/daedalus/pull/2772))
- Bumped vulnerable dependencies versions ([PR 2943](https://github.com/input-output-hk/daedalus/pull/2943))
- Added support for Trezor firmware 2.5.1 ([PR 2991](https://github.com/input-output-hk/daedalus/pull/2991))
- Added steps on how to link with `react-polymorph` and other external UI libraries ([PR 2948](https://github.com/input-output-hk/daedalus/pull/2948))
- Published selfnode installers for all 3 platforms ([PR 2971](https://github.com/input-output-hk/daedalus/pull/2971))

## 4.10.0

### Features

- Implemented hover tooltips for menu ([PR 2938](https://github.com/input-output-hk/daedalus/pull/2938))
- Improved UI regarding the Hardware Wallet public key export error ([PR 2922](https://github.com/input-output-hk/daedalus/pull/2922))
- Added ASCII name to token header when metadata name is missing ([PR 2904](https://github.com/input-output-hk/daedalus/pull/2904))
- Improved IPC by reducing the amount of messages from periodic events ([PR 2892](https://github.com/input-output-hk/daedalus/pull/2892))
- Improved RTS flags splash screen message ([PR 2901](https://github.com/input-output-hk/daedalus/pull/2901))
- Implemented error message when trying to leave wallet without enough ada to support tokens ([PR 2783](https://github.com/input-output-hk/daedalus/pull/2783))

### Fixes

- Fixed dialogs being closed after receiving address shared ([PR 2965](https://github.com/input-output-hk/daedalus/pull/2965))
- Fixed no progress shown on loading screen on Windows ([PR 2967](https://github.com/input-output-hk/daedalus/pull/2967))
- Fixes hardware wallet issues on Windows ([PR 2900](https://github.com/input-output-hk/daedalus/pull/2900))
- Fixed stake pool list styling ([PR 2920](https://github.com/input-output-hk/daedalus/pull/2920))
- Fixed PopOver overlap ([PR 2954](https://github.com/input-output-hk/daedalus/pull/2954))
- Fixed tooltip being hidden in several places ([PR-2934](https://github.com/input-output-hk/daedalus/pull/2934))
- Adjusted padding for search field in stake pools ([PR 2945](https://github.com/input-output-hk/daedalus/pull/2945))
- Fixed margin for dialogs content and token table header ([PR 2944](https://github.com/input-output-hk/daedalus/pull/2944))
- Fixed performance issue on stake pool list view ([PR 2924](https://github.com/input-output-hk/daedalus/pull/2924))
- Fixed position of popup on syncing screen ([PR 2921](https://github.com/input-output-hk/daedalus/pull/2921))
- Fixed issue with missing character when copying address from PDF ([PR 2925](https://github.com/input-output-hk/daedalus/pull/2925))
- Fixed stake pool list view overlapping news feed ([PR 2917](https://github.com/input-output-hk/daedalus/pull/2917))
- Restored opacity for search icon when focused ([PR 2909](https://github.com/input-output-hk/daedalus/pull/2909))
- Fixed styling of the incentivized testnet rewards wallet dropdown ([PR 2907](https://github.com/input-output-hk/daedalus/pull/2907))
- Fix warning sign displayed when recommend decimals is zero ([PR 2905](https://github.com/input-output-hk/daedalus/pull/2905))
- Fixed discrete tooltip being clipped by loading overlay when stake pools are adjusted ([PR 2902](https://github.com/input-output-hk/daedalus/pull/2902))
- Sets minimum transaction fee to ada input field when tokens are removed ([PR 2918](https://github.com/input-output-hk/daedalus/pull/2918))

### Chores

- Updated cardano-wallet to v2022-04-27 and cardano-node to 1.34.1 ([PR 2951](https://github.com/input-output-hk/daedalus/pull/2951))
- Refactor to remove duplicated code ([PR 2956](https://github.com/input-output-hk/daedalus/pull/2956))
- Updated cardano-launcher to 0.20220119.0 ([PR 2839](https://github.com/input-output-hk/daedalus/pull/2839))
- Added `storybook:build` check to pre-push hook ([PR 2955](https://github.com/input-output-hk/daedalus/pull/2955))
- Using new faker.js ([PR 2855](https://github.com/input-output-hk/daedalus/pull/2855))
- Fixed spelling issues and typos ([PR 2915](https://github.com/input-output-hk/daedalus/pull/2915))
- Removed SASS ts-lint ignore comments ([PR 2870](https://github.com/input-output-hk/daedalus/pull/2870))
- Enabled debugging of the main process ([PR 2893](https://github.com/input-output-hk/daedalus/pull/2893))

## 4.9.1

### Fixes

- Fixed catalyst fund name ([PR 2946](https://github.com/input-output-hk/daedalus/pull/2946))
- Fixed macOS installer on Monterey 12.3 ([PR 2929](https://github.com/input-output-hk/daedalus/pull/2929))

### Chores

- Introduced new version of the `@cardano-foundation/ledgerjs-hw-app-cardano` package ([PR 2930](https://github.com/input-output-hk/daedalus/pull/2930))

## 4.9.0

### Features

- Added display of current/unspent rewards ([PR 2803](https://github.com/input-output-hk/daedalus/pull/2803))
- Improve the syncing screen by showing syncing progress split into three stages ([PR 2877](https://github.com/input-output-hk/daedalus/pull/2877))
- Improved stake pool searchbar ([PR 2847](https://github.com/input-output-hk/daedalus/pull/2847))
- Implemented catalyst dynamic content ([PR 2856](https://github.com/input-output-hk/daedalus/pull/2856))

### Fixes

- Fixed main container zIndex ([PR 2863](https://github.com/input-output-hk/daedalus/pull/2863))
- Fixed ui overlap issues ([PR 2881](https://github.com/input-output-hk/daedalus/pull/2881))
- Fixed the gap between Stake Pool View options ([PR 2899](https://github.com/input-output-hk/daedalus/pull/2899))

### Chores

- Fixed Daedalus menu in Storybook used for theme and language selection ([PR 2886](https://github.com/input-output-hk/daedalus/pull/2886))

## 4.9.0-FC1

### Features

- Added table view for delegated stake pools list ([PR 2837](https://github.com/input-output-hk/daedalus/pull/2837))
- Removed Discreet mode notification ([PR 2852](https://github.com/input-output-hk/daedalus/pull/2852))
- Unified CPU info in diagnostics dialog ([PR 2818](https://github.com/input-output-hk/daedalus/pull/2818))
- Implemented wallet sorting on sidebar menu ([PR 2775](https://github.com/input-output-hk/daedalus/pull/2775))
- Implemented new token picker ([PR 2787](https://github.com/input-output-hk/daedalus/pull/2787))
- Improved wallet send form ([PR 2791](https://github.com/input-output-hk/daedalus/pull/2791), [PR 2859](https://github.com/input-output-hk/daedalus/pull/2859))

### Fixes

- Fixed rewards CSV export issues ([PR 2885](https://github.com/input-output-hk/daedalus/pull/2885))
- Fixed behaviour of wallet settings option of the app menu ([PR 2838](https://github.com/input-output-hk/daedalus/pull/2838))
- Fixed styling of ITN rewards feature ([PR 2861](https://github.com/input-output-hk/daedalus/pull/2861))
- Fixed available disk space takes a long time to show ([PR 2849](https://github.com/input-output-hk/daedalus/pull/2849))

### Chores

- Migrated codebase from javascript to typescript ([PR 2843](https://github.com/input-output-hk/daedalus/pull/2843))
- Updated the list of team members ([PR 2805](https://github.com/input-output-hk/daedalus/pull/2805))

## 4.8.0

### Features

- Added dynamic RTS flags setting ([PR 2758](https://github.com/input-output-hk/daedalus/pull/2758/files))
- Improved UI/UX of RTS flags settings ([PR 2842](https://github.com/input-output-hk/daedalus/pull/2842), [PR 2846](https://github.com/input-output-hk/daedalus/pull/2846))
- Updated messages about Cardano node sync on the initial screen ([PR 2827](https://github.com/input-output-hk/daedalus/pull/2827), [PR 2831](https://github.com/input-output-hk/daedalus/pull/2831))

### Chores

- Updated check-disk-space version ([PR 2845](https://github.com/input-output-hk/daedalus/pull/2845))
- Updated CWB and Cardano Node ([PR 2822](https://github.com/input-output-hk/daedalus/pull/2822))

### Fixes

- Fixed blockchain verification progress text ([PR 2840](https://github.com/input-output-hk/daedalus/pull/2840))

## 4.8.0-FC1

### Features

- Added dynamic RTS flags setting ([PR 2758](https://github.com/input-output-hk/daedalus/pull/2758/files))
- Improved UI/UX of RTS flags settings ([PR 2842](https://github.com/input-output-hk/daedalus/pull/2842), [PR 2846](https://github.com/input-output-hk/daedalus/pull/2846))
- Updated messages about Cardano node sync on the initial screen ([PR 2827](https://github.com/input-output-hk/daedalus/pull/2827), [PR 2831](https://github.com/input-output-hk/daedalus/pull/2831))

### Chores

- Updated check-disk-space version ([PR 2845](https://github.com/input-output-hk/daedalus/pull/2845))
- Updated CWB and Cardano Node ([PR 2822](https://github.com/input-output-hk/daedalus/pull/2822))

### Fixes

- Fixed blockchain verification progress text ([PR 2840](https://github.com/input-output-hk/daedalus/pull/2840))

## 4.7.0

### Features

- Updated Catalyst dates ([PR 2812](https://github.com/input-output-hk/daedalus/pull/2812))

### Fixes

- Fixed immediate language updates of application top menu bar ([PR 2813](https://github.com/input-output-hk/daedalus/pull/2813))
- Fixed receiver address validation by disallowing rewards addresses ([PR 2781](https://github.com/input-output-hk/daedalus/pull/2781))

### Chores

- Integrated Chromatic for visual regression testing ([PR 2776](https://github.com/input-output-hk/daedalus/pull/2776))
- Updated `trezor-connect` dependency to version `8.2.4` ([PR 2726](https://github.com/input-output-hk/daedalus/pull/2726))
- Updated vulnerable dependencies ([PR 2769](https://github.com/input-output-hk/daedalus/pull/2769))
- Updated CWB and Cardano Node ([PR 2799](https://github.com/input-output-hk/daedalus/pull/2799))

## 4.6.0

### Features

- Implement catalyst state snapshot phase ([PR 2771](https://github.com/input-output-hk/daedalus/pull/2771))
- Implemented "discreet mode" ([PR 2723](https://github.com/input-output-hk/daedalus/pull/2723), [PR 2724](https://github.com/input-output-hk/daedalus/pull/2724), [PR 2725](https://github.com/input-output-hk/daedalus/pull/2725), [PR 2742](https://github.com/input-output-hk/daedalus/pull/2742), [PR 2740](https://github.com/input-output-hk/daedalus/pull/2740), [PR 2756](https://github.com/input-output-hk/daedalus/pull/2756))
- Updated slider component to only execute onAfterChange if slider had moved ([PR 2766](https://github.com/input-output-hk/daedalus/pull/2766))

### Fixes

- Fixed app update for specific platform ([PR 2759](https://github.com/input-output-hk/daedalus/pull/2759))
- Fixed checkbox tick offset ([PR 2751](https://github.com/input-output-hk/daedalus/pull/2751))

### Chores

- Improve startup and shutdown messages ([PR 2770](https://github.com/input-output-hk/daedalus/pull/2770))
- Updated `cardano-wallet` to version `2021-11-11` ([PR 2765](https://github.com/input-output-hk/daedalus/pull/2765))
- Added jest library for unit testing ([PR 2633](https://github.com/input-output-hk/daedalus/pull/2633))
- Updated `cardano-launcher` to version `0.20211105.1`

## 4.5.2

### Fixes

- Fixed Cardano Node starting/stopping issues

## 4.5.1

### Fixes

- Fixed cardano-node startup race condition
- Fixed automatic closing of the wallet "Restoration" dialog during restoration of a first wallet in UI
- Fixed Daedalus 4.5.0 Windows deployment issue

## 4.5.0

### Features

- Implemented "Catalyst Fund7" voting registration changes ([PR 2732](https://github.com/input-output-hk/daedalus/pull/2732))
- Added "Over-saturation" warning in the delegation wizard ([PR 2733](https://github.com/input-output-hk/daedalus/pull/2733), [PR 2738](https://github.com/input-output-hk/daedalus/pull/2738))
- Added Catalyst footer links ([PR 2721](https://github.com/input-output-hk/daedalus/pull/2721))

### Fixes

- Fixed the Delegation popover timeout ([PR 2722](https://github.com/input-output-hk/daedalus/pull/2722))
- Fixed issues relating to minimum window size in Daedalus ([PR 2719](https://github.com/input-output-hk/daedalus/pull/2719))
- Updated "Trezor T" image shown on the "Pair a hardware wallet device" dialog ([PR 2712](https://github.com/input-output-hk/daedalus/pull/2712))
- Fixed transaction timestamps localization ([PR 2702](https://github.com/input-output-hk/daedalus/pull/2702))
- Small UI/UX Fixes ([PR 2685](https://github.com/input-output-hk/daedalus/pull/2685), [PR 2744](https://github.com/input-output-hk/daedalus/pull/2744))

### Chores

- Removed "Alonzo tada" icon and "Alonzo countdown" screen ([PR 2708](https://github.com/input-output-hk/daedalus/pull/2708))
- Improved the Daedalus startup by avoiding unnecessary Cardano Node restarts ([PR 2716](https://github.com/input-output-hk/daedalus/pull/2716))
- Updated README with solution steps for the nix SSL issue ([PR 2727](https://github.com/input-output-hk/daedalus/pull/2727))
- Covered LedgerJS v4.0.0 breaking changes ([PR 2697](https://github.com/input-output-hk/daedalus/pull/2697))
- Added hardware wallet support for all non-public testnets ([PR 2672](https://github.com/input-output-hk/daedalus/pull/2672))

## 4.4.1

### Fixes

- Updated Electron package to the version which includes a fix for crashes on Windows

## 4.4.0

### Features

- Implemented the wallet Tokens dedicated screen ([PR 2671](https://github.com/input-output-hk/daedalus/pull/2671), [PR 2701](https://github.com/input-output-hk/daedalus/pull/2701), [PR 2703](https://github.com/input-output-hk/daedalus/pull/2703))

### Fixes

- Fixed wallet settings screen - no space at the bottom when scrolled down ([PR 2686](https://github.com/input-output-hk/daedalus/pull/2686))
- Fixed the missing text for the DAPP static screens ([PR 2693](https://github.com/input-output-hk/daedalus/pull/2693))

### Chores

- Added the possibility to unpair a hardware wallet from Daedalus ([PR 2676](https://github.com/input-output-hk/daedalus/pull/2676))

## 4.3.2

### Features

- Implemented static screens for signing dApp interaction transactions ([PR 2626](https://github.com/input-output-hk/daedalus/pull/2626))

### Fixes

- Fixed font used for unavailable staking while updating messages ([PR 2680](https://github.com/input-output-hk/daedalus/pull/2680))
- Fixed crash when Yubikey is connected ([PR 2673](https://github.com/input-output-hk/daedalus/pull/2673))
- Updated Electron and related packages ([PR 2206](https://github.com/input-output-hk/daedalus/pull/2206))
- Fixed some Japanese translations for the external currencies ([PR 2667](https://github.com/input-output-hk/daedalus/pull/2667))

### Chores

- Updated `cardano-wallet` to version `2021-09-29` which includes `cardano-node` 1.30.1
- Updated `trezor-connect` dependency to version `8.2.0` ([PR 2675](https://github.com/input-output-hk/daedalus/pull/2675))

## 4.3.1

### Fixes

- Covered Trezor firmware breaking changes ([PR 2629](https://github.com/input-output-hk/daedalus/pull/2629))

### Chores

- Updated `cardano-wallet` to version `2021-09-09`

## 4.3.0

### Features

- Added "Alonzo countdown" screen for the upcoming Alonzo upgrade ([PR 2653](https://github.com/input-output-hk/daedalus/pull/2653))
- Maintain window size and position between launches ([PR 2611](https://github.com/input-output-hk/daedalus/pull/2611))
- Emptying wallet - warning ([PR 2617](https://github.com/input-output-hk/daedalus/pull/2617))

### Fixes

- Fixed type issue on Storybook "Wallet Send Confirmation Dialog" story ([PR 2654](https://github.com/input-output-hk/daedalus/pull/2654))
- Fixed ‘Stakepool Metadata Aggregation Server (SMASH)’ link returning 404 error ([PR 2646](https://github.com/input-output-hk/daedalus/pull/2646))
- Enter key not working on send ([PR 2621](https://github.com/input-output-hk/daedalus/pull/2621))

### Chores

- Updated `cardano-wallet` to revision `9ae2d48b` which enables hardware wallet support in Alonzo era ([PR 2663](https://github.com/input-output-hk/daedalus/issues/2663))
- Updated `cardano-wallet` to version `2021-08-27` which includes `cardano-node` 1.29.0 ([PR 2650](https://github.com/input-output-hk/daedalus/pull/2650))
- Updated `cardano-wallet` to version `2021-08-11` which includes `cardano-node` alonzo-purple-1.0.1 ([PR 2641](https://github.com/input-output-hk/daedalus/pull/2641))
- Updated `cardano-wallet` to version `2021-07-30` which includes `cardano-node` 1.28.0 ([PR 2635](https://github.com/input-output-hk/daedalus/pull/2635), [PR 2638](https://github.com/input-output-hk/daedalus/pull/2638))
- Removed "Decentralization countdown" tada icon and info tab ([PR 2625](https://github.com/input-output-hk/daedalus/pull/2625))

## 4.2.1

### Chores

- Updated Catalyst Fund6 and Fund7 voting registration dates ([PR 2640](https://github.com/input-output-hk/daedalus/pull/2640))

## 4.2.0

### Features

- Added voting registration with Trezor ([PR 2615](https://github.com/input-output-hk/daedalus/pull/2615))
- Added voting registration with Ledger ([PR 2538](https://github.com/input-output-hk/daedalus/pull/2538))

## 4.2.0-FC1

### Features

- Implemented sharing multi-signature public key ([PR 2604](https://github.com/input-output-hk/daedalus/pull/2604))
- Implemented the expandable token view ([PR 2589](https://github.com/input-output-hk/daedalus/pull/2589), [PR 2609](https://github.com/input-output-hk/daedalus/pull/2609))

### Fixes

- Fixed asset amount validation ([PR 2472](https://github.com/input-output-hk/daedalus/pull/2472))
- Fixed incorrect global instances in scss files ([PR 2593](https://github.com/input-output-hk/daedalus/pull/2593))
- Fixed "shelley-qa" network issue on "Delegation" wizard ([PR 2595](https://github.com/input-output-hk/daedalus/pull/2595))
- Fixed Trezor transaction native tokens grouping issue ([PR 2594](https://github.com/input-output-hk/daedalus/pull/2594))
- Fixed notes field visibility for software wallets on Share Wallet Address dialog ([PR 2582](https://github.com/input-output-hk/daedalus/pull/2582))

### Chores

- Disabled Fund5 voting registration ([PR 2608](https://github.com/input-output-hk/daedalus/pull/2608))
- Updated Fund5 registaration start date ([PR 2607](https://github.com/input-output-hk/daedalus/pull/2607))
- Removed Jormungandr integration and legacy clusters ([PR 2579](https://github.com/input-output-hk/daedalus/pull/2579))

## 4.1.0

### Features

- Enabled Catalyst voting registration on Mainnet and Flight builds

## 4.1.0-FC1

### Features

- Integrated cardano-wallet Api native tokens decimal places value ([PR 2577](https://github.com/input-output-hk/daedalus/pull/2577))
- Added address verification for "Trezor" hardware wallet devices ([PR 2558](https://github.com/input-output-hk/daedalus/pull/2558))
- Added address verification for "Ledger" hardware wallet devices ([PR 2282](https://github.com/input-output-hk/daedalus/pull/2282))
- Implemented manually configurable decimal places for native tokens ([PR 2533](https://github.com/input-output-hk/daedalus/pull/2533))
- Added warning message when pasting an address of the same wallet in the send form ([PR 2506](https://github.com/input-output-hk/daedalus/pull/2506))
- Implemented select search style variables ([PR 2512](https://github.com/input-output-hk/daedalus/pull/2512))
- Enabled pasting of wallet recovery phrase ([PR 2459](https://github.com/input-output-hk/daedalus/pull/2459))

### Fixes

- Fixed pool description scrolling and styling ([PR 2556](https://github.com/input-output-hk/daedalus/pull/2564), [PR 2568](https://github.com/input-output-hk/daedalus/pull/2568))
- Fixed external currencies not appearing when not connected ([PR 2556](https://github.com/input-output-hk/daedalus/pull/2556))
- Fixed hardware wallets transactions issue with native tokens ([PR 2543](https://github.com/input-output-hk/daedalus/pull/2543))
- Fixed hardware wallets transaction initialization issue on device reconnect ([PR 2541](https://github.com/input-output-hk/daedalus/pull/2541))
- Fixed blank currency selection after updating to 4.0.5 ([PR 2529](https://github.com/input-output-hk/daedalus/pull/2529))
- Fixed hardware wallets confirmation crashing issue when device is in "Busy" state ([PR 2522](https://github.com/input-output-hk/daedalus/pull/2522))
- Fixed hardware wallets spending password issue ([PR 2519](https://github.com/input-output-hk/daedalus/pull/2519))

### Chores

- Updated wallet importer to extract private keys from 'WalletUserSecret' field ([PR 2445](https://github.com/input-output-hk/daedalus/pull/2445))
- Changed the assets identifiers from fingerprint to policyId+name ([PR 2562](https://github.com/input-output-hk/daedalus/pull/2562))
- Implemented the Items Dropdown component and simplified the Wallets and Assets Dropdowns ([PR 2540](https://github.com/input-output-hk/daedalus/pull/2540))
- Included wallet names in dialog windows ([PR 2552](https://github.com/input-output-hk/daedalus/pull/2552))
- Updated `cardano-cli` version to always match the one from `cardano-wallet` ([PR 2561](https://github.com/input-output-hk/daedalus/pull/2561))
- Updated `react-polymorph` to version `1.0.0` ([PR 2549](https://github.com/input-output-hk/daedalus/pull/2549))
- Updated `cardano-wallet` to version `2021-05-26` ([PR 2577](https://github.com/input-output-hk/daedalus/pull/2577), [PR 2580](https://github.com/input-output-hk/daedalus/pull/2580))
- Updated `cardano-wallet` to version `2021-04-28` and `cardano-node` to version `1.26.2` ([PR 2551](https://github.com/input-output-hk/daedalus/pull/2551))
- Updated Catalyst voting registration to include rewards address instead of the payment one ([PR 2550](https://github.com/input-output-hk/daedalus/pull/2550))
- Moved currency related code into a dedicated store ([PR 2546](https://github.com/input-output-hk/daedalus/pull/2546))
- Updated `trezor-connect` dependency to version `8.1.26` ([PR 2534](https://github.com/input-output-hk/daedalus/pull/2534))
- Updated `@cardano-foundation/ledgerjs-hw-app-cardano` package to version `3.0.0` ([PR 2530](https://github.com/input-output-hk/daedalus/pull/2530))

## 4.0.5

### Features

- Added "Voting registration not available" screen ([PR 2518](https://github.com/input-output-hk/daedalus/pull/2518))
- Added Japanese translation to the external currencies list ([PR 2497](https://github.com/input-output-hk/daedalus/pull/2497))

### Fixes

- Fixed stake pool list cutting off items ([PR 2517](https://github.com/input-output-hk/daedalus/pull/2517))
- Fixed fee calculation edge cases in wallet send form ([PR 2501](https://github.com/input-output-hk/daedalus/pull/2501))
- Handle empty strings in transaction metadata correctly ([PR 2503](https://github.com/input-output-hk/daedalus/pull/2503))

### Chores

- Updated header section styles of delegation rewards screen ([PR 2509](https://github.com/input-output-hk/daedalus/pull/2509))
- Reduced the clock drift tolerance to 4,5 seconds ([PR 2510](https://github.com/input-output-hk/daedalus/pull/2510))
- Updates Catalyst Fund4 dates ([PR 2495](https://github.com/input-output-hk/daedalus/pull/2495))
- Updated `cardano-wallet` to revision `7df30796` ([PR 2495](https://github.com/input-output-hk/daedalus/pull/2495))

## 4.0.4

### Features

- Added wallet rewards address display on the "Rewards" screen ([PR 2475](https://github.com/input-output-hk/daedalus/pull/2475))

## 4.0.3

### Features

- Added absolute slot number to Catalyst voting registration transaction metadata ([PR 2476](https://github.com/input-output-hk/daedalus/pull/2476))
- Improved the transactions and rewards CSV export contents ([PR 2451](https://github.com/input-output-hk/daedalus/pull/2451))
- Implement Trezor passphrase handling ([PR 2284](https://github.com/input-output-hk/daedalus/pull/2284))

### Fixes

- Fixed Catalyst voting registration being stuck at confirming transaction step issue ([PR 2482](https://github.com/input-output-hk/daedalus/pull/2482))
- Fixed animation and positioning issues in the progress bar ([PR 2458](https://github.com/input-output-hk/daedalus/pull/2458))
- Fixed dropdown not appearing in the Stake Pools Settings screen ([PR 2460](https://github.com/input-output-hk/daedalus/pull/2460))
- Fixed delay on loading stake pool list after switching the SMASH server ([PR 2447](https://github.com/input-output-hk/daedalus/pull/2447))

### Chores

- Refactored stake pool lists to use our react-polymorph PopOver component ([PR 2373](https://github.com/input-output-hk/daedalus/pull/2373))
- Increased maximum event listeners limit to avoid IPC channel stalling ([PR 2482](https://github.com/input-output-hk/daedalus/pull/2482))
- Updated Catalyst voting registration text copy for Fund4 ([PR 2482](https://github.com/input-output-hk/daedalus/pull/2482))
- Improved reveal / hide action for public key field ([PR 2473](https://github.com/input-output-hk/daedalus/pull/2473))
- Updated lockfile:check and lockfile:fix scripts ([PR 2483](https://github.com/input-output-hk/daedalus/pull/2483))
- Improved hardware wallets public keys exporting while signing transaction witnesses ([PR 2477](https://github.com/input-output-hk/daedalus/pull/2477))
- Updated `trezor-connect` dependency to version `8.1.25` ([PR 2474](https://github.com/input-output-hk/daedalus/pull/2474))
- Improved receive screen's "show used" toggle UX ([PR 2466](https://github.com/input-output-hk/daedalus/pull/2466))
- Updated `iohk-nix` to revision `bc4216c5` ([PR 2462](https://github.com/input-output-hk/daedalus/pull/2462))
- Updated `cardano-wallet` to revision `1fbb9f46` ([PR 2462](https://github.com/input-output-hk/daedalus/pull/2462))
- Re-enabled "Selfnode" environment ([PR 2462](https://github.com/input-output-hk/daedalus/pull/2462))
- Restored global link style on voting registration over screen ([PR 2453](https://github.com/input-output-hk/daedalus/pull/2453), [PR 2456](https://github.com/input-output-hk/daedalus/pull/2456))

## 4.0.2-FC3

### Features

- Enabled sending of native tokens for hardware wallets ([PR 2446](https://github.com/input-output-hk/daedalus/pull/2446))
- Enabled the "View wallet's public key" feature ([PR 2429](https://github.com/input-output-hk/daedalus/pull/2429))
- Added visual backdrop blur effect to some overlays and dialogs ([PR 2431](https://github.com/input-output-hk/daedalus/pull/2431))
- Updated Tile view of stake pools with rank by rewards ([PR 2426](https://github.com/input-output-hk/daedalus/pull/2426))
- Enabled the "Loading Stake Pools" state anytime the pools are loading ([PR 2424](https://github.com/input-output-hk/daedalus/pull/2424))
- Implemented the syncing state for the Stake Pools settings screen ([PR 2418](https://github.com/input-output-hk/daedalus/pull/2418))

### Chores

- Improved error message shown when trying to make a transaction from a wallet that contains only reward ([PR 2450](https://github.com/input-output-hk/daedalus/pull/2450))
- Added configuration variable for native tokens formatted amounts display ([PR 2449](https://github.com/input-output-hk/daedalus/pull/2449))
- Updated `iohk-nix` to revision `60fe72cf` ([PR 2441](https://github.com/input-output-hk/daedalus/pull/2441))
- Updated `cardano-wallet` to version `2021-03-04` ([PR 2427](https://github.com/input-output-hk/daedalus/pull/2427))
- Increased "Report connecting issues" timeout ([PR 2440](https://github.com/input-output-hk/daedalus/pull/2440))
- Removed line break characters on the address from exported PDF ([PR 2402](https://github.com/input-output-hk/daedalus/pull/2402))

## 4.0.1-FC2

### Fixes

- Fixed text overlapping issue on native token input field ([PR 2422](https://github.com/input-output-hk/daedalus/pull/2422))
- Fixed transaction fee calculation error messages handling ([PR 2421](https://github.com/input-output-hk/daedalus/pull/2421))

## 4.0.0-FC1

### Features

- Added native token support [PR 2292](https://github.com/input-output-hk/daedalus/pull/2292)
- Enabled rewards withdrawals on hardware wallets ([PR 2408](https://github.com/input-output-hk/daedalus/pull/2408))

### Fixes

- Fixed InlineEditingInput common component so that wallet name input field works well ([PR 2409](https://github.com/input-output-hk/daedalus/pull/2409))

### Chores

- Moved ITN redemption to Daedalus application menu ([PR 2401](https://github.com/input-output-hk/daedalus/pull/2401))
- Removed unnecessary "socketFile" declaration in launcher-config ([PR 2400](https://github.com/input-output-hk/daedalus/pull/2400))
- Updated delegated stake pool status on delegation center screen ([PR 2404](https://github.com/input-output-hk/daedalus/pull/2404))
- Adjusted hover PopOver styles on the first tile in the delegation center screen ([PR 2386](https://github.com/input-output-hk/daedalus/pull/2386))

## 3.3.2

### Features

- Implemented "Voting registration is over" screen ([PR 2428](https://github.com/input-output-hk/daedalus/pull/2428))

## 3.3.1

### Features

- Implemented "Undelegate wallet" feature on "Wallet settings" screen ([PR 2351](https://github.com/input-output-hk/daedalus/pull/2351))

### Fixes

- Fixed calendar style issue on Filter dialog on transaction list screen ([PR 2387](https://github.com/input-output-hk/daedalus/pull/2387))
- Fixed issue on wallet send form with certain amounts ([PR 2379](https://github.com/input-output-hk/daedalus/pull/2379))

### Chores

- Enabled selecting whole addresses and ids when selecting them to copy on transactions and summary screens ([PR 2370](https://github.com/input-output-hk/daedalus/pull/2370))
- Added missing whitespace between amount and ADA in Japanese ([PR 2380](https://github.com/input-output-hk/daedalus/pull/2380))
- Updated link "Follow instructions and manually update" on testnet and flight ([PR 2372](https://github.com/input-output-hk/daedalus/pull/2372))
- Fixed broken staking stories in Storybook ([PR 2371](https://github.com/input-output-hk/daedalus/pull/2371))

## 3.3.0

### Fixes

- Fixed issue with hardware wallet delegation ([PR 2369](https://github.com/input-output-hk/daedalus/pull/2369))

### Chores

- Updated `@cardano-foundation/ledgerjs-hw-app-cardano` package to version `2.2.0` ([PR 2381](https://github.com/input-output-hk/daedalus/pull/2381))
- Updated `cardano-launcher` to version `0.20210215.0` ([PR 2363](https://github.com/input-output-hk/daedalus/pull/2363))
- Updated `cardano-wallet` to version `2021-02-15` ([PR 2363](https://github.com/input-output-hk/daedalus/pull/2363))
- Updated `cardano-wallet` to version `2021-02-12` ([PR 2358](https://github.com/input-output-hk/daedalus/pull/2358))
- Improved the error messages for the custom SMASH server url input ([PR 2355](https://github.com/input-output-hk/daedalus/pull/2355))

## 3.3.0-FC1

### Features

- Added display of wallet balance in other currencies ([PR 2290](https://github.com/input-output-hk/daedalus/pull/2290))
- Implemented alternate Ledger wallet handling ([PR 2342](https://github.com/input-output-hk/daedalus/pull/2342))
- Re-enabled "Wallet import" feature ([PR 2308](https://github.com/input-output-hk/daedalus/pull/2308))
- Configured "Staking" sidebar icon to always be shown and added a "Staking Syncing" screen to be shown instead of the "Delegation center" until Daedalus fully syncs ([PR 2315](https://github.com/input-output-hk/daedalus/pull/2315))
- Implemented "Voting Center" ([PR 2315](https://github.com/input-output-hk/daedalus/pull/2315), [PR 2353](https://github.com/input-output-hk/daedalus/pull/2353), [PR 2354](https://github.com/input-output-hk/daedalus/pull/2354))
- Implemented transaction metadata display ([PR 2338](https://github.com/input-output-hk/daedalus/pull/2338))
- Displayed fee and deposit info in transaction details and in the delegation wizard ([PR 2339](https://github.com/input-output-hk/daedalus/pull/2339))
- Added SMASH server configuration options ([PR 2259](https://github.com/input-output-hk/daedalus/pull/2259))

### Fixes

- Fixed issues with downloading logs and exporting transaction CSV history on Linux platform
- Fixed an automatic update failure ([PR 2352](https://github.com/input-output-hk/daedalus/pull/2352))
- Fixed logging issue with too few `cardano-wallet` logs being packed into logs zip archive ([PR 2341](https://github.com/input-output-hk/daedalus/pull/2341))
- Fixed misalignment of the "i" icon on the "Set password" dialog ([PR 2337](https://github.com/input-output-hk/daedalus/pull/2337))
- Removed steps counter from the "Success" wallet restoration dialog step ([PR 2335](https://github.com/input-output-hk/daedalus/pull/2335))

### Chores

- Disabled "Voting Center" for Flight builds
- Updated `cardano-wallet` to revision `1ea5e882` ([PR 2356](https://github.com/input-output-hk/daedalus/pull/2356))
- Force public key export on every interaction with hardware wallet device ([PR 2342](https://github.com/input-output-hk/daedalus/pull/2342))
- Updated Hardware Wallets delegation deposit calculation ([PR 2332](https://github.com/input-output-hk/daedalus/pull/2332))
- Implemented dynamic TTL calculation for hardware wallets transactions ([PR 2331](https://github.com/input-output-hk/daedalus/pull/2331))
- Added link to connecting issues support article on the hardware wallet "Pairing" dialog ([PR 2336](https://github.com/input-output-hk/daedalus/pull/2336))
- Updated recovery phrase entry ([PR 2334](https://github.com/input-output-hk/daedalus/pull/2334))
- Adjusted sorting of table values on the "Rewards" screen ([PR 2333](https://github.com/input-output-hk/daedalus/pull/2333))
- Fixed error thrown when closing delegation wizard while transaction fees are being calculated ([PR 2330](https://github.com/input-output-hk/daedalus/pull/2330))
- Fixed number format for syncing percentage and stake pools count ([PR 2313](https://github.com/input-output-hk/daedalus/pull/2313))
- Updated `cardano-wallet` to version `2021-01-28` and `cardano-node` to version `1.25.1` ([PR 2270](https://github.com/input-output-hk/daedalus/pull/2270))
- Updated `react-polymorph` package ([PR 2318](https://github.com/input-output-hk/daedalus/pull/2318))
- Updated `bignumber.js` package ([PR 2305](https://github.com/input-output-hk/daedalus/pull/2305))
- Disabled application menu navigation before the "Terms of use" have been accepted ([PR 2304](https://github.com/input-output-hk/daedalus/pull/2304))

## 3.2.0

### Chores

- Daedalus version change from `3.2.0-FC1` to `3.2.0`

## 3.2.0-FC1

### Features

- Improve Mnemonic Phrase Input UX [PR 2280](https://github.com/input-output-hk/daedalus/pull/2280)
- Added wallet public key viewing feature ([PR 2271](https://github.com/input-output-hk/daedalus/pull/2271))
- Added tile view for delegated Stake pools in Delegation Center ([PR 2275](https://github.com/input-output-hk/daedalus/pull/2275))

### Fixes

- Fixed Ledger TXs to Byron addresses ([PR 2299](https://github.com/input-output-hk/daedalus/pull/2299))

### Chores

- Updated `cardano-wallet` to version `2021-01-12` ([PR 2303](https://github.com/input-output-hk/daedalus/pull/2303))
- Improved "Settings" screen scrolling actions ([PR 2302](https://github.com/input-output-hk/daedalus/pull/2302))
- Improved scrollable dialogs ([PR 2285](https://github.com/input-output-hk/daedalus/pull/2285))
- Disabled and hid copy and paste context menu items on some scenarios ([PR 2300](https://github.com/input-output-hk/daedalus/pull/2300))
- Applied validation to spending password to be not longer than 255 characters ([PR 2287](https://github.com/input-output-hk/daedalus/pull/2287))
- Update `axios` package ([PR 2291](https://github.com/input-output-hk/daedalus/pull/2291))

## 3.1.0

### Features

- Included stake pool ID on the "Confirmation" step of the "Delegation" wizard and enabled search by stake pool ID on the "Stake Pools" screen ([PR 2281](https://github.com/input-output-hk/daedalus/pull/2281))

### Fixes

- Fixed Daedalus development environment issues on OSX Big Sur ([PR 2288](https://github.com/input-output-hk/daedalus/pull/2288))
- Added initial loading state to UTXO screen ([PR 2265](https://github.com/input-output-hk/daedalus/pull/2265))

### Chores

- Canceled running fee estimation api requests once new request is made ([PR 2239](https://github.com/input-output-hk/daedalus/pull/2239))
- Updated `iohk-nix` in order to fix `cardano-node` logging levels ([PR 2283](https://github.com/input-output-hk/daedalus/pull/2283))
- Updated `@cardano-foundation/ledgerjs-hw-app-cardano` package to version `2.1.0` ([PR 2279](https://github.com/input-output-hk/daedalus/pull/2279))
- Updated `ini` package ([PR 2278](https://github.com/input-output-hk/daedalus/pull/2278))

## 3.0.0

### Features

- Added hardware wallet support for "Ledger Nano S", "Ledger Nano X", and "Trezor model T" devices ([PR 2046](https://github.com/input-output-hk/daedalus/pull/2046), [PR 2269](https://github.com/input-output-hk/daedalus/pull/2269))
- Added hardware wallet support for Testnet network ([PR 2264](https://github.com/input-output-hk/daedalus/pull/2264))

### Fixes

- Fixed handling of expired transactions ([PR 2272](https://github.com/input-output-hk/daedalus/pull/2272))
- Fixed visual glitch on the transaction list switching between filters ([PR 2261](https://github.com/input-output-hk/daedalus/pull/2261))

### Chores

- Reduced the size of Linux installer ([PR 2260](https://github.com/input-output-hk/daedalus/pull/2260))
- Bumped `cardano-wallet` to version `2020-12-08` and `cardano-node` to version `1.24.2` ([PR 2270](https://github.com/input-output-hk/daedalus/pull/2270))

## 3.0.0-FC4

### Fixes

- Fixed Trezor transactions TTL ([PR 2046](https://github.com/input-output-hk/daedalus/pull/2046))

## 3.0.0-FC3

### Features

- Added hardware wallet support for "Ledger Nano S" and "Ledger Nano X" on Linux ([PR 2046](https://github.com/input-output-hk/daedalus/pull/2046))

## 3.0.0-FC2

### Features

- Added hardware wallet support for "Ledger Nano S" and "Ledger Nano X" on macOS and Windows ([PR 2046](https://github.com/input-output-hk/daedalus/pull/2046))

## 3.0.0-FC1

### Features

- Added hardware wallet support for "Trezor model T" ([PR 2046](https://github.com/input-output-hk/daedalus/pull/2046))

## 2.6.0

### Features

- Close Newsfeed when clicking anywhere else in the UI ([PR 2250](https://github.com/input-output-hk/daedalus/pull/2250))

### Fixes

- Fixed removal of stake pools which have been delisted on SMASH ([PR 2263](https://github.com/input-output-hk/daedalus/pull/2263))
- Fixed error message and validation on redeem rewards screen ([PR 2220](https://github.com/input-output-hk/daedalus/pull/2220))
- Fixed empty disk space calculation for some Linux distributions ([PR 2258](https://github.com/input-output-hk/daedalus/pull/2258))
- Fixed the overlap of the "X" button in the stake pools search box ([PR 2251](https://github.com/input-output-hk/daedalus/pull/2251))

### Chores

- Improved maximum delegation stake amount calculation ([PR 2262](https://github.com/input-output-hk/daedalus/pull/2262))
- Updated `cardano-wallet` to version `2020-11-26` ([PR 2262](https://github.com/input-output-hk/daedalus/pull/2262))

## 2.5.0

### Features

- Enabled the sharing functionality on the "Receive" screen ([PR 2245](https://github.com/input-output-hk/daedalus/pull/2245))
- Improved form field feedback UX ([PR 2241](https://github.com/input-output-hk/daedalus/pull/2241))
- Implemented "Filter and CSV export" feature on the "Transactions" screen ([PR 2207](https://github.com/input-output-hk/daedalus/pull/2207))

### Fixes

- Fixed visual glitch on the transaction list switching between filters ([PR 2249](https://github.com/input-output-hk/daedalus/pull/2249))
- Fixed Newsfeed drop shadow when there is an update item ([PR 2242](https://github.com/input-output-hk/daedalus/pull/2242))

### Chores

- Implemented Smart Tooltips across whole application ([PR 2243](https://github.com/input-output-hk/daedalus/pull/2243))
- Updated `cardano-wallet` to version `2020-11-17` ([PR 2246](https://github.com/input-output-hk/daedalus/pull/2246))
- Implemented a tool for quickly copying css properties on theme files ([PR 2196](https://github.com/input-output-hk/daedalus/pull/2196))
- Hid hardware wallet restoration support ([PR 2237](https://github.com/input-output-hk/daedalus/pull/2237))

## 2.4.1

### Fixes

- Fixed `leftovers` not being displayed correctly ([PR 2231](https://github.com/input-output-hk/daedalus/pull/2231))
- Fixed prettier/flow linting issue ([PR 2232](https://github.com/input-output-hk/daedalus/pull/2232))

## 2.4.1-FC1

### Features

- Updated stake pool ranking logic to display the same rank for all stake pools without potential rewards ([PR 2227](https://github.com/input-output-hk/daedalus/pull/2227))
- Replaced several tooltips with new react-polymorph `PopOver` component ([PR 2210](https://github.com/input-output-hk/daedalus/pull/2210), [PR 2223](https://github.com/input-output-hk/daedalus/pull/2223))

### Fixes

- Fixed Address validation on Shelley QA network ([PR 2218](https://github.com/input-output-hk/daedalus/pull/2218))
- Vertically centered empty news feed message ([PR 2216](https://github.com/input-output-hk/daedalus/pull/2216))
- Fixed the fix missing space in the App Update Overlay error message ([PR 2219](https://github.com/input-output-hk/daedalus/pull/2219))

## 2.4.0

### Features

- Added tooltips to stake pools table view headers ([PR 2214](https://github.com/input-output-hk/daedalus/pull/2214))
- Added stake pools list view ([PR 2186](https://github.com/input-output-hk/daedalus/pull/2186), [PR 2215](https://github.com/input-output-hk/daedalus/pull/2215))
- Implemented stake pool details explanation tooltips ([PR 2211](https://github.com/input-output-hk/daedalus/pull/2211))
- Re-enabled stake pool saturation info ([PR 2200](https://github.com/input-output-hk/daedalus/pull/2200))
- Implemented the wallet migration leftovers handling ([PR 2178](https://github.com/input-output-hk/daedalus/pull/2178))
- Implemented hiding ranking info for pools without non myopic member rewards on the stake pool tooltip panel ([PR 2209](https://github.com/input-output-hk/daedalus/pull/2209))

### Fixes

- Fixed wrong number formatting on staking screens ([PR 2213](https://github.com/input-output-hk/daedalus/pull/2213))
- Removed broken feature requests link ([PR 2212](https://github.com/input-output-hk/daedalus/pull/2212))
- Fixed animation on wallet dropdown option during redemption process ([PR 2191](https://github.com/input-output-hk/daedalus/pull/2191))
- Fixed Cardano Explorer URLs ([PR 2198](https://github.com/input-output-hk/daedalus/pull/2198))
- Fixed Stake pool tooltip height issue ([PR 2203](https://github.com/input-output-hk/daedalus/pull/2203))

### Chores

- Updated `cardano-launcher` to version `0.20201014.0` ([PR 2205](https://github.com/input-output-hk/daedalus/pull/2205))
- Updated `cardano-wallet` to version `2020-10-13` ([PR 2205](https://github.com/input-output-hk/daedalus/pull/2205))
- Update Nix installation instructions ([PR 2200](https://github.com/input-output-hk/daedalus/pull/2200))
- Updated `cardano-wallet` to revision `be40d5f2` which includes `cardano-node` 1.21.1 ([PR 2200](https://github.com/input-output-hk/daedalus/pull/2200))
- Dependency update ([PR 2190](https://github.com/input-output-hk/daedalus/pull/2190))

## 2.3.0

### Features

- Changed the way the update installer is launched, waiting first for the app to quit ([PR 2195](https://github.com/input-output-hk/daedalus/pull/2195))

## 2.3.0-FC2

### Chores

- Bumped Daedalus version to 2.3.0-FC2

## 2.3.0-FC1

### Features

- Made stake pools ranking slider logarithmic ([PR 2162](https://github.com/input-output-hk/daedalus/pull/2162))
- Implemented fetching of epoch configuration from the Api instead of using hardcoded constants ([PR 2165](https://github.com/input-output-hk/daedalus/pull/2165))
- Enabled SMASH ([PR 2154](https://github.com/input-output-hk/daedalus/pull/2154))

### Fixes

- Fixed the "Retirement" label on Stake Pools for yellow theme ([PR 2189](https://github.com/input-output-hk/daedalus/pull/2189))
- Fixed stake pool tooltip "Delegate to this pool" button background colors ([PR 2181](https://github.com/input-output-hk/daedalus/pull/2181))
- Removed one second delay on changing wallet selection and selecting delegating stake slider value on the "Stake pools" screen ([PR 2180](https://github.com/input-output-hk/daedalus/pull/2180))
- Improved rendering performance of the stake pools on the "Stake pools" screen ([PR 2177](https://github.com/input-output-hk/daedalus/pull/2177))

### Chores

- Updated `cardano-wallet` to version `2020-09-22` ([PR 2188](https://github.com/input-output-hk/daedalus/pull/2188))
- Updated `cardano-wallet` to revision `ffeca1d9` which includes `cardano-node` 1.20.0 ([PR 2187](https://github.com/input-output-hk/daedalus/pull/2187))
- Set "Delegation center" as default staking screen ([PR 2185](https://github.com/input-output-hk/daedalus/pull/2185))
- Changed the ordering of wallet addresses on the "Receive" screen so that the oldest one are on top and the newest one on the bottom of the list ([PR 2176](https://github.com/input-output-hk/daedalus/pull/2176))
- Fixed UI issues ([PR 2152](https://github.com/input-output-hk/daedalus/pull/2152))
- Updated list of team members on About screen ([PR 2167](https://github.com/input-output-hk/daedalus/pull/2167))

## 2.2.0

### Features

- Handled random stake pool data during the first "Shelley" epochs ([PR 2151](https://github.com/input-output-hk/daedalus/pull/2151), [PR 2157](https://github.com/input-output-hk/daedalus/pull/2157))
- Enabled wallet creation regardless of the sync progress ([PR 2150](https://github.com/input-output-hk/daedalus/pull/2150))
- Enabled restoration of "Shelley" wallets regardless of the sync progress ([PR 2150](https://github.com/input-output-hk/daedalus/pull/2150))
- Enabled restoration of Yoroi "Shelley" wallets ([PR 2144](https://github.com/input-output-hk/daedalus/pull/2144))
- Enabled "Send" screen for "Byron" wallets ([PR 2147](https://github.com/input-output-hk/daedalus/pull/2147))
- Added SMASH support ([PR 2143](https://github.com/input-output-hk/daedalus/pull/2143))

### Fixes

- Fixed text copy on "Transfer funds" and "Delegation" wizards ([PR 2153](https://github.com/input-output-hk/daedalus/pull/2153))
- Fixed display priority of Daedalus shutdown messages on the "Loading" screen ([PR 2150](https://github.com/input-output-hk/daedalus/pull/2150))
- Fixed double loading spinner issue on stake pools list page ([PR 2144](https://github.com/input-output-hk/daedalus/pull/2144))
- Fixed the Japanese translation for "Byron" wallet label ([PR 2147](https://github.com/input-output-hk/daedalus/pull/2147))
- Fixed validation of spending passwords which include spaces ([PR 2147](https://github.com/input-output-hk/daedalus/pull/2147))
- Disabled ITN rewards redemption in case Daedalus is not fully in sync ([PR 2146](https://github.com/input-output-hk/daedalus/pull/2146))

### Chores

- Updated `cardano-wallet` to revision `75b583a1` ([PR 2157](https://github.com/input-output-hk/daedalus/pull/2157))
- Updated `cardano-wallet` to revision `a4fd49e3` ([PR 2155](https://github.com/input-output-hk/daedalus/pull/2155))

## 2.1.0

### Features

- ITN rewards redemption ([PR 2133](https://github.com/input-output-hk/daedalus/pull/2133))

### Chores

- Updated `cardano-wallet` to revision `4a4d0a65`
- Updated `cardano-wallet` to revision `d5b43356`

## 2.0.1

### Features

- Disabled creating Byron wallets ([PR 2126](https://github.com/input-output-hk/daedalus/pull/2126))
- Transactions automatically spending rewards ([PR 2131](https://github.com/input-output-hk/daedalus/pull/2131))

### Fixes

- Improved wallet delegation error messages ([PR 2111](https://github.com/input-output-hk/daedalus/pull/2111))
- Fixed Daedalus logo animation on the "Loading" screen ([PR 2124](https://github.com/input-output-hk/daedalus/pull/2124))
- Fixed text copy on the "Delegation center" screen ([PR 2125](https://github.com/input-output-hk/daedalus/pull/2125))
- Stake pools search performance ([PR 2131](https://github.com/input-output-hk/daedalus/pull/2131))

### Chores

- Updated `cardano-wallet` to revision `03b7568b` ([PR 2119](https://github.com/input-output-hk/daedalus/pull/2119))
- Updated `cardano-wallet` to revision `e1890dbd` ([PR 2133](https://github.com/input-output-hk/daedalus/pull/2133))

## 2.0.0

### Features

- Implemented address validation ([PR 2041](https://github.com/input-output-hk/daedalus/pull/2041))
- Show block replay progress ([PR 2110](https://github.com/input-output-hk/daedalus/pull/2110))

### Fixes

- Extended the recovery phrase verification to all possible phrase lengths ([PR 2109](https://github.com/input-output-hk/daedalus/pull/2109))
- Fixed epoch length and slot numbers info ([PR 2108](https://github.com/input-output-hk/daedalus/pull/2108))

## 2.0.0-RC1

### Features

- Implemented Daedalus app automatic update ([PR 2056](https://github.com/input-output-hk/daedalus/pull/2056))

### Fixes

- Fixed the epoch duration copy ([PR 2107](https://github.com/input-output-hk/daedalus/pull/2107))
- Handled errors from a wallet which has ONLY rewards account balance when sending a transaction or delegating ([PR 2100](https://github.com/input-output-hk/daedalus/pull/2100))
- Fixed `cardano-wallet` info shown on the "About" screen ([PR 2097](https://github.com/input-output-hk/daedalus/pull/2097))
- Updated stake pool tooltip to display "Produced blocks" instead of "Blocks (current epoch)" ([PR 2096](https://github.com/input-output-hk/daedalus/pull/2096))
- Fixed wallet recovery phrase length used on "ITN rewards redemption" wizard ([PR 2095](https://github.com/input-output-hk/daedalus/pull/2095))

### Chores

- Updated `cardano-wallet` to revision `ca96c435` ([PR 2105](https://github.com/input-output-hk/daedalus/pull/2105))
- Extended network-parameters Api endpoint integration with hardfork info ([PR 2105](https://github.com/input-output-hk/daedalus/pull/2105))
- Configured mainnet instance for Shelley hardfork ([PR 2104](https://github.com/input-output-hk/daedalus/pull/2104))
- Updated network-info Api endpoint integration ([PR 2101](https://github.com/input-output-hk/daedalus/pull/2101))
- Extended incident and alert newsfeed type ([PR 2099](https://github.com/input-output-hk/daedalus/pull/2099))
- Enable Cardano Explorer URLs for STN network ([PR 2098](https://github.com/input-output-hk/daedalus/pull/2098))

## 1.6.0-STN5

### Features

- Added recovery phrase autocomplete on the wallet creation wizard ([PR 2083](https://github.com/input-output-hk/daedalus/pull/2083))
- Added restoration of 24-word Shelley wallets and 15-word ITN wallets ([PR 2083](https://github.com/input-output-hk/daedalus/pull/2083))
- Added restoration of legacy Byron hardware wallets ([PR 2082](https://github.com/input-output-hk/daedalus/pull/2082))

### Fixes

- Fixed genesis file

### Chores

- Added handlers for new error messages ([PR 2090](https://github.com/input-output-hk/daedalus/pull/2090))
- Updated `cardano-wallet` to revision `71c0b8fd` which includes `cardano-node` 1.18.0

## 1.5.0-STN4

### Chores

- Updated `cardano-wallet` to revision `cb6fae7b` which includes `cardano-node` 1.17.0

## 1.4.1-STN3

### Fixes

- Fixed wallet restoration success message text ([PR 2080](https://github.com/input-output-hk/daedalus/pull/2080))

### Chores

- Re-enabled Byron wallet "Transfer funds" wizard ([PR 2080](https://github.com/input-output-hk/daedalus/pull/2080))
- Updated "Splash" screen text copy ([PR 2080](https://github.com/input-output-hk/daedalus/pull/2080))

## 1.4.0-STN3

### Features

- Added Byron to Shelley transition ([PR 2079](https://github.com/input-output-hk/daedalus/pull/2079))
- Added UX improvements for the display of stake pool ID ([PR 2074](https://github.com/input-output-hk/daedalus/pull/2074))
- Updated application name and network identifiers for the Shelley Testnet Mainnet candidate network ([PR 2078](https://github.com/input-output-hk/daedalus/pull/2078))

### Fixes

- Fixed double punctuation on delegation dialog ([PR 2077](https://github.com/input-output-hk/daedalus/pull/2077))

### Chores

- Updated `cardano-wallet` to revision `c01efcfe` ([PR 2078](https://github.com/input-output-hk/daedalus/pull/2078))

## 1.3.0-STN2

### Features

- Added display of stake pool id on the stake pool tooltip ([PR 2071](https://github.com/input-output-hk/daedalus/pull/2071))
- Added stake pool retirement info ([PR 2068](https://github.com/input-output-hk/daedalus/pull/2068))
- Removed stake pool saturation info ([PR 2067](https://github.com/input-output-hk/daedalus/pull/2067))
- Implemented calculation of wallet's total rewards ([PR 2066](https://github.com/input-output-hk/daedalus/pull/2066))
- Added display of transaction withdrawals within transaction details ([PR 2065](https://github.com/input-output-hk/daedalus/pull/2065))
- Implemented the Redeem Incentivized testnet rewards feature ([PR 2042](https://github.com/input-output-hk/daedalus/pull/2042))
- Ranked stake pools based on delegated amount configured by slider ([PR 2051](https://github.com/input-output-hk/daedalus/pull/2051), [PR 2063](https://github.com/input-output-hk/daedalus/pull/2063), [PR 2072](https://github.com/input-output-hk/daedalus/pull/2072))
- Enabled Byron wallets ([PR 2062](https://github.com/input-output-hk/daedalus/pull/2062))

### Chores

- Removed 'ITN rewards redemption' icon from the sidebar ([PR 2070](https://github.com/input-output-hk/daedalus/pull/2070))
- Updated `cardano-wallet` to revision `c90fe652` which includes `cardano-node` 1.15.1 ([PR 2065](https://github.com/input-output-hk/daedalus/pull/2065))
- Updated `cardano-wallet` to revision `23981129` ([PR 2064](https://github.com/input-output-hk/daedalus/pull/2064))
- Updated `cardano-wallet` to revision `52966c3d` which includes `cardano-node` 1.15.0 ([PR 2062](https://github.com/input-output-hk/daedalus/pull/2062))

## 1.2.0-STN1

### Fixes

- Fixed tooltip position on "Delegation center" and "Rewards" screens ([PR 2049](https://github.com/input-output-hk/daedalus/pull/2049))

## 1.1.0-STN1

### Features

- Integrated react-polymorph `PasswordInput` component ([PR 2038](https://github.com/input-output-hk/daedalus/pull/2038))

### Fixes

- Removed ITN message from the "Create Wallet" dialog in Daedalus Shelley Testnet builds ([PR 2040](https://github.com/input-output-hk/daedalus/pull/2040))
- Fixed transaction list screen showing data outside the component ([PR 2033](https://github.com/input-output-hk/daedalus/pull/2033))

### Chores

- Resolved electron deprecation warnings ([PR 2039](https://github.com/input-output-hk/daedalus/pull/2039))
- Updated the copy for Shelley Testnet ([PR 2037](https://github.com/input-output-hk/daedalus/pull/2037))

## 1.0.0-STN1

### Features

- Implemented a download manager ([PR 2020](https://github.com/input-output-hk/daedalus/pull/2020))
- Implemented Hardware wallets connection screens ([PR 2016](https://github.com/input-output-hk/daedalus/pull/2016))

### Fixes

- Updated the copy to cover additional Friends & Family screens ([PR 2030](https://github.com/input-output-hk/daedalus/pull/2030))
- Fixed landing page rendering issue after initial startup of app ([PR 2032](https://github.com/input-output-hk/daedalus/pull/2032))
- Enabled the Recovery Phrase Verification feature of Shelley wallets on ITN ([PR 2008](https://github.com/input-output-hk/daedalus/pull/2008))
- Disabled button on forms when there is nothing to submit ([PR 1998](https://github.com/input-output-hk/daedalus/pull/1998), [PR 2010](https://github.com/input-output-hk/daedalus/pull/2010))
- Fixed system locale detection ([PR 2009](https://github.com/input-output-hk/daedalus/pull/2009))

### Chores

- Eliminate usage of `UNSAFE_XXX` legacy lifecycle methods ([PR 2028](https://github.com/input-output-hk/daedalus/pull/2028))
- Updated `cardano-wallet` to revision `acc3eb94` ([PR 2027](https://github.com/input-output-hk/daedalus/pull/2027))
- Updated react, react-router, mobx, mobx-react-router and their related package versions ([PR 1966](https://github.com/input-output-hk/daedalus/pull/1966))
- Removed Resync Wallet feature from the codebase ([PR 2024](https://github.com/input-output-hk/daedalus/pull/2024))
- Updated Hardware wallet screens for all themes ([PR 2023](https://github.com/input-output-hk/daedalus/pull/2023))
- Updated `cardano-wallet` to revision `03183595` and updated transaction fee estimation Api endpoint implementation ([PR 2021](https://github.com/input-output-hk/daedalus/pull/2021), [PR 2022](https://github.com/input-output-hk/daedalus/pull/2022))
- Improved wallet import dialogs ([PR 2014](https://github.com/input-output-hk/daedalus/pull/2014))
- Consolidated Japanese translations ([PR 2003](https://github.com/input-output-hk/daedalus/pull/2003))
- Prevented import for more than maximum number of wallets ([PR 2012](https://github.com/input-output-hk/daedalus/pull/2012))
- Implemented automated tests for wallet migration and import features ([PR 2011](https://github.com/input-output-hk/daedalus/pull/2011))

## 1.1.0

### Chores

- Bump Daedalus version ([PR 2015](https://github.com/input-output-hk/daedalus/pull/2015))

## 1.1.0-FC1

### Features

- Integrated the recovery phrase verification and re-enabled "Recovery phrase verification" feature ([PR 1929](https://github.com/input-output-hk/daedalus/pull/1929))

### Fixes

- Fixed Daedalus logo animation display on macOS ([PR 2005](https://github.com/input-output-hk/daedalus/pull/2005))
- Fixed `cardano-wallet` git revision ([PR 1999](https://github.com/input-output-hk/daedalus/pull/1999))
- Added a check which prevents installing Daedalus on unsupported Windows platform versions ([PR 1947](https://github.com/input-output-hk/daedalus/pull/1947))

### Chores

- Bumped Daedalus version to 1.1.0-FC1 ([PR 2006](https://github.com/input-output-hk/daedalus/pull/2006))
- All smart error handlers covered with automated E2E tests([PR 1973](https://github.com/input-output-hk/daedalus/pull/1973))
- Updated `cardano-wallet` to version v2020-04-28 which includes `cardano-node` 1.11.0 ([PR 2004](https://github.com/input-output-hk/daedalus/pull/2004))
- Improved "Delegate/Undelegate" wizard delegation fee calculation logic ([PR 2002](https://github.com/input-output-hk/daedalus/pull/2002))
- Improved "Wallet import" state handling ([PR 2001](https://github.com/input-output-hk/daedalus/pull/2001))

## 1.0.0

### Features

- Implemented NTP force check ([PR 1996](https://github.com/input-output-hk/daedalus/pull/1996))

### Fixes

- Fixed UI issues on wallet import overlays ([PR 1983](https://github.com/input-output-hk/daedalus/pull/1983))

### Chores

- Rename "Daedalus" to "Daedalus Mainnet" and update state directory path ([PR 1986](https://github.com/input-output-hk/daedalus/pull/1986))

## 1.0.0-FC5

### Fixes

- Handle duplicate wallets in import process ([PR 1985](https://github.com/input-output-hk/daedalus/pull/1985), [PR 1989](https://github.com/input-output-hk/daedalus/pull/1989), [PR 1991](https://github.com/input-output-hk/daedalus/pull/1991))
- Treat wallets with 100% syncing progress as synced wallets ([PR 1984](https://github.com/input-output-hk/daedalus/pull/1984))
- Fixed `cardano-node` / `jormungandr` and `cardano-wallet` info on the "Diagnostics" screen ([PR 1980](https://github.com/input-output-hk/daedalus/pull/1980))
- Persist "Blank screen fix" / "--safe-mode" flag between Daedalus restarts ([PR 1979](https://github.com/input-output-hk/daedalus/pull/1979))
- Track `cardano-node` / `jormungandr` PID and use it for safe shutdowns and improve cardano-launcher error handling ([PR 1972](https://github.com/input-output-hk/daedalus/pull/1972))
- Fixed "Wallet import" UI/UX issues ([PR 1968](https://github.com/input-output-hk/daedalus/pull/1968))

### Chores

- Updated Daedalus installer file names to contain only Daedalus version, cluster name and build number ([PR 1971](https://github.com/input-output-hk/daedalus/pull/1971))
- Restore the paragraph about "Automatic wallet migration" on the Daedalus Flight "Splash" screen ([PR 1967](https://github.com/input-output-hk/daedalus/pull/1967))
- Updated `Electron` and it's related dependencies ([PR 1887](https://github.com/input-output-hk/daedalus/pull/1887))

## 1.0.0-FC4

### Fixes

- Fixed active address handling ([PR 1969](https://github.com/input-output-hk/daedalus/pull/1969))

## 1.0.0-FC3

### Features

- Implemented "Wallet Import" feature ([PR 1956](https://github.com/input-output-hk/daedalus/pull/1956))
- Force setting spending password on passwordless wallets ([PR 1957](https://github.com/input-output-hk/daedalus/pull/1957))

### Chores

- Implemented smart error handler ([PR 1962](https://github.com/input-output-hk/daedalus/pull/1962))
- Separated handling of `cardano-wallet` and `cardano-node` logs ([PR 1960](https://github.com/input-output-hk/daedalus/pull/1960))
- Refactored and improved automated tests setup ([PR 1912](https://github.com/input-output-hk/daedalus/pull/1912))
- Updated README ([PR 1953](https://github.com/input-output-hk/daedalus/pull/1953))
- Updated `cardano-wallet` to revision `7140ff08` which includes `cardano-node` 1.10.1 ([PR 1960](https://github.com/input-output-hk/daedalus/pull/1960))
- Updated `cardano-wallet` to revision `745aaad6` with fee estimation fix ([PR 1964](https://github.com/input-output-hk/daedalus/pull/1964))

## 1.0.0-FC2

### Features

- Removed "Syncing" screen ([PR 1952](https://github.com/input-output-hk/daedalus/pull/1952))

### Fixes

- Disabled address generation for Yoroi wallets ([PR 1961](https://github.com/input-output-hk/daedalus/pull/1961))
- Fixed "Restoration" dialog validation ([PR 1951](https://github.com/input-output-hk/daedalus/pull/1951))
- Fixed the `EPERM` errors thrown in the console upon changing wallet name on the "Wallet Settings" screen ([PR 1944](https://github.com/input-output-hk/daedalus/pull/1944))

### Chores

- Removed counter from Alert dialog if we have only one newsfeed alert to show ([PR 1949](https://github.com/input-output-hk/daedalus/pull/1949))
- Integrated react-polymorph ScrollBar CSS vars into each theme ([PR 1827](https://github.com/input-output-hk/daedalus/pull/1827))
- Implemented generation of an Byron wallet address after creating a Byron wallet ([PR 1943](https://github.com/input-output-hk/daedalus/pull/1943))
- Disabled "Force resync" feature for Byron wallets ([PR 1946](https://github.com/input-output-hk/daedalus/pull/1946))

## 1.0.0-FC1

### Features

- Implemented NTP checks ([PR 1925](https://github.com/input-output-hk/daedalus/pull/1925))
- Disabled "Recovery phrase verification" feature ([PR 1931](https://github.com/input-output-hk/daedalus/pull/1931))
- Implemented automated wallet migration ([PR 1905](https://github.com/input-output-hk/daedalus/pull/1905), [PR 1922](https://github.com/input-output-hk/daedalus/pull/1922), [PR 1930](https://github.com/input-output-hk/daedalus/pull/1930), [PR 1935](https://github.com/input-output-hk/daedalus/pull/1935), ([PR 1936](https://github.com/input-output-hk/daedalus/pull/1936))
- Integrated network clock api endpoint ([PR 1918](https://github.com/input-output-hk/daedalus/pull/1918))
- Implements the Flight release information window ([PR 1917](https://github.com/input-output-hk/daedalus/pull/1917))
- Added new icon for Daedalus Flight ([PR 1909](https://github.com/input-output-hk/daedalus/pull/1909))
- Integrated network parameters api endpoint ([PR 1915](https://github.com/input-output-hk/daedalus/pull/1915))
- Implemented flight candidates changes ([PR 1908](https://github.com/input-output-hk/daedalus/pull/1908))
- Implemented new "Flight candidate" theme ([PR 1914](https://github.com/input-output-hk/daedalus/pull/1914))
- Implemented non-responding wallets handling ([PR 1901](https://github.com/input-output-hk/daedalus/pull/1901))
- Added support for building both `cardano-node` and `jormungandr` installers ([PR 1894](https://github.com/input-output-hk/daedalus/pull/1894))
- Added support for `cardano-node` "Selfnode" network ([PR 1897](https://github.com/input-output-hk/daedalus/pull/1897))
- Enabled Byron wallet creation for Haskell node builds ([PR 1895](https://github.com/input-output-hk/daedalus/pull/1895))
- Enabled all Mainnet Daedalus features for Byron wallets for Haskell node builds ([PR 1895](https://github.com/input-output-hk/daedalus/pull/1895))
- Enabled changing spending password in "Wallet settings" for Haskell node builds ([PR 1902](https://github.com/input-output-hk/daedalus/pull/1902))
- Enabled "Send" feature for Haskell node builds ([PR 1902](https://github.com/input-output-hk/daedalus/pull/1902))
- Disabled transfer funds notification for Haskell node builds ([PR 1902](https://github.com/input-output-hk/daedalus/pull/1902))
- Disabled Shelley wallets and delegation features for Haskell node builds ([PR 1895](https://github.com/input-output-hk/daedalus/pull/1895))
- Disabled "Data layer migration" notification for Haskell node builds ([PR 1895](https://github.com/input-output-hk/daedalus/pull/1895))
- Disabled "Paper wallet certificate" creation for Haskell node builds ([PR 1895](https://github.com/input-output-hk/daedalus/pull/1895))
- Removed hardware and Shelley wallet restoration options for Haskel node builds ([PR 1895](https://github.com/input-output-hk/daedalus/pull/1895))
- Integrated new `cardano-launcher` ([PR 1886](https://github.com/input-output-hk/daedalus/pull/1886))

### Fixes

- Fixed Cardano Node version shown on the About screen ([PR 1941](https://github.com/input-output-hk/daedalus/pull/1941))
- Fixed UI issues across all themes ([PR 1916](https://github.com/input-output-hk/daedalus/pull/1916))
- Removed the click and mark from spinner component ([PR 1885](https://github.com/input-output-hk/daedalus/pull/1885))
- Removed locale specific rules from the CSS files ([PR 1871](https://github.com/input-output-hk/daedalus/pull/1871))

### Chores

- Added new "Terms of use" ([PR 1934](https://github.com/input-output-hk/daedalus/pull/1934))
- Updated About screen ([PR 1928](https://github.com/input-output-hk/daedalus/pull/1928))
- Configured Daedalus Flight to use it's own newsfeed ([PR 1927](https://github.com/input-output-hk/daedalus/pull/1927))
- Reduced "connection timeout limit" for Byron Reboot builds to 5 minutes ([PR 1926](https://github.com/input-output-hk/daedalus/pull/1926))
- Updated Daedalus name and version for the Daedalus Flight 1.0.0-FC1 release ([PR 1910](https://github.com/input-output-hk/daedalus/pull/1910))
- Enabled "Wallet UTXO distribution" feature for Byron wallets([PR 1913](https://github.com/input-output-hk/daedalus/pull/1913))
- Enabled Byron wallet name editing ([PR 1911](https://github.com/input-output-hk/daedalus/pull/1911))
- Updated UI copy ([PR 1907](https://github.com/input-output-hk/daedalus/pull/1907))
- Updated Byron Haskell address validation ([PR 1902](https://github.com/input-output-hk/daedalus/pull/1902))
- Updated test configuration to cover Byron features only ([PR 1895](https://github.com/input-output-hk/daedalus/pull/1895))
- Optimized e2e tests ([PR 1874](https://github.com/input-output-hk/daedalus/pull/1874))
- Updated `react-polymorph` dependency ([PR 1882](https://github.com/input-output-hk/daedalus/pull/1882))
- Updated small 3rd party dependencies ([PR 1877](https://github.com/input-output-hk/daedalus/pull/1877), [PR 1924](https://github.com/input-output-hk/daedalus/pull/1924))
- Updated React dependencies ([PR 1873](https://github.com/input-output-hk/daedalus/pull/1873))
- Updated Storybook dependencies ([PR 1873](https://github.com/input-output-hk/daedalus/pull/1873))
- Updated node and yarn dependencies ([PR 1883](https://github.com/input-output-hk/daedalus/pull/1883))
- Re-enabled theme selection on the "Settings" screen ([PR 1872](https://github.com/input-output-hk/daedalus/pull/1872))
- Implemented acceptance tests for custom number, date and time formats ([PR 1868](https://github.com/input-output-hk/daedalus/pull/1868))

## 2.3.1-ITN1

### Fixes

- Fixed stake pool rank coloring within the stake pool tooltip ([PR 1891](https://github.com/input-output-hk/daedalus/pull/1891))
- Fixed missing notarization on macOS installers ([PR 1890](https://github.com/input-output-hk/daedalus/pull/1890))
- Fixed issues with loading stake pools on the "Stake pools" screen ([PR 1888](https://github.com/input-output-hk/daedalus/pull/1888))

### Chores

- Updated `cardano-wallet` to version `2020-03-16` which includes Jormungandr 0.8.14 ([PR 1888](https://github.com/input-output-hk/daedalus/pull/1888))

## 2.3.0-ITN1

### Fixes

- Fixed "Decentralization countdown" Storybook story ([PR 1863](https://github.com/input-output-hk/daedalus/pull/1863))

### Chores

- Implemented acceptance tests for "Rewards" screen ([PR 1861](https://github.com/input-output-hk/daedalus/pull/1861))
- Updated test environment dependencies ([PR 1867](https://github.com/input-output-hk/daedalus/pull/1867))
- Updated `Flow` and `ESLint` dependencies ([PR 1866](https://github.com/input-output-hk/daedalus/pull/1866))
- Updated `husky` dependency ([PR 1865](https://github.com/input-output-hk/daedalus/pull/1865))
- Updated `stylelint` and `stylelint-order` dependencies ([PR 1864](https://github.com/input-output-hk/daedalus/pull/1864))
- Updated `cardano-wallet` to version `2020-03-03` which includes Jormungandr 0.8.13 ([PR 1870](https://github.com/input-output-hk/daedalus/pull/1870))

## 2.2.0-ITN1

### Features

- Added "block-0" to the installers in order to speed up the initial bootstrap phase ([PR 1857](https://github.com/input-output-hk/daedalus/pull/1857))
- Implemented transactions filtering dialog on wallet "Transactions" screen ([PR 1815](https://github.com/input-output-hk/daedalus/pull/1815))

### Fixes

- Fixed pending delegation preferences handling ([PR 1856](https://github.com/input-output-hk/daedalus/pull/1856))

### Chores

- Implemented unit tests for scrambling and unscrambling mnemonics ([PR 1849](https://github.com/input-output-hk/daedalus/pull/1849))
- Implemented acceptance tests for wallet delegation ([PR 1814](https://github.com/input-output-hk/daedalus/pull/1814))
- Improved loading state of the "Stake pools" screen ([PR 1814](https://github.com/input-output-hk/daedalus/pull/1814))
- Updated `cardano-wallet` to version `2020-02-17` ([PR 1856](https://github.com/input-output-hk/daedalus/pull/1856))
- Updated `cardano-wallet` to revision `573a7038` ([PR 1862](https://github.com/input-output-hk/daedalus/pull/1862))

## 2.1.0-ITN1

### Features

- Added stake pools saturation info and ordering based on desirability ([PR 1826](https://github.com/input-output-hk/daedalus/pull/1826))
- Implemented "Resync wallet" feature ([PR 1822](https://github.com/input-output-hk/daedalus/pull/1822))
- Implemented "Hardware wallets" restoration ([PR 1801](https://github.com/input-output-hk/daedalus/pull/1801))
- Implemented "Yoroi wallets" restoration ([PR 1740](https://github.com/input-output-hk/daedalus/pull/1740))
- Implemented new menu shortcuts ([PR 1780](https://github.com/input-output-hk/daedalus/pull/1780))
- Implemented React-Polymorph "Link" component ([PR 1799](https://github.com/input-output-hk/daedalus/pull/1799))
- Implemented a spinner on Wallet delegation screens for wallets in the restoration process ([PR 1847](https://github.com/input-output-hk/daedalus/pull/1847))
- Implemented experimental data UI ([PR 1845](https://github.com/input-output-hk/daedalus/pull/1845), [PR 1850](https://github.com/input-output-hk/daedalus/pull/1850))
- Implemented pending delegation preferences ([PR 1842](https://github.com/input-output-hk/daedalus/pull/1842))
- Renamed "Profit margin" into "Pool margin" on the stake pool tooltip ([PR 1841](https://github.com/input-output-hk/daedalus/pull/1841))

### Fixes

- Fixed a routing issue after wallet deletion ([PR 1823](https://github.com/input-output-hk/daedalus/pull/1823))
- Fixed a typo on the "Staking pools" screen ([PR 1785](https://github.com/input-output-hk/daedalus/pull/1785))
- Fixed a typo in the Daedalus ITN "Terms of Service" ([PR 1809](https://github.com/input-output-hk/daedalus/pull/1809))
- Fixed handling of duplicated wallet IDs when restoring Yoroi Balance and Rewards wallets from the same wallet recovery phrase ([PR 1805](https://github.com/input-output-hk/daedalus/pull/1805))
- Fixed stake pool descriptions text clipping on stake pool tooltip ([PR 1832](https://github.com/input-output-hk/daedalus/pull/1832))
- Fixed "Low disk space" notification not being shown for Incentivized testnet ([PR 1833](https://github.com/input-output-hk/daedalus/pull/1833))
- Fixed download logs link underline color ([PR 1831](https://github.com/input-output-hk/daedalus/pull/1831))

### Chores

- Improved notification display ([PR 1748](https://github.com/input-output-hk/daedalus/pull/1748))
- Improved delete wallet text copy ([PR 1819](https://github.com/input-output-hk/daedalus/pull/1819))
- Improved the paper wallet recovery phrase validation ([PR 1818](https://github.com/input-output-hk/daedalus/pull/1818))
- Improved network screen with responsive main copy box ([PR 1797](https://github.com/input-output-hk/daedalus/pull/1797))
- Updated checkboxes, radio buttons and switchers sizes and borders ([PR 1793](https://github.com/input-output-hk/daedalus/pull/1793))
- Updated `cardano-wallet` to revision `b89cfa19` which includes Jormungandr 0.8.9 ([PR 1834](https://github.com/input-output-hk/daedalus/pull/1834))
- Updated `cardano-wallet` to revision `23e12d1a` which includes Jormungandr 0.8.7 ([PR 1828](https://github.com/input-output-hk/daedalus/pull/1828))
- Updated `cardano-wallet` to revision `d188a5fc` ([PR 1825](https://github.com/input-output-hk/daedalus/pull/1825))
- Updated `cardano-wallet` to revision `e6316404` ([PR 1826](https://github.com/input-output-hk/daedalus/pull/1826))
- Updated `cardano-wallet` to revision `254575e4` which includes Jormungandr 0.8.6 ([PR 1821](https://github.com/input-output-hk/daedalus/pull/1821))
- Updated `cardano-wallet` to revision `132a5faf` ([PR 1740](https://github.com/input-output-hk/daedalus/pull/1740))
- Improved GitHub pull request template ([PR 1843](https://github.com/input-output-hk/daedalus/pull/1843))
- Removed unused locales and translation files ([PR 1840](https://github.com/input-output-hk/daedalus/pull/1840))
- Improved acceptance tests setup with "rerun" feature ([PR 1835](https://github.com/input-output-hk/daedalus/pull/1835)
- Implemented acceptance tests for Daedalus Balance wallets ([PR 1816](https://github.com/input-output-hk/daedalus/pull/1816), [PR 1828](https://github.com/input-output-hk/daedalus/pull/1828))
- Implemented acceptance tests for stake pools loading ([PR 1820](https://github.com/input-output-hk/daedalus/pull/1820))
- Bumped cardano-wallet dependecy to 4ea622c694768bf61bd5c9d04a6e59fe1de3fd53 ([PR 1851](https://github.com/input-output-hk/daedalus/pull/1851))

## 2.0.0-ITN1

### Features

- Added stake pool metadata registry for "SelfNode" network ([PR 1771](https://github.com/input-output-hk/daedalus/pull/1771/))
- Added list stake pools API endpoint errors handlers ([PR 1765](https://github.com/input-output-hk/daedalus/pull/1765))
- Added "Terms of use" for the Incentivized Testnet v1 - Rewards network ([PR 1741](https://github.com/input-output-hk/daedalus/pull/1741))
- Updated stake pool ranking logic to use `apparent_performance` from the Api response ([PR 1757](https://github.com/input-output-hk/daedalus/pull/1757))
- Integrated V2 API endpoint for join/quit fee estimation ([PR 1752](https://github.com/input-output-hk/daedalus/pull/1752))
- Integrated stake pool join V2 API endpoint and "Delegation" wizard updated ([PR 1744](https://github.com/input-output-hk/daedalus/pull/1744))
- Integrated the V2 API endpoint for fetching stake pool data ([PR 1733](https://github.com/input-output-hk/daedalus/pull/1733))
- Integrated stake pool Quit V2 API endpoint and added "Undelegate" dialog ([PR 1737](https://github.com/input-output-hk/daedalus/pull/1737))
- Added support for new number formats to the React-Polymorph "Numeric input" on the "Send" screen ([PR 1735](https://github.com/input-output-hk/daedalus/pull/1735))
- Added script which checks for "integrity" lines in yarn.lock file ([PR 1715](https://github.com/input-output-hk/daedalus/pull/1715))
- Added new Mainnet and Testnet icons ([PR 1716](https://github.com/input-output-hk/daedalus/pull/1716))
- Added number of pending transactions to “Wallet Summary” screen ([PR 1705](https://github.com/input-output-hk/daedalus/pull/1705))

### Fixes

- Fixed undelegation issue ([PR 1774](https://github.com/input-output-hk/daedalus/pull/1774))
- Fixed the display of headlines on the "Network" splash screen ([PR 1770](https://github.com/input-output-hk/daedalus/pull/1770))
- Fixed spending password error handling ([PR 1767](https://github.com/input-output-hk/daedalus/pull/1767))
- Fixed an issue where "Balance" wallet notification wasn't offering an option to create a "Rewards" wallet in case none are available ([PR 1761](https://github.com/input-output-hk/daedalus/pull/1761))
- Fixed the middle ellipsis for different addresses lengths ([PR 1736](https://github.com/input-output-hk/daedalus/pull/1736))
- Fixed missing "hamburger" icon on wallet menu while there are only legacy wallets in the UI ([PR 1730](https://github.com/input-output-hk/daedalus/pull/1730))
- Fixed issues with wrong transaction amounts when new number formats are used ([PR 1726](https://github.com/input-output-hk/daedalus/pull/1726))
- Fixed naming of labels on "Daedalus Diagnostics" screen and status icons on "Loading" screen from "Node..." to "Cardano node..." ([PR 1723](https://github.com/input-output-hk/daedalus/pull/1723))
- Adjusted the Stake pool logic for ranking color to account for the total number of stake pools ([PR 1719](https://github.com/input-output-hk/daedalus/pull/1719))
- Fixed styling issues on the "Network info" overlay on all the themes ([PR 1708](https://github.com/input-output-hk/daedalus/pull/1708))
- Fixed read newsfeed items ID duplication in local storage ([PR 1710](https://github.com/input-output-hk/daedalus/pull/1710))
- Fixed the `themes:check:createTheme` script and updated the `createTheme` object ([PR 1709](https://github.com/input-output-hk/daedalus/pull/1709))
- Fixed disabled button background color on all of the themes ([PR 1707](https://github.com/input-output-hk/daedalus/pull/1707))
- Fixed select language e2e tests ([PR 1702](https://github.com/input-output-hk/daedalus/pull/1702))
- Fixed yellow frame around the "Diagnostics" dialog ([PR 1699](https://github.com/input-output-hk/daedalus/pull/1699))
- Fixed the Japanese translation for `timeAgo` in "Wallet settings" stories ([PR 1701](https://github.com/input-output-hk/daedalus/pull/1701))

### Chores

- Updated newsfeed ([PR 1786](https://github.com/input-output-hk/daedalus/pull/1786))
- Changed Incentivized TestNet "Having trouble syncing" URL to point to one specific to TestNet ([PR 1795](https://github.com/input-output-hk/daedalus/pull/1795))
- Updated `cardano-wallet` to revision `d3d93ba3` ([PR 1784](https://github.com/input-output-hk/daedalus/pull/1784))
- Updated `cardano-wallet` to revision `e341d288` ([PR 1779](https://github.com/input-output-hk/daedalus/pull/1779))
- Reduced stake pools fetching interval to 1 minute ([PR 1779](https://github.com/input-output-hk/daedalus/pull/1779))
- Updated `cardano-wallet` to version `2019.12.13` ([PR 1775](https://github.com/input-output-hk/daedalus/pull/1775))
- Updated stake pool details tooltip with profit margin & cost ([PR 1773](https://github.com/input-output-hk/daedalus/pull/1773), [PR 1779](https://github.com/input-output-hk/daedalus/pull/1779))
- Updated `react-polymorph` to version `0.9.0-rc.26` ([PR 1772](https://github.com/input-output-hk/daedalus/pull/1772))
- Updated wallet delegation / undelegation ([PR 1766](https://github.com/input-output-hk/daedalus/pull/1766))
- Updated `webpack` package to version `4.39.1` ([PR 1769](https://github.com/input-output-hk/daedalus/pull/1769))
- Improved "Delegration Center" epoch countdown to use epoch info from the Api ([PR 1751](https://github.com/input-output-hk/daedalus/pull/1751))
- Updated `cardano-wallet` to revision `555b5e82` ([PR 1764](https://github.com/input-output-hk/daedalus/pull/1764))
- Updated rewards screen note and learn more button style ([PR 1760](https://github.com/input-output-hk/daedalus/pull/1760))
- Updated message for current stake pool being selected ([PR 1758](https://github.com/input-output-hk/daedalus/pull/1758))
- Updated "Delegation" UI Support portal article URLs ([PR 1759](https://github.com/input-output-hk/daedalus/pull/1759))
- Updated `serialize-javascript` package dependency ([PR 1756](https://github.com/input-output-hk/daedalus/pull/1756))
- Updated minimum amount of ada for delegation to be available ([PR 1753](https://github.com/input-output-hk/daedalus/pull/1753))
- Enabled "Delegation" UI in Daedalus builds ([PR 1750](https://github.com/input-output-hk/daedalus/pull/1750))
- Updated `cardano-wallet` to revision `d4571952` which includes Jormungandr 0.8.0-rc8 ([PR 1749](https://github.com/input-output-hk/daedalus/pull/1749))
- Improved the unit and e2e test setup ([PR 1743](https://github.com/input-output-hk/daedalus/pull/1743))
- Updated to react-polymorph@0.9.0-rc.25 which includes a theme var for checkbox icon color (([PR 1742](https://github.com/input-output-hk/daedalus/pull/1742)))
- Updated `cardano-wallet` to revision `833f9d4e` which includes Jormungandr 0.8.0-rc7 ([PR 1739](https://github.com/input-output-hk/daedalus/pull/1739))
- Updated stake header info component and adds new information with the countdown timer for next epoch ([PR 1729](https://github.com/input-output-hk/daedalus/pull/1729))
- Removed "StakePool" from Wallet domain ([PR 1738](https://github.com/input-output-hk/daedalus/pull/1738))
- Refactored wallet navigation to use new React-Polymorph Dropdown component ([PR 1593](https://github.com/input-output-hk/daedalus/pull/1593))
- Improved error messages on the "Send" screen ([PR 1724](https://github.com/input-output-hk/daedalus/pull/1724))
- Improved menu items while adding external link icon on menu items which open external links ([PR 1727](https://github.com/input-output-hk/daedalus/pull/1727), [PR 1728](https://github.com/input-output-hk/daedalus/pull/1728))
- Updated `cardano-js` package to version 0.2.2 and improved address validation ([PR 1712](https://github.com/input-output-hk/daedalus/pull/1712))
- Removed unused dependencies ([PR 1706](https://github.com/input-output-hk/daedalus/pull/1706))
- Improved the error message shown on the "Change password" dialog when too short current password is submitted ([PR 1703](https://github.com/input-output-hk/daedalus/pull/1703))

## 1.1.0-ITN0

### Features

- Implemented the new Wallet Receive screen ([PR 1700](https://github.com/input-output-hk/daedalus/pull/1700))
- Added Japanese "Terms of use" text ([PR 1691](https://github.com/input-output-hk/daedalus/pull/1691))

### Fixes

- Fixed Wallet name length UI issues on smaller screen sizes ([PR 1689](https://github.com/input-output-hk/daedalus/pull/1689))
- Fixed "Number of words in your recovery phrase" default state on "Restore a wallet" dialog ([PR 1692](https://github.com/input-output-hk/daedalus/pull/1692))
- Fixed "Verify wallet recovery phrase" button text vertical centering ([PR 1693](https://github.com/input-output-hk/daedalus/pull/1693))
- Disabled "Latest version check" call and fixed "Legacy" wallet ordering after wallet restoration ([PR 1690](https://github.com/input-output-hk/daedalus/pull/1690))
- Increased Sync and Report sync-issue threshold to 10 mins ([PR 1697](https://github.com/input-output-hk/daedalus/pull/1697))

### Chores

- Bumped Daedalus version to "1.1.0-ITN0" ([PR 1695](https://github.com/input-output-hk/daedalus/pull/1695))

## 1.0.0-ITN0

### Features

- Added the Cardano Explorer URL's for ITN ([PR 1674](https://github.com/input-output-hk/daedalus/pull/1674))
- Hide legacy wallet notification during wallet restoration / syncing ([PR 1667](https://github.com/input-output-hk/daedalus/pull/1667))
- Implemented the necessary UI changes for the Incentivized Testnet network ([PR 1657](https://github.com/input-output-hk/daedalus/pull/1657))
- Implemented syncing and connecting screens for the Incentivized Testnet network ([PR 1673](https://github.com/input-output-hk/daedalus/pull/1673))
- Implemented cancel pending transaction V2 API endpoint and UI for legacy wallets ([PR 1651](https://github.com/input-output-hk/daedalus/pull/1651))
- Implemented cancel pending transaction V2 API endpoint and UI ([PR 1633](https://github.com/input-output-hk/daedalus/pull/1633))
- Implemented "Transfer funds" wizard for Incentivized Testnet version of Daedalus ([PR 1634](https://github.com/input-output-hk/daedalus/pull/1634), [PR 1659](https://github.com/input-output-hk/daedalus/pull/1659), [PR 1660](https://github.com/input-output-hk/daedalus/pull/1660))
- Implemented "Network info" overlay ([PR 1655](https://github.com/input-output-hk/daedalus/pull/1655), [PR 1676](https://github.com/input-output-hk/daedalus/pull/1676))
- Disable "Manual update" notification for Incentivized Testnet version of Daedalus ([PR 1652](https://github.com/input-output-hk/daedalus/pull/1652))
- Update rewards screen for incentivized testnet ([PR 1643](https://github.com/input-output-hk/daedalus/pull/1643))
- Implement restoration of both 12 and 15 mnemonic words phrases in Wallet Restore dialog ([PR 1629](https://github.com/input-output-hk/daedalus/pull/1629))
- Replace sidebar "Bug-report" icon with a "Network" badge ([PR 1622](https://github.com/input-output-hk/daedalus/pull/1622))
- Implemented "Spending password" as required parameter for Cardano V2 API ([PR 1631](https://github.com/input-output-hk/daedalus/pull/1631))
- Implemented "Incentivized Testnet" theme for Incentivized Testnet version of Daedalus ([PR 1620](https://github.com/input-output-hk/daedalus/pull/1620))
- Removed "Decentralization countdown", "Decentralization info", and "Staking epochs" screens for Incentivized Testnet Daedalus version ([PR 1625](https://github.com/input-output-hk/daedalus/pull/1625))
- Implemented address validator ([PR 1609](https://github.com/input-output-hk/daedalus/pull/1609), [PR 1618](https://github.com/input-output-hk/daedalus/pull/1618))
- Add "frontend-only" mode utility that has no dependency on nix ([PR 1583](https://github.com/input-output-hk/daedalus/pull/1583))
- Added Jormungandr support for Cardano V2 API ([PR 1567](https://github.com/input-output-hk/daedalus/pull/1567)
- Integrated Cardano V2 API endpoints ([PR 1548](https://github.com/input-output-hk/daedalus/pull/1548), [PR 1551](https://github.com/input-output-hk/daedalus/pull/1551), [PR 1552](https://github.com/input-output-hk/daedalus/pull/1552), [PR 1553](https://github.com/input-output-hk/daedalus/pull/1553), [PR 1555](https://github.com/input-output-hk/daedalus/pull/1555), [PR 1556](https://github.com/input-output-hk/daedalus/pull/1556), [PR 1557](https://github.com/input-output-hk/daedalus/pull/1557), [PR 1558](https://github.com/input-output-hk/daedalus/pull/1558), [PR 1559](https://github.com/input-output-hk/daedalus/pull/1559), [PR 1560](https://github.com/input-output-hk/daedalus/pull/1560), [PR 1575](https://github.com/input-output-hk/daedalus/pull/1575), [PR 1577](https://github.com/input-output-hk/daedalus/pull/1577), [PR 1579](https://github.com/input-output-hk/daedalus/pull/1579), [PR 1604](https://github.com/input-output-hk/daedalus/pull/1604), [PR 1613](https://github.com/input-output-hk/daedalus/pull/1613), [PR 1637](https://github.com/input-output-hk/daedalus/pull/1637), [PR 1639](https://github.com/input-output-hk/daedalus/pull/1639), [PR 1641](https://github.com/input-output-hk/daedalus/pull/1641))
- Add internal link support in newsfeed items and verification hash generator script ([PR 1617](https://github.com/input-output-hk/daedalus/pull/1617))
- Implemented date, time and number format user options ([PR 1611](https://github.com/input-output-hk/daedalus/pull/1611))

### Chores

- Daedalus copy updates for Incentivized Testnet - Balance check ([PR 1680](https://github.com/input-output-hk/daedalus/pull/1680))
- Added "Terms of use" for Incentivized Testnet version of Daedalus ([PR 1664](https://github.com/input-output-hk/daedalus/pull/1664))
- Added Legacy Wallet UI changes ([PR 1647](https://github.com/input-output-hk/daedalus/pull/1647))
- Removed wallet recovery phrase verification feature for Incentivized Testnet version of Daedalus ([PR 1645](https://github.com/input-output-hk/daedalus/pull/1645))
- Changed wallet restoration notification message ([PR 1644](https://github.com/input-output-hk/daedalus/pull/1644))
- Removed parallel wallet restoration limitation ([PR 1638](https://github.com/input-output-hk/daedalus/pull/1638))
- Disabled create a paper wallet certificate feature ([PR 1640](https://github.com/input-output-hk/daedalus/pull/1640))
- Removed all notions of account indexes from the codebase ([PR 1614](https://github.com/input-output-hk/daedalus/pull/1614))
- Removed "Block consolidation status" dialog ([PR 1610](https://github.com/input-output-hk/daedalus/pull/1610))
- Fixed build mode of webpack auto dll plugin ([PR 1606](https://github.com/input-output-hk/daedalus/pull/1606))
- Changed delete wallet button layout for emphasized location/importance and removed export wallet feature ([PR 1612](https://github.com/input-output-hk/daedalus/pull/1612), [PR 1619](https://github.com/input-output-hk/daedalus/pull/1619))
- Speedup storybook builds in development ([PR 1607](https://github.com/input-output-hk/daedalus/pull/1607))
- Added note to UTXO screen showing pending transactions ([PR 1589](https://github.com/input-output-hk/daedalus/pull/1589))
- Fixed broken source maps ([PR 1594](https://github.com/input-output-hk/daedalus/pull/1594))
- Reorganized Storybook by domain ([PR 1537](https://github.com/input-output-hk/daedalus/pull/1537))
- Reorganized Tests by domain ([PR 1540](https://github.com/input-output-hk/daedalus/pull/1540))

### Fixes

- Fixed incentivized testnet theme ([PR 1677](https://github.com/input-output-hk/daedalus/pull/1677), [PR 1684](https://github.com/input-output-hk/daedalus/pull/1684))
- Fixed wrong "Jormugandr" process name ([PR 1669](https://github.com/input-output-hk/daedalus/pull/1669))
- Fixed paper wallet certificate restoration ([PR 1055](https://github.com/input-output-hk/daedalus/pull/1055))
- Reduce layout re-renderings ([PR 1595](https://github.com/input-output-hk/daedalus/pull/1595))
- Fixed green Cardano theme white color and borders color ([PR 1584](https://github.com/input-output-hk/daedalus/pull/1584))
- Fixed flat button color ([PR 1586](https://github.com/input-output-hk/daedalus/pull/1586))

## 0.15.1

### Fixes

- Fixed newsfeed content loading issue ([PR 1653](https://github.com/input-output-hk/daedalus/pull/1653))

## 0.15.0

### Features

- Implemented "Newsfeed" feature ([PR 1570](https://github.com/input-output-hk/daedalus/pull/1570))
- Implemented "Wallet recovery phrase verification" feature ([PR 1565](https://github.com/input-output-hk/daedalus/pull/1565))
- Implemented new "Automated update" notification design ([PR 1491](https://github.com/input-output-hk/daedalus/pull/1491))
- Improved "Wallets" list scrollbar UX ([PR 1475](https://github.com/input-output-hk/daedalus/pull/1475))
- Removed "Ada Redemption" feature ([PR 1510](https://github.com/input-output-hk/daedalus/pull/1510))
- Updated behavior of system dialogs ([PR 1494](https://github.com/input-output-hk/daedalus/pull/1494))

### Fixes

- Fixed the "Numeric input" component caret positioning issues ([PR 1511](https://github.com/input-output-hk/daedalus/pull/1511))
- Fixed the "Feature Request" menu item Japanese translation ([PR 1533](https://github.com/input-output-hk/daedalus/pull/1533))
- Implemented disk space check error handling ([PR 1562](https://github.com/input-output-hk/daedalus/pull/1562))
- Implemented platform specific main window minimum heights ([PR 1485](https://github.com/input-output-hk/daedalus/pull/1485))
- Implemented pluralization for number of confirmations in transaction details ([PR 1531](https://github.com/input-output-hk/daedalus/pull/1531))
- Minor UI fixes:
  - Fixed external and copy icons positioning inconsistencies ([PR 1512](https://github.com/input-output-hk/daedalus/pull/1512))
  - Fixed minor UI issues across different themes ([PR 1547](https://github.com/input-output-hk/daedalus/pull/1547))
  - Fixed "White" theme styling ([PR 1532](https://github.com/input-output-hk/daedalus/pull/1532))
  - Removed select dropdown arrow ([PR 1550](https://github.com/input-output-hk/daedalus/pull/1550))
  - Removed tooltip from connection error message on "Daedalus Diagnostics" screen ([PR 1535](https://github.com/input-output-hk/daedalus/pull/1535))
- Storybook fixes:
  - Fixed Storybook dialog content overflow issues in Chrome 76+ versions ([PR 1536](https://github.com/input-output-hk/daedalus/pull/1536))
  - Fixed Storybook "Wallets" stories ([PR 1473](https://github.com/input-output-hk/daedalus/pull/1473))

### Chores

- Implemented automatic sorting of CSS properties ([PR 1483](https://github.com/input-output-hk/daedalus/pull/1483))
- Improved "Netlify" build setup ([PR 1509](https://github.com/input-output-hk/daedalus/pull/1509))
- Unified "Support request" data handling ([PR 1472](https://github.com/input-output-hk/daedalus/pull/1472))
- Updated Storybook to the latest version ([PR 1426](https://github.com/input-output-hk/daedalus/pull/1426))
- Updated "Buildkite" to use "daedalus" queue for macOS builds ([PR 1568](https://github.com/input-output-hk/daedalus/pull/1568))
- Theming improvements:
  - Improved scalability of theme management code ([PR 1502](https://github.com/input-output-hk/daedalus/pull/1502))
  - Renamed `themes:check` script and updated "Daedalus Theme Management" documentation ([PR 1525](https://github.com/input-output-hk/daedalus/pull/1525))

### Specifications

- Newsfeed ([PR 1569](https://github.com/input-output-hk/daedalus/pull/1569))
- New "Wallet Creation" process ([PR 1499](https://github.com/input-output-hk/daedalus/pull/1499), [PR 1515](https://github.com/input-output-hk/daedalus/pull/1515), [PR 1530](https://github.com/input-output-hk/daedalus/issues/1530))
- "Delegation" screens UI:
  - Improved "Delegation setup" wizard ([PR 1503](https://github.com/input-output-hk/daedalus/pull/1503))
  - Implemented "Stake pools" screen "Search and Filtering" logic ([PR 1468](https://github.com/input-output-hk/daedalus/pull/1468))
  - Implemented "Delegation countdown" design update ([PR 1481](https://github.com/input-output-hk/daedalus/pull/1481))
  - Fixed minor UI issues on the "Delegation center" screen and "Delegation setup" wizard ([PR 1545](https://github.com/input-output-hk/daedalus/pull/1545))

## 0.14.0

### Features

- Implemented guided manual updates ([PR 1410](https://github.com/input-output-hk/daedalus/pull/1410), [PR 1423](https://github.com/input-output-hk/daedalus/pull/1423))
- Application menu improvements:
  - Implemented "Help" application menu item with "Known Issues", "Blank Screen Fix", "Safety Tips", "Feature Request", "Support Request", "Download Logs", "Block Consolidation Status" and "Daedalus Diagnostics" options ([PR 1382](https://github.com/input-output-hk/daedalus/pull/1382), [PR 1459](https://github.com/input-output-hk/daedalus/pull/1459), [PR 1487](https://github.com/input-output-hk/daedalus/issues/1487), [PR 1488](https://github.com/input-output-hk/daedalus/pull/1488), [PR 1493](https://github.com/input-output-hk/daedalus/pull/1493), [PR 1518](https://github.com/input-output-hk/daedalus/pull/1518), [PR 1523](https://github.com/input-output-hk/daedalus/pull/1523))
  - Improved the "Ada redemption" menu item UX by disabling the option while Daedalus is not fully synced ([PR 1458](https://github.com/input-output-hk/daedalus/pull/1458))
- Application dialogs improvements:
  - Implemented new "About Us" dialog design ([PR 1369](https://github.com/input-output-hk/daedalus/pull/1369), [PR 1450](https://github.com/input-output-hk/daedalus/pull/1450))
  - Implemented new "Block Consolidation Status" dialog design ([PR 1377](https://github.com/input-output-hk/daedalus/pull/1377), [PR 1416](https://github.com/input-output-hk/daedalus/pull/1416), [PR 1521](https://github.com/input-output-hk/daedalus/pull/1521))
  - Implemented new "Daedalus Diagnostics" dialog design ([PR 1370](https://github.com/input-output-hk/daedalus/pull/1370), [PR 1402](https://github.com/input-output-hk/daedalus/pull/1402), [PR 1404](https://github.com/input-output-hk/daedalus/pull/1404), [PR 1408](https://github.com/input-output-hk/daedalus/pull/1408), [PR 1438](https://github.com/input-output-hk/daedalus/pull/1438), [PR 1516](https://github.com/input-output-hk/daedalus/pull/1516), [PR 1521](https://github.com/input-output-hk/daedalus/pull/1521))
- Implemented detection of system locale during Daedalus startup ([PR 1348](https://github.com/input-output-hk/daedalus/pull/1348))
- Logging improvements:
  - Implemented "Daedalus versions" log file ([PR 1401](https://github.com/input-output-hk/daedalus/pull/1401))
  - Implemented "State-snapshot.json" log file ([PR 1456](https://github.com/input-output-hk/daedalus/pull/1456))
  - Improved file extension of Daedalus logs ([PR 1381](https://github.com/input-output-hk/daedalus/pull/1381), [PR 1384](https://github.com/input-output-hk/daedalus/pull/1384))
  - Improved Daedalus log file rotation logic ([PR 1448](https://github.com/input-output-hk/daedalus/pull/1448))
- "Loading" screen improvements:
  - Implemented animated Daedalus logo ([PR 1457](https://github.com/input-output-hk/daedalus/pull/1457))
  - Implemented status icons ([PR 1325](https://github.com/input-output-hk/daedalus/pull/1325), [PR 1365](https://github.com/input-output-hk/daedalus/pull/1365), [PR 1452](https://github.com/input-output-hk/daedalus/pull/1452))
  - Improved "Having trouble connecting/syncing?" notification ([PR 1453](https://github.com/input-output-hk/daedalus/pull/1453), [PR 1479](https://github.com/input-output-hk/daedalus/pull/1479), [PR 1489](https://github.com/input-output-hk/daedalus/pull/1489))
  - Improved handling of "Invalid TLS certificate" error ([PR 1344](https://github.com/input-output-hk/daedalus/pull/1344)), ([PR 1471](https://github.com/input-output-hk/daedalus/pull/1471))
  - Improved clock synchronisation checks (NTP) ([PR 1462](https://github.com/input-output-hk/daedalus/pull/1462))
  - Improved code structure by adding containers for each overlay ([PR 1446](https://github.com/input-output-hk/daedalus/pull/1446))
- Implemented "Wallet UTXO distribution" screen ([PR 1353](https://github.com/input-output-hk/daedalus/pull/1353), [PR 1437](https://github.com/input-output-hk/daedalus/pull/1437), [PR 1526](https://github.com/input-output-hk/daedalus/pull/1526))
- Implemented "External link" icon on all links which open external content ([PR 1447](https://github.com/input-output-hk/daedalus/pull/1447), [PR 1455](https://github.com/input-output-hk/daedalus/pull/1455), [PR 1476](https://github.com/input-output-hk/daedalus/pull/1476))
- Implemented "Preparing logs for download" notification ([PR 1341](https://github.com/input-output-hk/daedalus/pull/1341))
- Implemented autmated scrolling of error messages into view ([PR 1383](https://github.com/input-output-hk/daedalus/pull/1383))
- Implemented timestamp on "Paper wallet certificate" PDF ([PR 1385](https://github.com/input-output-hk/daedalus/pull/1385), [PR 1400](https://github.com/input-output-hk/daedalus/pull/1400), [PR 1482](https://github.com/input-output-hk/daedalus/pull/1482))
- Implemented three new themes: "Yellow", "White" and "Dark Cardano" ([PR 1480](https://github.com/input-output-hk/daedalus/pull/1480), [PR 1517](https://github.com/input-output-hk/daedalus/pull/1517))
- "Delegation" screens UI:
  - Implemented "Legacy wallet" notification UI ([PR 1409](https://github.com/input-output-hk/daedalus/pull/1409))
  - Implemented "Decentralisation countdown" screen UI ([PR 1390](https://github.com/input-output-hk/daedalus/pull/1390))
  - Implemented "Staking" screens navigation ([PR 1395](https://github.com/input-output-hk/daedalus/pull/1395), [PR 1400](https://github.com/input-output-hk/daedalus/pull/1400), [PR 1461](https://github.com/input-output-hk/daedalus/pull/1461))
  - Implemented "Delegation center" screen UI ([PR 1440](https://github.com/input-output-hk/daedalus/pull/1440), [PR 1463](https://github.com/input-output-hk/daedalus/pull/1463))
  - Implemented "Delegation setup" wizard UI ([PR 1416](https://github.com/input-output-hk/daedalus/pull/1416)), [PR 1430](https://github.com/input-output-hk/daedalus/pull/1430), [PR 1439](https://github.com/input-output-hk/daedalus/pull/1439), [PR 1442](https://github.com/input-output-hk/daedalus/pull/1442))
  - Implemented "Stake pools" screen UI ([PR 1397](https://github.com/input-output-hk/daedalus/pull/1397), [PR 1412](https://github.com/input-output-hk/daedalus/pull/1412), [PR 1420](https://github.com/input-output-hk/daedalus/pull/1420), [PR 1429](https://github.com/input-output-hk/daedalus/pull/1429), [PR 1431](https://github.com/input-output-hk/daedalus/pull/1431), [PR 1484](https://github.com/input-output-hk/daedalus/pull/1484))
  - Implemented "Rewards" screen UI ([PR 1403](https://github.com/input-output-hk/daedalus/pull/1403))
  - Implemented "Epochs" screen UI ([PR 1418](https://github.com/input-output-hk/daedalus/pull/1418))
  - Implemented "Info" screen UI ([PR 1394](https://github.com/input-output-hk/daedalus/pull/1394))

### Fixes

- Fixed validation rules for passwords containing non-Latin characters ([PR 1354](https://github.com/input-output-hk/daedalus/pull/1354))
- Minor UI fixes:
  - Fixed content truncation on the "Receive" screen of wallets with spending password while Daedalus is in minimized mode ([PR 1407](https://github.com/input-output-hk/daedalus/pull/1407))
  - Fixed content truncation on the "Add Wallet" screen during wallet restoration ([PR 1405](https://github.com/input-output-hk/daedalus/pull/1405))
  - Fixed spending password being asked on the "Receive" screen when clicking on the "Copy address" link ([PR 1392](https://github.com/input-output-hk/daedalus/pull/1392))
  - Fixed the "download logs" link position in Japanese version of the "Support" screen ([PR 1372](https://github.com/input-output-hk/daedalus/pull/1372))
  - Fixed incorrect capitalization by replacing "Ada" with "ada" ([PR 1317](https://github.com/input-output-hk/daedalus/pull/1317), [PR 1336](https://github.com/input-output-hk/daedalus/pull/1336))
- Routing logic fixes:
  - Fixed screen flashes during Daedalus loading stage ([PR 1444](https://github.com/input-output-hk/daedalus/pull/1444))
  - Fixed routing logic which allowed the display of "Settings" screens before the wallet data is fully loaded ([PR 1373](https://github.com/input-output-hk/daedalus/pull/1373))
  - Fixed shutting down screen priority ([PR 1428](https://github.com/input-output-hk/daedalus/pull/1428))

### Chores

- Code improvements:
  - Implemented Storybook menu using queryStrings over localStorage ([PR 1426](https://github.com/input-output-hk/daedalus/pull/1426))
  - Implemented Storybook build script for deployment to "Netlify" ([1391](https://github.com/input-output-hk/daedalus/pull/1391))
  - Implemented Storybook theme and language selection ([PR 1408](https://github.com/input-output-hk/daedalus/pull/1408))
  - Implemented `prettier` formatting tool and set up automation for basic maintainance tasks ([PR 1335](https://github.com/input-output-hk/daedalus/pull/1335), [PR 1347](https://github.com/input-output-hk/daedalus/pull/1347), [PR 1352](https://github.com/input-output-hk/daedalus/pull/1352), [PR 1375](https://github.com/input-output-hk/daedalus/pull/1375))
  - Improved internal IPC communication ([PR 1332](https://github.com/input-output-hk/daedalus/pull/1332), [PR 1368](https://github.com/input-output-hk/daedalus/pull/1368))
  - Improved switching of the application menu screens ([PR 1419](https://github.com/input-output-hk/daedalus/pull/1419))
  - Improved `Webpack 4` build performance ([PR 1320](https://github.com/input-output-hk/daedalus/pull/1320))
  - Refactored store async functions to use mobx runInAction calls ([PR 1367](https://github.com/input-output-hk/daedalus/pull/1367))
  - Refactored the current epoch calculation ([PR 1339](https://github.com/input-output-hk/daedalus/pull/1339))
  - Removed legacy references to report server ([PR 1338](https://github.com/input-output-hk/daedalus/pull/1338), [PR 1425](https://github.com/input-output-hk/daedalus/pull/1425))
  - Removed legacy references to `npm` in favour of `yarn` ([PR 1399](https://github.com/input-output-hk/daedalus/pull/1399), [PR 1402](https://github.com/input-output-hk/daedalus/pull/1402))
  - Resolved dependency vulnerabilities ([PR 1414](https://github.com/input-output-hk/daedalus/pull/1414), [PR 1467](https://github.com/input-output-hk/daedalus/pull/1467))
  - Resolved EKG port collision in Linux development environment ([PR 1393](https://github.com/input-output-hk/daedalus/pull/1393))
  - Updated `React-Polymorph` to version 0.8.6 ([PR 1396](https://github.com/input-output-hk/daedalus/pull/1396))
  - Updated PR template ([PR 1376](https://github.com/input-output-hk/daedalus/pull/1376))
- Theming improvements:
  - Implemented `createTheme` utility function for rapid generation of new themes ([PR 1371](https://github.com/input-output-hk/daedalus/pull/1371), [PR 1445](https://github.com/input-output-hk/daedalus/pull/1445))
  - Implemented `themes:check` script for checking missing definitions on `createTheme` object ([PR 1424](https://github.com/input-output-hk/daedalus/pull/1424))
  - Implemented `themes:update` script for automatically generating color values using `createTheme` and writing the updates to all themes ([PR 1464](https://github.com/input-output-hk/daedalus/pull/1464))
- Testing improvements:
  - Implemented unit tests for mnemonic generation and validation ([PR 1379](https://github.com/input-output-hk/daedalus/pull/1379))
  - Implemented acceptance tests for the "Loading" screen and "Block Consolidation Status" dialog ([PR 1318](https://github.com/input-output-hk/daedalus/pull/1318))
  - Simplified the test setup ([PR 1378](https://github.com/input-output-hk/daedalus/pull/1378))

## 0.13.1

### Fixes

- Improved text on the "Support" screen ([PR 1361](https://github.com/input-output-hk/daedalus/pull/1361))
- Fixed Daedalus log file rotation ([PR 1358](https://github.com/input-output-hk/daedalus/pull/1358))

## 0.13.0

### Features

- New validation message for transactions which are to big in data size ([PR 1308](https://github.com/input-output-hk/daedalus/pull/1308), [PR 1331](https://github.com/input-output-hk/daedalus/pull/1331))
- Structured logging for Daedalus in JSON format, matching the structured logging format used in Cardano SL ([PR 1299](https://github.com/input-output-hk/daedalus/pull/1299))
- Additional instructions for paper wallet certificate creation ([PR 1309](https://github.com/input-output-hk/daedalus/pull/1309))
- Performant rendering of big lists using virtual lists, applied to lists of transactions and addresses ([PR 1276](https://github.com/input-output-hk/daedalus/pull/1276), [PR 1303](https://github.com/input-output-hk/daedalus/pull/1303), [PR 1305](https://github.com/input-output-hk/daedalus/pull/1305), [PR 1306](https://github.com/input-output-hk/daedalus/pull/1306), [PR 1312](https://github.com/input-output-hk/daedalus/pull/1312), [PR 1313](https://github.com/input-output-hk/daedalus/pull/1313), [PR 1340](https://github.com/input-output-hk/daedalus/pull/1340))
- "System-info.json" file saved in the public log folder, containing the system specification of the user's computer running Daedalus ([PR 1292](https://github.com/input-output-hk/daedalus/pull/1292))
- Block storage consolidation status screen, available from the application menu ([PR 1275](https://github.com/input-output-hk/daedalus/pull/1275), [PR 1294](https://github.com/input-output-hk/daedalus/pull/1294))
- Application menu in English and Japanese ([PR 1262](https://github.com/input-output-hk/daedalus/pull/1262), [PR 1296](https://github.com/input-output-hk/daedalus/pull/1296))
- Notification for log download completion ([PR 1228](https://github.com/input-output-hk/daedalus/pull/1228))
- Handling low disk space operation, including warning message and stopping Cardano node to prevent data corruption ([PR 1157](https://github.com/input-output-hk/daedalus/pull/1157))
- Improved clock synchronisation checks (NTP) ([PR 1258](https://github.com/input-output-hk/daedalus/pull/1258), [PR 1319](https://github.com/input-output-hk/daedalus/pull/1319))
- Support for "frontend-only" mode ([PR 1241](https://github.com/input-output-hk/daedalus/pull/1241), [PR 1260](https://github.com/input-output-hk/daedalus/pull/1260))
- Improved user experience for multiple running Daedalus instances by replacing "Daedalus is already running" dialog with focusing of the already running Daedalus instance ([PR 1229](https://github.com/input-output-hk/daedalus/pull/1229))
- Replaced in-app support request with links to support page ([PR 1199](https://github.com/input-output-hk/daedalus/pull/1199), [PR 1331](https://github.com/input-output-hk/daedalus/pull/1331))
- Improved user experience for inline editing fields (like wallet renaming) ([PR 1231](https://github.com/input-output-hk/daedalus/pull/1231))
- Added a BESTPRACTICES.md document containing rules and recommendations for writing better JavaScript and CSS ([PR 1233](https://github.com/input-output-hk/daedalus/pull/1233))
- Refactoring to disable Node JS integration in Electron's rendering processes ([PR 1099](https://github.com/input-output-hk/daedalus/pull/1099/commits))
- Daedalus Windows installer/uninstaller multi-language support and English and Japanese translations ([PR 1287](https://github.com/input-output-hk/daedalus/pull/1287), [PR 1298](https://github.com/input-output-hk/daedalus/pull/1298), [PR 1322](https://github.com/input-output-hk/daedalus/pull/1322))

### Fixes

- Fixed "Daedalus" state directory on "Staging" and "Testnet" builds ([PR 1316](https://github.com/input-output-hk/daedalus/pull/1316))
- Fixed Dev-Tools initialization in development mode ([PR 1302](https://github.com/input-output-hk/daedalus/pull/1302))
- Fixed text selection issues (disabled drag and drop of selected text and enabled unselect option) ([PR 1289](https://github.com/input-output-hk/daedalus/pull/1289))
- Disabled the default behavior of the context menu following a right click for non-input elements ([PR 1281](https://github.com/input-output-hk/daedalus/pull/1281))
- Initial setup of language locale and terms of use available before Cardano node is connected to network ([PR 1279](https://github.com/input-output-hk/daedalus/pull/1279), [PR 1284](https://github.com/input-output-hk/daedalus/pull/1284))
- Fixed `ps-list` issues which caused false detection of already running Daedalus instance ([PR 1266](https://github.com/input-output-hk/daedalus/pull/1266))
- Fixed `getTransactionHistory` API endpoint account index query parameter name ([PR 1255](https://github.com/input-output-hk/daedalus/pull/1255))
- Fixed issues with custom lock files by implementing "instance-lock" feature from Electron 3 ([PR 1229](https://github.com/input-output-hk/daedalus/pull/1229), [PR 1244](https://github.com/input-output-hk/daedalus/pull/1244))
- Fixed application reloading issues in the Ada Redemption page ([PR 1217](https://github.com/input-output-hk/daedalus/pull/1217))
- Fixed the contents of `/etc/nix/nix.conf` file listed as a part of Cardano setup in Daedalus README file ([PR 1215](https://github.com/input-output-hk/daedalus/pull/1215))
- Fixed the partly broken Flow setup ([PR 1124](https://github.com/input-output-hk/daedalus/pull/1124))
- Fixed failing apply-node-update test ([PR 1156](https://github.com/input-output-hk/daedalus/pull/1156))
- Removed antivirus notification which was presented during wallet restoration ([PR 1164](https://github.com/input-output-hk/daedalus/pull/1164))
- Fixed design implementations issues ([PR 1209](https://github.com/input-output-hk/daedalus/pull/1209))
- Fixed the flickering issue on "Network Status" screen syncing chart ([PR 1206](https://github.com/input-output-hk/daedalus/pull/1206))

### Chores

- Added support for newly introduced `UtxoNotEnoughFragmented` API error message ([PR 1297](https://github.com/input-output-hk/daedalus/pull/1297))
- Changed the behavior of "Network status" from screen to dialog ([PR 1286](https://github.com/input-output-hk/daedalus/pull/1286))
- Updated the list of contributors on the "About" screen ([PR 1282](https://github.com/input-output-hk/daedalus/pull/1282))
- Added more logging for API errors ([PR 1278](https://github.com/input-output-hk/daedalus/pull/1278)
- Removed node version check as nix shell is enforcing the version ([PR 1236](https://github.com/input-output-hk/daedalus/pull/1236))
- Fixed Daedalus shell to evaluate embedded variables properly ([PR 1235](https://github.com/input-output-hk/daedalus/pull/1235))
- Added `--rp` prefixed CSS variables to Daedalus themes to configure styles of React-Polymorph components and removed SimpleTheme imports previously used for React-Polymorph overrides in favor of using ThemeProvider's themeOverrides feature ([PR 1139](https://github.com/input-output-hk/daedalus/pull/1139))
- Changed API call logging level from `debug` to `info` ([PR 1183](https://github.com/input-output-hk/daedalus/pull/1183))
- Enabled Cardano Wallet API documentation server in development mode - reacheable on https://localhost:8091/docs/v1/index/ ([PR 1187](https://github.com/input-output-hk/daedalus/pull/1187))
- Updated application dependencies ([PR 1201](https://github.com/input-output-hk/daedalus/pull/1201), [PR 1216](https://github.com/input-output-hk/daedalus/pull/1216), [PR 1227](https://github.com/input-output-hk/daedalus/pull/1227), [PR 1251](https://github.com/input-output-hk/daedalus/pull/1251), [PR 1265](https://github.com/input-output-hk/daedalus/pull/1265))

## 0.12.1

### Fixes

- Fixed the transactions loading logic ([PR 1261](https://github.com/input-output-hk/daedalus/pull/1261), [PR 1267](https://github.com/input-output-hk/daedalus/pull/1267))
- Fixed the network block stalling logic in order to prevent showing "Network connection lost - reconnecting..." screen in case network block height is stalling while local block height is still increasing ([PR 1246](https://github.com/input-output-hk/daedalus/pull/1246))

## 0.12.0

### Features

- Implemented "Forbidden mnemonic" error message ([PR 1093](https://github.com/input-output-hk/daedalus/pull/1093), [PR 1100](https://github.com/input-output-hk/daedalus/pull/1100))
- Implemented extended error messages for transaction fee calculation failures ([PR 1111](https://github.com/input-output-hk/daedalus/pull/1111))
- Implemented forms submission on "Enter" key press ([PR 981](https://github.com/input-output-hk/daedalus/pull/981))
- Implemented Japanese "Terms of use" for the "Testnet" network ([PR 1097](https://github.com/input-output-hk/daedalus/pull/1097))
- Implemented lock-file mechanism which prevents multiple Daedalus instances from running against the same state directory and network ([PR 1113](https://github.com/input-output-hk/daedalus/pull/1113), [PR 1114](https://github.com/input-output-hk/daedalus/pull/1114), [PR 1121](https://github.com/input-output-hk/daedalus/pull/1121), [PR 1166](https://github.com/input-output-hk/daedalus/pull/1166))
- Implemented "New data layer migration" screen ([PR 1096](https://github.com/input-output-hk/daedalus/pull/1096))
- Implemented sending of `cert` and `key` with API requests in order to enable 2-way TLS authentication ([PR 1072](https://github.com/input-output-hk/daedalus/pull/1072))
- Implemented support for Cardano node "structured logging" ([PR 1092](https://github.com/input-output-hk/daedalus/pull/1092), [PR 1122](https://github.com/input-output-hk/daedalus/pull/1122))
- Implemented the IPC driven Cardano node / Daedalus communication ([PR 1075](https://github.com/input-output-hk/daedalus/pull/1075), [PR 1107](https://github.com/input-output-hk/daedalus/pull/1107), [PR 1109](https://github.com/input-output-hk/daedalus/pull/1109), [PR 1115](https://github.com/input-output-hk/daedalus/pull/1115), [PR 1118](https://github.com/input-output-hk/daedalus/pull/1118), [PR 1119](https://github.com/input-output-hk/daedalus/pull/1119), [PR 1162](https://github.com/input-output-hk/daedalus/pull/1162))
- Improved the loading UX ([PR 723](https://github.com/input-output-hk/daedalus/pull/723))
- Improved the NTP check handling ([PR 1086](https://github.com/input-output-hk/daedalus/pull/1086), [PR 1149](https://github.com/input-output-hk/daedalus/pull/1149), [PR 1158](https://github.com/input-output-hk/daedalus/pull/1158), [PR 1194](https://github.com/input-output-hk/daedalus/pull/1194), [PR 1213](https://github.com/input-output-hk/daedalus/pull/1213))
- Improved the transaction details text selection ([PR 1073](https://github.com/input-output-hk/daedalus/pull/1073), [PR 1095](https://github.com/input-output-hk/daedalus/pull/1095))
- Integrated Cardano V1 API endpoints ([PR 1018](https://github.com/input-output-hk/daedalus/pull/1018), [PR 1031](https://github.com/input-output-hk/daedalus/pull/1031), [PR 1037](https://github.com/input-output-hk/daedalus/pull/1037), [PR 1042](https://github.com/input-output-hk/daedalus/pull/1042), [PR 1045](https://github.com/input-output-hk/daedalus/pull/1045), [PR 1070](https://github.com/input-output-hk/daedalus/pull/1070), [PR 1078](https://github.com/input-output-hk/daedalus/pull/1078), [PR 1079](https://github.com/input-output-hk/daedalus/pull/1079), [PR 1080](https://github.com/input-output-hk/daedalus/pull/1080), [PR 1088](https://github.com/input-output-hk/daedalus/pull/1088), [PR 1220](https://github.com/input-output-hk/daedalus/pull/1220))
- Refactored and improved `NetworkStatus` store to use V1 API data ([PR 1081](https://github.com/input-output-hk/daedalus/pull/1081))

### Fixes

- Added support for non-Latin characters in spending password ([PR 1196](https://github.com/input-output-hk/daedalus/pull/1196))
- Fixed an issue which allowed to submit invalid form on the "Send" screen using an "enter" key ([PR 1002](https://github.com/input-output-hk/daedalus/pull/1002))
- Fixed an issue which caused the send bug report form to hang and not send the support request if the logs were downloaded before attempting to send the request ([PR 1176](https://github.com/input-output-hk/daedalus/pull/1176))
- Fixed an issue which would show a runtime JavaScript error in case Daedalus is not started using Launcher ([PR 1169](https://github.com/input-output-hk/daedalus/pull/1169))
- Fixed an issue which would trigger submission of the "Generate address" button on the "Receive" screen on right-mouse click ([PR 1082](https://github.com/input-output-hk/daedalus/pull/1082))
- Fixed an issue with the "Having Trouble Connecting" notification not showing up on the "Connection lost. Reconnecting..." screen ([PR 1112](https://github.com/input-output-hk/daedalus/pull/1112))
- Fixed broken "Export wallet to file" dialog and improved "Wallet settings" screen dialogs file structure and namings ([PR 998](https://github.com/input-output-hk/daedalus/pull/998))
- Fixed special buttons outline color ([PR 990](https://github.com/input-output-hk/daedalus/pull/990))
- Fixed the close button's hover state within the AntivirusRestaurationSlowdownNotification so it's full height ([PR 1131](https://github.com/input-output-hk/daedalus/pull/1131))
- Fixed uninstaller unicode issue ([PR 1116](https://github.com/input-output-hk/daedalus/pull/1116))
- Fixed window icon quality issue on Linux ([PR 1170](https://github.com/input-output-hk/daedalus/pull/1170))
- Implement design review fixes and improvements ([PR 1090](https://github.com/input-output-hk/daedalus/pull/1090))
- Pinned eslint-scope version via the yarn resolutions feature ([PR 1017](https://github.com/input-output-hk/daedalus/pull/1017))
- Prevented wallet data polling during wallet deletion ([PR 996](https://github.com/input-output-hk/daedalus/pull/996))
- Removed transaction status and number of confirmation during wallet restoration ([PR 1189](https://github.com/input-output-hk/daedalus/pull/1189))

### Chores

- Added acceptance tests for node update notification with apply and postpone update scenarios ([PR 977](https://github.com/input-output-hk/daedalus/pull/977))
- Added acceptance tests for maximum wallets limit ([PR 979](https://github.com/input-output-hk/daedalus/pull/979))
- Added acceptance tests for the "About" dialog ([PR 975](https://github.com/input-output-hk/daedalus/pull/975))
- Added acceptance tests for wallets, transactions and addresses ordering ([PR 976](https://github.com/input-output-hk/daedalus/pull/976))
- Added Daedalus and Cardano version to Daedalus log ([PR 1094](https://github.com/input-output-hk/daedalus/pull/1094), [PR 1137](https://github.com/input-output-hk/daedalus/pull/1137))
- Added dynamic prefix derivation to local storage keys used to store previous Cardano node PID and added dynamic derivation of Cardano node process names based on the current platform ([PR 1109](https://github.com/input-output-hk/daedalus/pull/1109))
- Added logging of "GPU-crashed" events ([PR 1083](https://github.com/input-output-hk/daedalus/pull/1083))
- Added `NETWORK` info to the application title bar ([PR 1174](https://github.com/input-output-hk/daedalus/pull/1174))
- Added screenshot recording of failed acceptance tests ([PR 1103](https://github.com/input-output-hk/daedalus/pull/1103))
- Added Storybook stories for the wallet screens ([942](https://github.com/input-output-hk/daedalus/pull/942))
- Disabled logging to `cardano-node.log` since it was not needed for the support and it was impacting performance ([PR 1027](https://github.com/input-output-hk/daedalus/pull/1027))
- Enabled Cardano node EKG for non-mainnet networks and made it accessible from the "Network status" screen ([PR 1188](https://github.com/input-output-hk/daedalus/pull/1188))
- Fixated all `npm` dependencies and update script names ([PR 1014](https://github.com/input-output-hk/daedalus/pull/1014))
- Fixed broken storybook ([PR 1041](https://github.com/input-output-hk/daedalus/pull/1041))
- Improved compress/download logs handling ([PR 995](https://github.com/input-output-hk/daedalus/pull/995))
- Integrated latest React-Polymorph features: render props architecture, theme composition, and a ThemeProvider HOC ([PR 950](https://github.com/input-output-hk/daedalus/pull/950))
- Integrated latest React-Polymorph with a fix for `NumericInput` component carrot positioning issues ([PR 1172](https://github.com/input-output-hk/daedalus/pull/1172))
- Made `nodelLogPath` entry in `launcher-config.yaml` optional ([PR 1027](https://github.com/input-output-hk/daedalus/pull/1027))
- Made `port` and `ca` of Ada Api configurable during runtime ([PR 1067](https://github.com/input-output-hk/daedalus/pull/1067))
- Removed all ETC specific files ([PR 1068](https://github.com/input-output-hk/daedalus/pull/1068), [PR 1108](https://github.com/input-output-hk/daedalus/pull/1108))
- Removed Wallet `export` and `import` features for the "Testnet" network ([PR 1168](https://github.com/input-output-hk/daedalus/pull/1168))
- Switched from `npm` to `yarn` ([PR 989](https://github.com/input-output-hk/daedalus/pull/989))

## 0.11.2

### Fixes

- Update Cardano SL revision to version `1.3.2` ([PR 1181](https://github.com/input-output-hk/daedalus/pull/1181))

## 0.11.1

### Features

- Added support for configurable Api port ([PR 992](https://github.com/input-output-hk/daedalus/pull/992))
- Added initial configuration for a testnet build of Daedalus ([PR 991](https://github.com/input-output-hk/daedalus/pull/991))

### Fixes

- Changed the information we are sending on support requests to the reporting server ([PR 1036](https://github.com/input-output-hk/daedalus/pull/1036))
- Fixed an issue on Windows where Daedalus couldn't start if the Windows username contained non-ASCII characters ([PR 1057](https://github.com/input-output-hk/daedalus/pull/1057))
- Fixed a issue with Electron which results in blank/white screen rendering on some OS/Graphics-card/Drivers combinations ([PR 1007](https://github.com/input-output-hk/daedalus/pull/1007))
- Fixed Daedalus icon scaling issues on Windows ([PR 1064](https://github.com/input-output-hk/daedalus/pull/1064))
- Implemented error dialog shown in case Daedalus is not started using Launcher ([PR 1054](https://github.com/input-output-hk/daedalus/pull/1054))
- Improved paper wallet certificate QR code compatibility ([PR 999](https://github.com/input-output-hk/daedalus/pull/999))
- Updated to `electon@1.7.16` to avoid the known vulnarability [CVE-2018-15685](https://cve.mitre.org/cgi-bin/cvename.cgi?name=CVE-2018-15685) ([PR 1066](https://github.com/input-output-hk/daedalus/pull/1066))

### Chores

- Implemented `NETWORK` specific Cardano Blockchain Explorer links (Mainnet, Testnet, Staging) ([PR 1051](https://github.com/input-output-hk/daedalus/pull/1051))
- Improved network label in the Top bar ([PR 988](https://github.com/input-output-hk/daedalus/pull/988))
- Added "Testnet" label to paper wallet certificates which are not generated on the mainnet ([PR 1055](https://github.com/input-output-hk/daedalus/pull/1055))

## 0.11.0

### Features

- Implemented a switch instead of a link for "hide used" addresses on the Receive screen ([PR 935](https://github.com/input-output-hk/daedalus/pull/935))
- Added a notification for Windows users that using antivirus software might slow down wallet restoration ([PR 1020](https://github.com/input-output-hk/daedalus/pull/1020))

### Fixes

- Disabled dragging of static UI images elements ([PR 910](https://github.com/input-output-hk/daedalus/pull/910))
- Disabled webview tags to prevent XSS attacks ([PR 946](https://github.com/input-output-hk/daedalus/pull/946))
- Fixed 633 npm audit issues by upgrading dependencies ([PR 944](https://github.com/input-output-hk/daedalus/pull/944))
- Fixed a bug related to rendering of transactions with duplicated IDs ([PR 947](https://github.com/input-output-hk/daedalus/pull/947))
- Fixed broken translation on create wallet dialog ([PR 930](https://github.com/input-output-hk/daedalus/pull/930))
- Fixed huge build file sizes ([PR 886](https://github.com/input-output-hk/daedalus/pull/886))
- Fixed Linux Kanji characters support ([PR 971](https://github.com/input-output-hk/daedalus/pull/971))
- Fixed Storybook configuration issues ([PR 928](https://github.com/input-output-hk/daedalus/pull/928))
- Fixed text wrapping within wallet navigation tabs ([PR 911](https://github.com/input-output-hk/daedalus/pull/911))
- Reduced resource usage ([PR 886](https://github.com/input-output-hk/daedalus/pull/886))
- Refactored about window to normal UI dialog to save resources ([PR 965](https://github.com/input-output-hk/daedalus/pull/965))
- Removed mocked ETC wallet data injection in Cardano Daedalus builds ([PR 955](https://github.com/input-output-hk/daedalus/pull/955))
- Removed the Ada Redemption link from the "app bar" menu and added it to system menu ([PR 972](https://github.com/input-output-hk/daedalus/pull/972))

### Chores

- Added "Author" and "Status" information to all Daedalus README files ([PR 901](https://github.com/input-output-hk/daedalus/pull/901))
- Added acceptance tests for displaying transactions in various contexts ([PR 870](https://github.com/input-output-hk/daedalus/pull/870))
- Added acceptance tests for hide/show used addresses feature ([PR 957](https://github.com/input-output-hk/daedalus/pull/957))
- Added flow checks and linting for storybook code ([PR 938](https://github.com/input-output-hk/daedalus/pull/938))
- Added PR template with PR review checlist ([PR 882](https://github.com/input-output-hk/daedalus/pull/882))
- Fixed wallet restoration and import fragile acceptance tests steps definitions ([PR 885](https://github.com/input-output-hk/daedalus/pull/885))
- Improved re-build times with caching ([PR 915](https://github.com/input-output-hk/daedalus/pull/915))
- Improved request body length calculation ([PR 892](https://github.com/input-output-hk/daedalus/pull/892))
- Improved text display by reducing letter-spacing ([PR 973](https://github.com/input-output-hk/daedalus/pull/973))
- Log expected errors as debug messages only ([PR 916](https://github.com/input-output-hk/daedalus/pull/916))
- Refactored npm scripts to use colon style ([PR 939](https://github.com/input-output-hk/daedalus/pull/939))
- Refactored various magic numbers & strings into constants ([PR 881](https://github.com/input-output-hk/daedalus/pull/881))

## 0.10.1

### Fixes

- Fixed issues with printing paper wallet certificate PDFs on printers with low amount of memory by replacing the SVG assets with transparent PNG images ([PR 951](https://github.com/input-output-hk/daedalus/pull/951))
- Fixed presentation bug that caused only ten wallets to be shown in the wallets list, even though there were more than ten wallets in the application ([PR 958](https://github.com/input-output-hk/daedalus/pull/958))
- Fixed reporting server URL used for submitting support requests for the Linux build of Daedalus ([PR 959](https://github.com/input-output-hk/daedalus/pull/959))
- Fixed missing launcher log file ([PR 963](https://github.com/input-output-hk/daedalus/pull/963))
- Fixed issue with multiple cardano-node processes running and windows allowing skipping of files in installer ([PR 953](https://github.com/input-output-hk/daedalus/pull/953))
- Limited maximum number of wallets to 20 ([PR 966](https://github.com/input-output-hk/daedalus/pull/966))

## 0.10.0

### Features

- Asynchronous wallet restoration ([PR 849](https://github.com/input-output-hk/daedalus/pull/849))
- Added wallet restore indicator in the sidebar ([PR 862](https://github.com/input-output-hk/daedalus/pull/862))
- Added Daedalus version in application title ([PR 826](https://github.com/input-output-hk/daedalus/pull/826))
- Added "Show more transactions" button on the summary screen ([PR 848](https://github.com/input-output-hk/daedalus/pull/848))
- Added "Recovery - regular" and "Recovery - force vended" tabs on "Ada redemption" screen ([PR 933](https://github.com/input-output-hk/daedalus/pull/933))
- Setting spending password to "ON" by default ([PR 856](https://github.com/input-output-hk/daedalus/pull/856))
- Paper wallet certificate generation ([PR 779](https://github.com/input-output-hk/daedalus/pull/779))
- Paper wallet certificate restoration ([PR 794](https://github.com/input-output-hk/daedalus/pull/794))
- Update paper wallet certificate generation to use more random bits ([PR 871](https://github.com/input-output-hk/daedalus/pull/871))

### Fixes

- Fixed color of Ada logo color on the loading screen (logo was too transparent) ([PR 850](https://github.com/input-output-hk/daedalus/pull/850))
- Fixed styling of "click to upload" text on Ada redemption screen (text was too bold) ([PR 850](https://github.com/input-output-hk/daedalus/pull/850))
- Fixed node syncing state bubble background color in Dark blue theme ([PR 850](https://github.com/input-output-hk/daedalus/pull/850))
- Fixed cropped log list within the support request dialog ([PR 850](https://github.com/input-output-hk/daedalus/pull/850))
- Fixed submit button flicker within the support request dialog ([PR 850](https://github.com/input-output-hk/daedalus/pull/850))
- Fixed "Download logs" button bug where it would throw Javascript error in case user does not select destination directory for saving logs file ([PR 850](https://github.com/input-output-hk/daedalus/pull/850))
- Updated moment.js dependency to the latest version which fixes ReDOS vulnerability ([PR 782](https://github.com/input-output-hk/daedalus/pull/782))
- Updated Electron dependency to the latest version which fixes vulnerability issues ([PR 855](https://github.com/input-output-hk/daedalus/pull/855))
- Extended About dialog size in order to display increased team members list ([PR 880](https://github.com/input-output-hk/daedalus/pull/880))
- Created screen for Ada redemption in case there are no wallets ([PR 970](https://github.com/input-output-hk/daedalus/pull/970))

### Chores

- Improved build system ([PR 692](https://github.com/input-output-hk/daedalus/pull/692))
- Added instructions for accessing FAQ, reporting a problem and downloading logs on the support page in general settings ([PR 818](https://github.com/input-output-hk/daedalus/pull/818))
- Removed sending logs to remote server feature ([PR 818](https://github.com/input-output-hk/daedalus/pull/818))
- Improved support dialog by default switching the "Attach logs" switch to active ([PR 829](https://github.com/input-output-hk/daedalus/pull/829))
- Improved support dialog logs list (logs are now alphabetically ordered) ([PR 828](https://github.com/input-output-hk/daedalus/pull/828))
- Improved manual bug report submission dialog text color in Dark blue theme ([PR 824](https://github.com/input-output-hk/daedalus/pull/824))
- Improved transaction element addresses: addresses and hashes now act like linkes to Cardano Explorer ([PR 850](https://github.com/input-output-hk/daedalus/pull/850))
- Improved transaction element toggling: click on the white areas should collapse view (not only click on the top part) ([PR 850](https://github.com/input-output-hk/daedalus/pull/850))
- Improved transaction element transaction state handling: transaction with 0 confirmations is now shown in pending state ([PR 825](https://github.com/input-output-hk/daedalus/pull/825))
- Improved wallet creation dialogs handling: clicking outside of the dialogs should not close them ([PR 832](https://github.com/input-output-hk/daedalus/pull/832))
- Improved Dark blue theme styling of update and restoration notifications ([PR 827](https://github.com/input-output-hk/daedalus/pull/827))
- Improved About dialog (team members are updated and alphabetically ordered) ([PR 830](https://github.com/input-output-hk/daedalus/pull/830))
- Improved copy icons visibility on Receive screen ([PR 850](https://github.com/input-output-hk/daedalus/pull/850))
- Improved error message on the NTP time synchronisation error screen ([PR 852](https://github.com/input-output-hk/daedalus/pull/852))
- Improved Loading screen timeouts logic ([PR 860](https://github.com/input-output-hk/daedalus/pull/860))
- Updated sidebar bug icon action to open Settings Support page instead of Support Request dialog ([PR 858](https://github.com/input-output-hk/daedalus/pull/858))
- Updated the way parallel restore/imports are explaned to the user by disabling the restore/import buttons on the Add wallet dialog and presenting a user friendly message ([PR 861](https://github.com/input-output-hk/daedalus/pull/861))
- Updated main and acceptance tests readme files with new acceptance tests setup information ([PR 831](https://github.com/input-output-hk/daedalus/pull/831))
- Paper wallet certificate generation and wallet restoration from a paper wallet certificate acceptance tests ([PR 807](https://github.com/input-output-hk/daedalus/pull/807))
- Use UTC time in Daedalus logs ([PR 833](https://github.com/input-output-hk/daedalus/pull/833))
- Disabled text selection on static UI elements ([PR 868](https://github.com/input-output-hk/daedalus/pull/868))
- Updated Daedalus and Cardano team members on the about dialog ([PR 872](https://github.com/input-output-hk/daedalus/pull/872))

## 0.9.1

### Features

- New Edit section in system menu with copy & paste and related actions ([PR 817](https://github.com/input-output-hk/daedalus/pull/817))

### Fixes

- A presentation bug has been fixed that caused only five recent transactions to be shown on the transaction list, even though there were more than five transactions in the wallet ([PR 778](https://github.com/input-output-hk/daedalus/pull/778))
- An issue has been fixed that stopped copy and paste operations from working when initiated using the right-click context menu ([PR 817](https://github.com/input-output-hk/daedalus/pull/817))
- On Windows, the desktop icon was not showing the Daedalus image; this has now been fixed ([837](https://github.com/input-output-hk/daedalus/pull/837))
- An error has been fixed that in some cases prevented users creating a wallet with a name containing non-latin characters, like Japanese Kanji or Chinese ([PR 840](https://github.com/input-output-hk/daedalus/pull/840))

## 0.9.0

### Features

- Do not block the UI while wallet is being restored/imported ([PR 457](https://github.com/input-output-hk/daedalus/pull/457))
- Start and stop Mantis Client from Daedalus main process ([PR 568](https://github.com/input-output-hk/daedalus/pull/568))
- Add Ada and Mantis logo to loader screens ([PR 584](https://github.com/input-output-hk/daedalus/pull/584))
- New Edit section in system menu with copy & paste and related actions ([PR 817](https://github.com/input-output-hk/daedalus/pull/817))

### Fixes

- Use correct Daedalus version in bug reports ([PR 948](https://github.com/input-output-hk/daedalus/pull/948))
- Fixed about dialog for ETC build ([PR 586](https://github.com/input-output-hk/daedalus/pull/586))
- Made `Light Blue` theme the default for ETC build ([PR 586](https://github.com/input-output-hk/daedalus/pull/586))
- Fixed a bug related to gas limit when sending transactions ([PR 586](https://github.com/input-output-hk/daedalus/pull/586))
- Remove transactions sorting ([PR 587](https://github.com/input-output-hk/daedalus/pull/587))
- Improve ETC transaction amount validation ([PR 590](https://github.com/input-output-hk/daedalus/pull/590))
- Fixed storybook and resolver issues ([PR 617](https://github.com/input-output-hk/daedalus/pull/617))
- Time of your machine is different from global time. You are 0 seconds behind ([PR 678](https://github.com/input-output-hk/daedalus/pull/678))
- Updated copy for the sync error screen ([PR 657](https://github.com/input-output-hk/daedalus/pull/657))
- Detect when wallet is disconnected ([PR 689](https://github.com/input-output-hk/daedalus/pull/689))
- Improve connecting/reconnecting messages on Loading screen ([PR 696](https://github.com/input-output-hk/daedalus/pull/696))
- Send bug reports with logs from Daedalus ([PR 691](https://github.com/input-output-hk/daedalus/pull/691))
- Poll local time difference every 1 hour, only when connected ([PR 719](https://github.com/input-output-hk/daedalus/pull/719))
- Fixed various styling issues and updated to React-Polymorph 0.6.2 ([PR 726](https://github.com/input-output-hk/daedalus/pull/726))
- Fixed async restore/import dialogs logic ([PR 735](https://github.com/input-output-hk/daedalus/pull/735))
- Fixed minor UI issue on receive screen when generating wallet addresses with spending password ([PR 738](https://github.com/input-output-hk/daedalus/pull/738))
- Fixed `Time sync error notification` not showing up in case blocks syncing has not started ([PR 752](https://github.com/input-output-hk/daedalus/pull/752))
- Properly wait for node update request ([PR 949](https://github.com/input-output-hk/daedalus/pull/949))

### Chores

- Update DLL package and cleanup translation files ([PR 566](https://github.com/input-output-hk/daedalus/pull/566))
- Replace electron-json-storage with electron-storage ([PR 579](https://github.com/input-output-hk/daedalus/pull/579))
- Update terms of use for ETC version ([PR 606](https://github.com/input-output-hk/daedalus/pull/606))
- Update Electron to version 1.7.11 which resolves security issues ([PR 677](https://github.com/input-output-hk/daedalus/pull/677))
- Document Cardano SL build ([PR 654](https://github.com/input-output-hk/daedalus/pull/654))
- Update about dialog content ([PR 680](https://github.com/input-output-hk/daedalus/pull/680))
- Ada redemption tests modified to run in mainnet mode ([PR 681](https://github.com/input-output-hk/daedalus/pull/681))
- Log file moved to public folder and Electron crash reporter removed ([PR 682](https://github.com/input-output-hk/daedalus/pull/682))

## 0.8.3

### Fixes

- Improved messages for update notifications ([PR 526](https://github.com/input-output-hk/daedalus/pull/526))

## 0.8.2

### Features

- Postpone Update Api call integration ([PR 485](https://github.com/input-output-hk/daedalus/pull/485))
- Wallet restore recovery phrase textarea replaced with React-Polymorph Autocomplete ([PR 516](https://github.com/input-output-hk/daedalus/pull/516))
- Instructions for setting up NTP on Daedalus website ([PR 531](https://github.com/input-output-hk/daedalus/pull/531))

### Fixes

- Fix error message text for A to A transaction error ([PR 484](https://github.com/input-output-hk/daedalus/pull/484))
- Fix "label prop type" checkbox issue in React-Polymorph ([PR 487](https://github.com/input-output-hk/daedalus/pull/487))
- Remove all drag and drop instructions from UI ([PR 495](https://github.com/input-output-hk/daedalus/pull/495))
- Preferences saved to local storage prefixed with network ([PR 501](https://github.com/input-output-hk/daedalus/pull/501))
- Disable wallet import and export features for the mainnet ([PR 503](https://github.com/input-output-hk/daedalus/pull/503))
- Correctly prevent max-window-size in electron ([PR 532](https://github.com/input-output-hk/daedalus/pull/532))
- Make sure wallet import and restore in-progress notification is hidden once process is done ([PR 540](https://github.com/input-output-hk/daedalus/pull/540))
- Fix broken translation files loading ([PR 559](https://github.com/input-output-hk/daedalus/pull/559))
- Fix async wallet import/restore issues ([PR 562](https://github.com/input-output-hk/daedalus/pull/562))

### Chores

- Update version in About dialog to 0.8.2 ([PR 496](https://github.com/input-output-hk/daedalus/pull/496))
- Set Dark theme as default one for the mainnet ([PR 497](https://github.com/input-output-hk/daedalus/pull/497))
- JS-Api integration ([PR 525](https://github.com/input-output-hk/daedalus/pull/525))
- Only log errors to papertrail ([PR 509](https://github.com/input-output-hk/daedalus/pull/509))
- Update MobX React Form to latest version ([PR 533](https://github.com/input-output-hk/daedalus/pull/533))
- Update Electron, electron-devtools-installer, electron-packager and electron-rebuild to latest versions ([PR 541](https://github.com/input-output-hk/daedalus/pull/541))
- Update flow-bin to version `0.59.0` ([PR 544](https://github.com/input-output-hk/daedalus/pull/544))
- Cleanup and standardization of ETC Api calls ([PR 549](https://github.com/input-output-hk/daedalus/pull/549))
- Unused vendor dependencies cleanup and DLL file optimization ([PR 555](https://github.com/input-output-hk/daedalus/pull/555))
- Introduce `testReset` endpoint for ETC API ([PR 558](https://github.com/input-output-hk/daedalus/pull/558))
- Update acceptance tests suite dependencies ([PR 561](https://github.com/input-output-hk/daedalus/pull/561))
- Refactor Cardano type declarations to type folder ([PR 557](https://github.com/input-output-hk/daedalus/pull/557))

## 0.8.1

### Fixes

- Fix all features eslint warnings ([PR 468](https://github.com/input-output-hk/daedalus/pull/468))
- Fix for disabled buttons on dark-blue theme ([PR 473](https://github.com/input-output-hk/daedalus/pull/473))
- Remove maximum screen width and height in full-screen mode ([PR 472](https://github.com/input-output-hk/daedalus/pull/472))
- Fix "label click" dropdown issue in React-Polymorph ([PR 479](https://github.com/input-output-hk/daedalus/pull/479))

## 0.8.0

### Features

- Opt-in mode for sending logs to the remote server
- Added support for Cmd+H hotkey shortcut for hiding application window on OSX ([PR 404](https://github.com/input-output-hk/daedalus/pull/404))
- Setup environment variable for testnet/mainnet mode ([PR 400](https://github.com/input-output-hk/daedalus/pull/400))
- Export wallet to file ([PR 426](https://github.com/input-output-hk/daedalus/pull/426))
- Add theming options in settings ([PR 370](https://github.com/input-output-hk/daedalus/pull/398))
- About page, available from the system menu ([PR 430](https://github.com/input-output-hk/daedalus/pull/430))
- Setup default themes for TN and MN and introduce new theme names ([PR 451](https://github.com/input-output-hk/daedalus/pull/451))
- Transaction fees UX improvements on wallet sent screen ([PR 449](https://github.com/input-output-hk/daedalus/pull/449))
- Acceptance tests for General Settings screens ([PR 466](https://github.com/input-output-hk/daedalus/pull/466))
- Simple confirmation dialog for sending money ([PR 481](https://github.com/input-output-hk/daedalus/pull/481))
- Japanese terms of use for the mainnet ([PR 486](https://github.com/input-output-hk/daedalus/pull/486))

### Fixes

- Implement MomentJs internationalization
- Also quit whole app when last window is closed on osx
- Daedalus and Cardano node update notification text messages change ([PR 392](https://github.com/input-output-hk/daedalus/pull/392))
- Fixed brittle acceptance test steps (wallet latest transaction amount check) ([PR 393](https://github.com/input-output-hk/daedalus/pull/393))
- Fixed Numeric component caret position bug on wallet send screen ([PR 394](https://github.com/input-output-hk/daedalus/pull/394))
- Fixed all design implementation issues ([PR 397](https://github.com/input-output-hk/daedalus/pull/397))
- Fixed eslint syntax warnings ([PR 403](https://github.com/input-output-hk/daedalus/pull/403))
- Fixed wallet spending password fields error messages positioning on wallet create/restore/import dialogs ([PR 407](https://github.com/input-output-hk/daedalus/pull/407))
- Fixed inline-editing success-messages on wallet settings screen ([PR 408](https://github.com/input-output-hk/daedalus/pull/408))
- Fixed dialogs cut-off content on smaller screens ([PR 370](https://github.com/input-output-hk/daedalus/pull/398))
- Fixed the issue of dialogs being closable while wallet import/creation/restoring is happening ([PR 393](https://github.com/input-output-hk/daedalus/pull/414))
- Fixed calculation and display of transaction assurance levels ([PR 390](https://github.com/input-output-hk/daedalus/pull/416))
- Fixes Transaction additional info showing/hiding affects all user's wallets ([PR 411](https://github.com/input-output-hk/daedalus/pull/411))
- Fixed promise handling for unmounted wallet send form ([PR 412](https://github.com/input-output-hk/daedalus/pull/412))
- Fixes Transaction toggle issue on active wallet change ([PR 421](https://github.com/input-output-hk/daedalus/pull/421))
- Fixed missing 'used address' styling for default wallet address on wallet receive screen ([PR 422](https://github.com/input-output-hk/daedalus/pull/422))
- Terms of service for the mainnet ([PR 425](https://github.com/input-output-hk/daedalus/pull/425))
- Fixed failing acceptance tests ([PR 424](https://github.com/input-output-hk/daedalus/pull/424))
- Show correct error message on sending money to Ada redemption address ([PR 423](https://github.com/input-output-hk/daedalus/pull/423))
- Fixed ENOENT error on log file rotation ([PR 428](https://github.com/input-output-hk/daedalus/pull/428))
- Fixed logging error ([PR 431](https://github.com/input-output-hk/daedalus/pull/431))
- Add theming support on UI which was introduced after introduction of Theming feature ([PR 434](https://github.com/input-output-hk/daedalus/pull/434))
- Speed-up About page opening time ([PR 450](https://github.com/input-output-hk/daedalus/pull/450))
- Importing a wallet from a file updated-API integration ([PR 439](https://github.com/input-output-hk/daedalus/pull/439))
- Exporting a wallet updated-API integration ([PR 438](https://github.com/input-output-hk/daedalus/pull/438))
- Show correct error message for A -> A transaction error ([PR 441](https://github.com/input-output-hk/daedalus/pull/441))
- Improved QR code colors on wallet send screen for dark themes ([PR 448](https://github.com/input-output-hk/daedalus/pull/448))
- Fixed transaction fees flicker on wallet send screen form submit ([PR 456](https://github.com/input-output-hk/daedalus/pull/456))
- Fixed failing acceptance-tests ([PR 454](https://github.com/input-output-hk/daedalus/pull/454))
- Prevent application window auto focusing while running acceptance-tests ([PR 458](https://github.com/input-output-hk/daedalus/pull/458))
- Limit and validate wallet's name maximum length ([PR 465](https://github.com/input-output-hk/daedalus/pull/465))
- UI/UX fixes ([PR 476](https://github.com/input-output-hk/daedalus/pull/476))

### Chores

- Added readme file for running acceptance tests ([PR 395](https://github.com/input-output-hk/daedalus/pull/395))
- Improved webpack build performance ([PR 402](https://github.com/input-output-hk/daedalus/pull/402))
- Updated README file "Development - network options" section ([PR 410](https://github.com/input-output-hk/daedalus/pull/410))
- All CSS hardcoded values replaced with variables ([PR 370](https://github.com/input-output-hk/daedalus/pull/398))
- Implemented sync progress and payment requests with new JS API as first step to remove the purescript API ([PR 437](https://github.com/input-output-hk/daedalus/pull/437))
- Speed optimizations for page reloads while running tests by loading bundled up for one-time tests runs ([PR 448](https://github.com/input-output-hk/daedalus/pull/445))
- Optimized structure and naming of theming files ([PR 453](https://github.com/input-output-hk/daedalus/pull/453))
- Added testnet label, loaded from translations for release candidate ([PR 460](https://github.com/input-output-hk/daedalus/pull/460))

## 0.7.0

### Features

- Sidebar is no longer auto-hiding if application window is width is greater than 1150px
- When user opens or closes the sidebar using the hamburger icon it stays open or closed
- Optionally setting a password during wallet creation
- "Terms of use" screen on first application start
- Spending password on "Send money" form
- Optionally setting a password during wallet restore
- Ada redemption disclaimer
- "Terms of use" page in settings section
- Change wallet password dialog UX improvements
- New receive screen with support for HD wallets
- Wallet rename
- Multiple input and output addresses in transaction details
- Ada redemption acceptance tests
- Show BTC and ETC currencies as coming soon in create wallet dialog
- Remove currencies dropdown on create wallet dialog
- Spending password on wallet receive page
- Prepared UI dialogs for exporting paper wallets
- Prepared UI dialogs for importing paper wallets
- Spending password on "Import wallet" dialog
- Spending password on "Ada redemption" forms
- Acceptance test for "Restore wallet with and without spending password" feature
- Acceptance test for "Create wallet with spending password" feature
- Acceptance test for "Import wallet with/without spending password" feature
- Acceptance test for "Send money from a wallet with spending password" feature
- Acceptance test for "Generate wallet address" feature
- Acceptance test for "Wallet settings management" features
- Final version of Daedalus logo added on the loading screen
- Final version of Daedalus logo added in the top-bar
- Receive page design update
- UI for displaying transaction fees on wallet send screen
- Correct placeholder text for Ada redemption "Ada amount" input

### Fixes

- Verification of mnemonic recovery phrase during wallet backup is not working if it contains duplicate words
- Pending confirmation amounts split (incoming and outgoing)
- Prevent wallet send form reset on submit
- Prevent redemption key reset after unsuccessful redemption
- Ghost boxes on "Loading" screen
- Reset Ada redemption form values on page load and certificate add/remove events
- Prevent sidebar auto-hiding feature and always show submenus on wallets page load
- Apply grammatical fixes to redemption instructions
- Prevent sidebar visual glitch on sidebar open
- Added missing "Add wallet" button label translation key
- Prevent "Delete wallet" dialog from closing until deletion is over
- Improved error handling on "Set/Change wallet password" dialog
- Improved API response errors handling
- Update active wallet after wallet update actions
- Last generated address was not being reset when switching wallets and it was shown on the receive screen for the wrong wallet
- Improved API nextUpdate response errors handling
- Improved active wallet data refresh after wallet balance/settings change
- Fixed failing wallet add/restore/import acceptance tests
- Polling for wallet data and system update should be disabled while node is syncing with the blockchain
- Prevent syncing icon from being always stuck in syncing state by refactoring in-sync state calculation
- Acceptance test for "Sending money" feature should check receiver wallet's balance
- Improved spending password validation rules
- Improved acceptance tests for generating new addresses
- Removed temporary workaround for creating new accounts during wallet create and wallet restore
- Prevent React key duplicates in transaction from/to addresses lists
- Acceptance tests configuration fix for the timeouts
- Show more specific error messages on "Change password" dialog
- Update password fields placeholders to match latest designs
- Prevent selected wallet reset on "Ada redemption" screen on tab or certificate change
- Fixed sending amount maximum value validation
- Use correct styling for used addresses marking on wallet receive screen

### Chores

- Prevent logging of harmless error messages to the terminal
- Purge "translation/messages/app" as a part of npm dev script
- Use markdown for "Terms of use" content
- Added manually written Flow types for API responses
- Testnet version on the testnet label bumped from 0.3 to 0.5
- Replaced all React-Toolbox components with React-Polymorph ones ([PR 361](https://github.com/input-output-hk/daedalus/pull/361))
- Temporary workaround for missing Japanese translations for Terms of Use that allows users to accept them in English

## 0.6.2

### Features

- Ada redemption testnet disclaimer on Ada redemption pages

## 0.6.1

### Fixes

- Wallet name on the send screen was hardcoded in Japanese translation
- Button on the wallet send screen is too large
- Hamburger button" and wallet's name are present on the "Ada redemption" and "Settings" screens
- Ada redemption button is not enabled as soon as inputs are valid
- Prevent navigation on file drop event in the application window

### Chores

- Logging improved by stringifying API requests and responses being logged
- Added system information to logs: OS, OS version, CPU and total RAM information

## 0.6.0

### Features

- Ada redemption with certificate decryption and parsing to extract the redemption key
- Transaction assurance level with color coding for transactions and settings for a normal or strict mode
- Wallet settings page with wallet deletion and transaction assurance level settings
- User interface language option on application start and in settings with English and Japanese translations
- Ada amounts formatting with thousands separator for displaying and entering amounts
- Application header updated to show wallet name and amount of money in the wallet
- Copy wallet address to the clipboard with UI notification
- Testnet label
- Application level settings page with language choice option
- Wallet summary page
- Showing absolute percentage with two decimals on the blockchain sync page
- File logging and sending logs to Papertrail logging service without sensitive user data

### Fixes

- Toggling the application bar not working properly
- UI glitch when quickly typing in Ada amounts on the send money form
- "Add wallet" dialog does not disappear immediately after wallet creation
- Clearing correctly entered backup recovery phrase should not be possible
- Sidebar "randomly" closes/opens when navigating
- Ada redemption overlay should also cover the wallet navigation
- No transactions message is not vertically centered on Transactions page
- Transactions ordering
- Smaller UI improvements and fixes

## 0.5.0

![Main Wallet screenshot in 0.5.0](screenshots/2016-11-24 release 0.5.0 main-wallet.png)
![Main Wallet screenshot in 0.5.0](screenshots/2016-11-24 release 0.5.0 profile-settings.png)

### Features

- Infinite loading for transaction list, testable with "Main wallet"
- Simple transaction search by title
- Adding new personal wallet via sidebar menu
- Profile settings screen
- "No transactions" message for wallets with no transactions
- "No transactions found" message when transaction search returns no results
- Loading indicator on application startup

### Fixes

- Improved sidebar UX when there is no sub-menu
- Transaction icons updated to design specs
- Improved transaction list page styling
- Active input field styled to design specs

### Chores

- Fixed and improved first acceptance test, made API data configurable for future test cases

## 0.4.0

![Screenshot from Release 0.4.0](screenshots/2016-11-16 release 0.4.0.png)

### Features

- Sidebar with "Wallets" and "Settings" categories and list of dummy wallets
- Switch between wallets via the sidebar
- Selected wallet is highlighted in sidebar
- New transactions list design
- Transactions are grouped by date
- Click on transaction title toggles the details

### Fixes

- Fixed many styling issues
- Improved font rendering

### Chores

- First version of the backend API
- Improved and simplified mobx state management

## 0.3.0

### Features

- Added wallet creation screen that appears when there is no wallet yet
- Updated to the latest design specs and refactor to
  [react-toolbox](http://react-toolbox.com/) instead of
  material-ui for the UI components. This gives us much better style
  customization and theming options.
- Cleaned up the boilerplate app menus
- Added basic form validations using [mobx-react-form](https://github.com/foxhound87/mobx-react-form)
- Added i18n support with [react-intl](https://github.com/yahoo/react-intl)
- Added wallet send / receive / transactions screens
- Form submitting UX updated to the design specifications with introduction of button loading spinners
  and support for submitting the form with enter key
- Added cut, copy & paste application menu items and keyboard shortcuts

### Fixes

- Fixed problems with the form validations
- Fixed the electron build process & ensured that it worked

### Chore

- Updated to react-router v4
- Testing setup with cucumber & spectron
