Changelog
=========

## vNext

### Fixes

- Removed locale specific rules from the CSS files ([PR 1871](https://github.com/input-output-hk/daedalus/pull/1871))

### Chores

- Integrated react-polymorph ScrollBar CSS vars into each theme ([PR 1827](https://github.com/input-output-hk/daedalus/pull/1827))
- Updated small 3rd party dependencies ([PR 1877](https://github.com/input-output-hk/daedalus/pull/1877))
- Updated React dependencies ([PR 1873](https://github.com/input-output-hk/daedalus/pull/1873))
- Updated Storybook dependencies ([PR 1873](https://github.com/input-output-hk/daedalus/pull/1873))
- Re-enabled theme selection on the "Settings" screen ([PR 1872](https://github.com/input-output-hk/daedalus/pull/1872))
- Implemented acceptance tests for custom number, date and time formats ([PR 1868](https://github.com/input-output-hk/daedalus/pull/1868))

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
- Fixed flat button color  ([PR 1586](https://github.com/input-output-hk/daedalus/pull/1586))

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
