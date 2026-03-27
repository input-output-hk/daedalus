# Daedalus Release Test Plan

> **Status:** Draft
> **Branch:** feature/mithril-ux
> **Networks:** Mainnet · Preprod testnet
> **Platforms:** Linux · macOS (Intel + Apple Silicon) · Windows 10/11

---

## Contents

1. [Platform Matrix](#1-platform-matrix)
2. [Environment Setup](#2-environment-setup)
3. [Mithril Bootstrap Sync](#3-mithril-bootstrap-sync)
4. [Genesis (Traditional) Sync](#4-genesis-traditional-sync)
5. [Sending and Receiving ADA](#5-sending-and-receiving-ada)
6. [Native Tokens](#6-native-tokens)
7. [Stake Registration and Delegation](#7-stake-registration-and-delegation)
8. [DRep (Voting Power) Delegation](#8-drep-voting-power-delegation)
9. [Hardware Wallets — Ledger](#9-hardware-wallets--ledger)
10. [Hardware Wallets — Trezor](#10-hardware-wallets--trezor)
11. [Regression Checklist](#11-regression-checklist)
12. [Pass/Fail Recording Template](#12-passfail-recording-template)

---

## 1. Platform Matrix

Every numbered test case below must be executed on **all cells marked ✓** in this matrix. If a cell shows **N/A** the combination is not applicable or not supported.

| Test Area                   | Linux x64 | macOS Intel | macOS ARM (M-series) | Windows 10 | Windows 11 |
|-----------------------------|:---------:|:-----------:|:--------------------:|:----------:|:----------:|
| Mithril bootstrap sync      | ✓         | ✓           | ✓                    | ✓          | ✓          |
| Genesis sync                | ✓         | ✓           | ✓                    | ✓          | ✓          |
| Send/receive ADA            | ✓         | ✓           | ✓                    | ✓          | ✓          |
| Native tokens               | ✓         | ✓           | ✓                    | ✓          | ✓          |
| Stake delegation            | ✓         | ✓           | ✓                    | ✓          | ✓          |
| DRep delegation             | ✓         | ✓           | ✓                    | ✓          | ✓          |
| Ledger Nano S/S+/X          | ✓         | ✓           | ✓                    | ✓          | ✓          |
| Trezor Model T              | ✓         | ✓           | ✓                    | ✓          | ✓          |

---

## 2. Environment Setup

### 2.1 Prerequisites

- [ ] Fresh install from the official installer (`.deb` / `.dmg` / `.exe`) — do **not** reuse a previous data directory for full-sync tests
- [ ] Testnet (Preprod) wallet with at least 100 ADA funded via the faucet
- [ ] Mainnet wallet with at least 10 ADA for smoke tests
- [ ] Two separate wallets: **Sender** (funded) and **Receiver** (empty or near-empty)
- [ ] At least one native token policy and three test tokens minted to the Sender wallet
- [ ] Hardware wallet devices: Ledger Nano S, Nano S+, Nano X; Trezor Model T
- [ ] Ledger Live and Trezor Suite **closed** during tests (Daedalus must own the USB connection)
- [ ] Known DRep ID on preprod for DRep delegation tests

### 2.2 Data Directory Locations

| Platform     | Default path |
|--------------|-------------|
| Linux        | `~/.local/share/Daedalus/` |
| macOS        | `~/Library/Application Support/Daedalus/` |
| Windows      | `%APPDATA%\Daedalus\` |

### 2.3 Log Collection

On failure, collect:
- `node.log` — cardano-node output
- `cardano-wallet.log` — wallet backend output
- `renderer.log` — renderer process log
- `main.log` — Electron main process log

---

## 3. Mithril Bootstrap Sync

### Overview

Mithril allows syncing the node from a certified snapshot rather than replaying the entire chain from genesis. The first-run wizard presents the user with a choice, storage location selection, and a multi-stage progress view.

### 3.1 Decision Screen

| # | Test Case | Expected Result |
|---|-----------|-----------------|
| M-01 | Launch Daedalus on a machine with **no existing chain data**. | The Mithril chain-storage decision overlay appears before any sync begins. |
| M-02 | On the decision screen, verify the displayed storage path matches the platform default. | Path is correct for each OS (see §2.2). |
| M-03 | Click **Accept** (use Mithril). | Transitions to the Mithril progress view; no genesis sync starts. |
| M-04 | Click **Decline** (use genesis sync). | Overlay is dismissed; genesis sync begins (covered in §4). |
| M-05 | Kill Daedalus mid-decision and relaunch. | Decision screen re-appears; previous choice is not persisted. |

### 3.2 Chain Storage Location

| # | Test Case | Expected Result |
|---|-----------|-----------------|
| M-06 | On the chain storage view, click **Choose a folder** and select a non-default path. | Decision screen reflects the chosen path. |
| M-07 | After choosing a custom path, click **Reset to default**. | Path reverts to the platform default; decision screen shows the default path. |
| M-08 | Select a folder on a volume with **insufficient free space** (< ~50 GB). | Validation error "Insufficient space" appears; Continue button is disabled. |
| M-09 | While the storage validation is running (busy state), verify UI is locked. | Select, Continue, and Reset buttons are all disabled. |
| M-10 | Select a path on a read-only volume / path without write permission. | Appropriate permission error is shown; user cannot proceed. |

### 3.3 Progress View

| # | Test Case | Expected Result |
|---|-----------|-----------------|
| M-11 | Accept Mithril; observe the progress stepper. | Steps appear in order: **Preparing → Downloading → Verifying → Converting → Starting node**. |
| M-12 | Verify the **Preparing** step becomes active first. | Step indicator is highlighted; descriptive text is visible. |
| M-13 | Verify transition to **Downloading** step. | Progress bar or byte counter updates. Download speed / ETA visible. |
| M-14 | Verify transition to **Finalizing** (Converting + Starting node) step. | Spinner or progress text reflects final conversion stage. |
| M-15 | Click **Cancel** during the **Downloading** stage. | Returns to the chain storage selection view; partial data is cleaned up. |
| M-16 | Click **Cancel** during the **Verifying** stage. | Same as M-15. |
| M-17 | Re-accept after cancellation. | Download restarts from the beginning (or resumes if supported). |

### 3.4 Error Handling

| # | Test Case | Expected Result |
|---|-----------|-----------------|
| M-18 | Simulate a **download error** (disconnect network mid-download). | "Download failed" error heading and retry option are displayed. |
| M-19 | Simulate a **verification error** (corrupt snapshot file). | "Verify failed" error heading and retry option are displayed. |
| M-20 | Simulate a **conversion error** (disk full during import). | "Convert failed" error heading with guidance is displayed. |
| M-21 | Simulate a **node-start error** (manually corrupt converted DB). | "Node start failed" error heading is shown with support link. |
| M-22 | Click **Retry** from any error view. | Process restarts from the failed stage (or from the beginning). |

### 3.5 Post-Sync State

| # | Test Case | Expected Result |
|---|-----------|-----------------|
| M-23 | After Mithril sync completes, verify the wallet backend connects. | Daedalus main UI loads; block sync progress shows near 100%. |
| M-24 | Verify the sync tip is within a few minutes of wall-clock time. | Epoch/slot shown in the status bar matches network tip. |
| M-25 | Subsequent launch (chain data already present). | Decision screen does **not** appear; node starts directly. |

### 3.6 Platform-Specific

| # | Test Case | Expected Result |
|---|-----------|-----------------|
| M-26 | **macOS** — select an iCloud Drive folder as storage path. | Warning or error shown; iCloud paths are not suitable. |
| M-27 | **Windows** — select a path on a drive letter other than `C:\`. | Works correctly; path stored and used on relaunch. |
| M-28 | **Linux** — run as a non-root user with a home directory on a network mount. | Graceful error or successful completion with appropriate warning. |

---

## 4. Genesis (Traditional) Sync

### Overview

When the user declines Mithril (or Mithril is unavailable), Daedalus syncs from genesis. This path must still work correctly.

| # | Test Case | Expected Result |
|---|-----------|-----------------|
| G-01 | Decline Mithril on first launch; observe sync indicator. | Progress bar increments from 0%; epoch/slot advances. |
| G-02 | Close Daedalus at ~10% synced; relaunch. | Sync **resumes** from where it stopped (no re-sync from zero). |
| G-03 | Interrupt network connectivity during genesis sync; restore. | Daedalus detects disconnect, shows appropriate status, resumes automatically when network returns. |
| G-04 | Let sync reach 100%; verify tip is within 2 minutes of wall-clock. | Status bar shows "Synced" or equivalent; no "Connecting" spinner. |
| G-05 | Verify correct block height shown in the About dialog. | Block number matches a public block explorer (±5 blocks). |
| G-06 | **macOS** — verify CPU usage drops after sync is complete. | Fan noise subsides; Activity Monitor shows cardano-node CPU < 10% at rest. |
| G-07 | **Windows** — verify no Windows Firewall prompts after first-run allow. | No repeated UAC/firewall dialogs on subsequent launches. |
| G-08 | **Linux** — verify no AppArmor/SELinux denials in system journal. | `journalctl` shows no denial messages for daedalus binaries. |

---

## 5. Sending and Receiving ADA

### 5.1 Receiving ADA

| # | Test Case | Expected Result |
|---|-----------|-----------------|
| R-01 | Navigate to the **Receive** tab of the Receiver wallet. | A fresh Shelley (bech32 `addr1…`) address is shown. |
| R-02 | Copy address via the **Copy** button; paste into a text editor. | Exact address string is in the clipboard. |
| R-03 | Click **Generate new address** (Byron wallet only). | A new unique address appears at the top of the list; previous address is moved to "used". |
| R-04 | Toggle **Show used** addresses on/off. | Used addresses appear/disappear accordingly; count is correct. |
| R-05 | Generate new address with wrong spending password (Byron). | Inline error "Incorrect password" appears; no address is generated. |
| R-06 | Send funds from an external wallet to the displayed Receiver address. | Transaction appears in the Receiver wallet's history with correct amount. |
| R-07 | Address ownership check — switch to a different wallet; verify address belongs to original wallet. | Address is flagged as not belonging to the active wallet. |

### 5.2 Sending ADA

| # | Test Case | Expected Result |
|---|-----------|-----------------|
| S-01 | Open the **Send** tab; enter a valid Receiver address and amount `1 ADA`. | Fee estimate appears; "Next" button becomes active. |
| S-02 | Enter amount > wallet balance. | Inline error `NotEnoughFundsForTransactionError`; Next button stays disabled. |
| S-03 | Enter amount = wallet balance (leaving nothing for fees). | Inline error `NotEnoughFundsForTransactionFeesError`; Next button stays disabled. |
| S-04 | Enter `0` ADA. | Inline error `invalidAmount`; Next button stays disabled. |
| S-05 | Enter an invalid receiver address (random string). | Inline error `invalidAddress`; Next button stays disabled. |
| S-06 | Submit a valid transaction with correct spending password. | Confirmation dialog shown; after submit, transaction appears in history as **Pending** then **Confirmed**. |
| S-07 | Submit with wrong spending password. | Error `IncorrectPasswordError` shown in confirmation dialog; transaction not sent. |
| S-08 | Submit with too-short spending password. | Same error as S-07. |
| S-09 | Verify Sender wallet balance is reduced after transaction confirms. | Sender balance shown in the UI is lower than before the transaction. |
| S-10 | Verify Receiver wallet balance is higher after transaction confirms. | Receiver balance shown in the UI reflects the received amount. |
| S-11 | Send to a Byron address from a Shelley wallet. | Transaction succeeds; funds arrive in Byron wallet. |
| S-12 | Send maximum available ADA using the **Send all** feature. | Wallet balance reaches minimum ADA threshold or 0; fee is covered. |

### 5.3 Transaction History

| # | Test Case | Expected Result |
|---|-----------|-----------------|
| TH-01 | Verify sent transaction shows correct amount, direction (−), and timestamp. | Amount is negative; timestamp is within 2 minutes of submission. |
| TH-02 | Verify received transaction shows correct amount, direction (+), and timestamp. | Amount is positive. |
| TH-03 | Click a transaction row to expand details. | Transaction ID (TxHash), block number, epoch, and slot are shown. |
| TH-04 | Click the transaction ID external link. | Opens the Cardano explorer in the default browser. |
| TH-05 | Filter/search transactions (if applicable). | Results match filter criteria. |

---

## 6. Native Tokens

### 6.1 Viewing Tokens

| # | Test Case | Expected Result |
|---|-----------|-----------------|
| NT-01 | Open wallet with known native tokens; navigate to **Tokens** tab. | All tokens are listed with name, ticker (if metadata), and balance. |
| NT-02 | Verify tokens with on-chain metadata show correct asset name and icon. | Name and ticker from Cardano Token Registry are displayed. |
| NT-03 | Verify unregistered tokens show policy ID and hex asset name. | Raw identifiers shown when metadata is unavailable. |
| NT-04 | Search/filter the token list by name or policy ID. | Matching tokens appear; non-matching are hidden. |
| NT-05 | Verify token balance updates after receiving new tokens. | Balance reflects the on-chain UTxO. |

### 6.2 Receiving Native Tokens

| # | Test Case | Expected Result |
|---|-----------|-----------------|
| NT-06 | Send a native token from an external wallet to the Daedalus Receiver address. | Token appears in the **Tokens** tab with correct balance; accompanying ADA min-UTxO is shown. |
| NT-07 | Verify the accompanying min-UTxO ADA is included in the ADA balance. | ADA balance accounts for the min-UTxO portion locked with the token. |

### 6.3 Sending Native Tokens

| # | Test Case | Expected Result |
|---|-----------|-----------------|
| NT-08 | Open **Send** tab; click **Add token**; select a token from the picker. | Token row appears in the send form alongside the ADA amount field. |
| NT-09 | Send 1 unit of a native token along with the minimum required ADA. | Transaction is submitted; token arrives in Receiver wallet; ADA min-UTxO transferred. |
| NT-10 | Send multiple different tokens in a single transaction. | All tokens arrive in Receiver wallet; single transaction in history. |
| NT-11 | Attempt to send a token without including the minimum required ADA. | Validation error shown; prevents submission. |
| NT-12 | Send entire balance of a token (token should disappear from sender). | Token no longer appears in Sender's token list after confirmation. |
| NT-13 | Attempt to send a token to a Byron address. | Error or warning that Byron addresses cannot receive native tokens. |
| NT-14 | Verify token transaction appears in history with both ADA and token amounts. | History entry lists the token name and amount alongside the ADA. |

---

## 7. Stake Registration and Delegation

### 7.1 Delegation Center Display

| # | Test Case | Expected Result |
|---|-----------|-----------------|
| D-01 | Navigate to **Staking → Delegation center**. | Screen loads; epoch countdown timer ticks correctly. |
| D-02 | Verify current and next epoch numbers are correct against network. | Epoch number matches network status. |
| D-03 | With no Shelley wallets, verify the "Create a Shelley wallet" prompt is shown. | Notification visible; delegate button not available. |
| D-04 | With only Byron wallets, verify they do **not** appear in the delegation list. | Legacy wallet rows are absent. |
| D-05 | With Shelley wallets, verify they are listed with correct balances. | Each Shelley wallet shows its ADA balance and delegation status. |
| D-06 | Undelegated wallet shows "Delegate" action; delegated wallet shows pool ticker. | Labels and actions match delegation state. |

### 7.2 Delegating to a Stake Pool

| # | Test Case | Expected Result |
|---|-----------|-----------------|
| D-07 | Start delegation wizard for a wallet with < 10 ADA. | "Minimum 10 ADA required" warning shown; cannot proceed. |
| D-08 | Start delegation wizard for a wallet with ≥ 10 ADA. | Wizard opens successfully; stake pool list loads. |
| D-09 | Search for a stake pool by ticker. | Matching pools appear in the list; non-matching are filtered out. |
| D-10 | Select a stake pool; verify pool details (ticker, saturation, fee, ROA) are shown. | Pool card displays relevant metadata. |
| D-11 | Complete delegation with correct spending password. | Success confirmation shown; delegation transaction appears in history. |
| D-12 | Submit delegation with wrong spending password. | Error `IncorrectPasswordError`; transaction not sent. |
| D-13 | Verify delegated wallet shows the new pool ticker after transaction confirms (up to 2 epochs). | Pool ticker replaces "Undelegated" in the delegation center. |

### 7.3 Changing Delegation

| # | Test Case | Expected Result |
|---|-----------|-----------------|
| D-14 | Re-delegate an already-delegated wallet to a different pool. | New delegation transaction submitted; ticker updates on confirmation. |

### 7.4 Undelegating / Withdrawing Rewards

| # | Test Case | Expected Result |
|---|-----------|-----------------|
| D-16 | After at least 2 epochs delegated, verify staking rewards appear. | Rewards balance shown in the wallet summary; total balance = ADA + rewards. |
| D-17 | Withdraw rewards by sending a transaction (rewards auto-withdrawn on next tx). | Rewards balance resets to zero; total ADA balance includes former reward amount. |

---

## 8. DRep (Voting Power) Delegation

### Overview

CIP-1694 governance allows ADA holders to delegate their voting power to a DRep, or to register Abstain / No Confidence. Daedalus supports delegating to a DRep; acting as a DRep is out of scope.

### 8.1 Navigation and Display

| # | Test Case | Expected Result |
|---|-----------|-----------------|
| VP-01 | Navigate to the **Voting power delegation** screen (Governance section). | The voting power delegation form is displayed. |
| VP-02 | Verify the wallet dropdown lists all Shelley wallets. | Byron wallets are excluded; all Shelley wallets listed. |
| VP-03 | Verify the vote-type dropdown contains: **Abstain**, **No Confidence**, **Delegate to DRep**. | All three options present. |
| VP-04 | Select **Delegate to DRep** vote type; verify DRep ID input field appears. | Input field shown with correct label; DRep directory link visible. |
| VP-05 | On Preprod, verify the DRep ID label references the preprod environment. | Label text differs from mainnet to indicate test network. |

### 8.2 Input Validation

| # | Test Case | Expected Result |
|---|-----------|-----------------|
| VP-06 | Enter an invalid DRep ID (random string); leave the field. | Inline error "Invalid DRep ID" shown; Submit button disabled. |
| VP-07 | Enter a valid DRep ID (`drep1…` bech32 format). | No error shown; Submit button enabled (if wallet is also selected). |
| VP-08 | Submit without selecting a wallet. | Submit button remains disabled; no error triggered. |
| VP-09 | Select **Abstain** or **No Confidence** — DRep ID field should be hidden. | DRep input disappears; Submit button enabled once wallet is selected. |

### 8.3 Transaction Flow

| # | Test Case | Expected Result |
|---|-----------|-----------------|
| VP-10 | Submit a valid **Delegate to DRep** transaction with correct spending password. | Fee confirmation dialog appears; fee shown; wallet balance sufficient. |
| VP-11 | Confirm the DRep delegation transaction. | Transaction submitted; confirmation dialog closes; history shows the governance tx. |
| VP-12 | Submit **Abstain** delegation with correct spending password. | Transaction succeeds; voting power set to Abstain on-chain. |
| VP-13 | Submit **No Confidence** delegation with correct spending password. | Transaction succeeds; voting power set to No Confidence on-chain. |
| VP-14 | Submit a delegation and immediately attempt the same vote type again (same DRep/option). | Error `same_vote` shown; no transaction submitted. |
| VP-15 | Submit with insufficient ADA balance (wallet near-empty). | Error `not_enough_money` shown; transaction not submitted. |
| VP-16 | Submit with wallet that has no UTxOs. | Error `no_utxos_available` shown. |
| VP-17 | Submit with wrong spending password. | Error from wallet API shown; transaction not submitted. |
| VP-18 | Dismiss fee confirmation dialog by clicking **Cancel / Close**. | Returns to form state; no transaction submitted. |

### 8.4 State Persistence

| # | Test Case | Expected Result |
|---|-----------|-----------------|
| VP-19 | After successful DRep delegation, check on-chain state via a block explorer. | Wallet's stake key is associated with the correct DRep ID. |
| VP-20 | Re-open Daedalus and navigate to governance tab. | Form shows blank state (no cached previous selection shown as pre-filled). |

---

## 9. Hardware Wallets — Ledger

### Devices

- Ledger Nano S (firmware ≥ 2.1)
- Ledger Nano S+ (firmware ≥ 1.1)
- Ledger Nano X (firmware ≥ 2.1, Bluetooth **disabled**; use USB only)

### 9.1 Pairing / First Connection

| # | Test Case | Expected Result |
|---|-----------|-----------------|
| L-01 | Connect Ledger via USB; navigate to **Add wallet → Connect hardware wallet**. | Daedalus detects the device; on-screen instructions guide the user to open the Cardano app. |
| L-02 | Open the Cardano app on the Ledger; follow prompts in Daedalus. | Public key exported; wallet created and named; loading screen transitions to summary. |
| L-03 | Disconnect and reconnect Ledger during pairing. | Daedalus detects disconnect, shows "reconnect device" prompt, recovers on reconnect. |
| L-04 | Connect two Ledger devices simultaneously. | Daedalus handles gracefully (guides user to use one device at a time). |
| L-05 | **macOS** — verify no driver installation is needed (HID support native). | Ledger recognized without any user-installed drivers. |
| L-06 | **Windows** — verify WinUSB/HID driver is in place; device shows in Device Manager. | No "unknown device" in Device Manager. |
| L-07 | **Linux** — verify udev rules installed; user is in `plugdev` group (or equivalent). | Device accessible without `sudo`. |

### 9.2 Receiving ADA on Ledger Wallet

| # | Test Case | Expected Result |
|---|-----------|-----------------|
| L-08 | Navigate to **Receive** tab of the Ledger-backed wallet. | Address is shown; "Verify on device" button present. |
| L-09 | Click **Verify on device**; confirm on Ledger. | Ledger displays the exact same address; Daedalus shows "Verified" confirmation. |
| L-10 | Send ADA from a software wallet to the verified Ledger address. | Transaction appears in Ledger wallet history with correct amount. |

### 9.3 Sending ADA from Ledger Wallet

| # | Test Case | Expected Result |
|---|-----------|-----------------|
| L-11 | Open **Send** tab; fill in receiver address and 2 ADA. | Fee estimate appears; "Next" button active. |
| L-12 | Proceed to confirmation; Daedalus prompts to sign on device. | Ledger screen shows transaction summary (amount, fee, receiver). |
| L-13 | Approve transaction on Ledger. | Transaction submitted; appears in history as Pending, then Confirmed. |
| L-14 | Reject transaction on Ledger. | Daedalus shows "Transaction rejected"; no transaction sent. |
| L-15 | Disconnect Ledger during the signing prompt. | Daedalus detects disconnect; signing dialog shows error; user can retry. |
| L-16 | Send native tokens from Ledger wallet. | Ledger displays multi-asset transaction; approval sends tokens correctly. |

### 9.4 Staking from Ledger Wallet

| # | Test Case | Expected Result |
|---|-----------|-----------------|
| L-17 | Initiate stake pool delegation from a Ledger-backed wallet. | Daedalus prompts to sign delegation certificate on device. |
| L-18 | Approve delegation on Ledger. | Delegation transaction confirmed; pool ticker appears after epoch boundary. |
| L-19 | Reject delegation on Ledger. | Delegation cancelled; no transaction sent; wizard closes. |

### 9.5 DRep Delegation from Ledger Wallet

| # | Test Case | Expected Result |
|---|-----------|-----------------|
| L-20 | Initiate DRep delegation from a Ledger-backed wallet (if Cardano app supports CIP-1694). | Ledger prompts to sign governance certificate. |
| L-21 | Approve DRep delegation on Ledger. | Transaction confirmed on-chain; Ledger wallet's vote delegation updated. |

### 9.6 Nano S+ and Nano X specifics

| # | Test Case | Expected Result |
|---|-----------|-----------------|
| L-22 | Nano X — ensure Bluetooth is **disabled** before test (use USB only). | Nano X recognized as USB HID; no Bluetooth pairing dialog in OS. |
| L-23 | Nano S+ — verify Cardano app has sufficient memory (no "not enough storage" error). | App launches without memory error. |

---

## 10. Hardware Wallets — Trezor

### Devices

- Trezor Model T (firmware ≥ 2.6)
- Trezor Model One and Trezor Safe 3 are **not** supported for Cardano

### 10.1 Pairing / First Connection

| # | Test Case | Expected Result |
|---|-----------|-----------------|
| T-01 | Connect Trezor Model T via USB; navigate to **Add wallet → Connect hardware wallet**. | Trezor Bridge or WebUSB recognized; Daedalus shows device connection prompt. |
| T-02 | Approve export of public key on Trezor. | Wallet created in Daedalus; balance loads. |
| T-03 | **macOS** — verify Trezor Bridge is installed or WebUSB fallback works. | Device recognized without kernel extension. |
| T-04 | **Windows** — verify Trezor Bridge service is running; device appears. | Wallet connects without Trezor Suite being open. |
| T-05 | **Linux** — verify udev rules present for Trezor VID/PID. | Device accessible without `sudo`. |

### 10.2 Receiving ADA on Trezor Wallet

| # | Test Case | Expected Result |
|---|-----------|-----------------|
| T-06 | Navigate to **Receive** tab; click **Verify on device**. | Trezor displays the address on-screen; Daedalus shows "Verified" on confirm. |
| T-07 | Send ADA to the verified Trezor address from a software wallet. | Transaction appears in Trezor wallet history with correct amount. |

### 10.3 Sending ADA from Trezor Wallet

| # | Test Case | Expected Result |
|---|-----------|-----------------|
| T-08 | Build a transaction in Send tab; proceed to signing. | Trezor screen displays amount, receiver; user prompted to confirm. |
| T-09 | Approve on Trezor. | Transaction submitted; Pending → Confirmed in history. |
| T-10 | Reject on Trezor (cancel on device). | Daedalus shows rejection error; no transaction sent. |
| T-11 | Disconnect Trezor during signing prompt. | Error shown; retry button available. |
| T-12 | Send native tokens from Trezor wallet. | Trezor shows multi-asset summary; transaction succeeds on approval. |

### 10.4 Staking from Trezor Wallet

| # | Test Case | Expected Result |
|---|-----------|-----------------|
| T-13 | Initiate stake pool delegation from Trezor-backed wallet. | Trezor prompts for delegation certificate approval. |
| T-14 | Approve delegation on Trezor. | Delegation confirmed; pool ticker appears after epoch boundary. |
| T-15 | Reject delegation on Trezor. | No transaction sent; wizard closes gracefully. |

### 10.5 DRep Delegation from Trezor Wallet

| # | Test Case | Expected Result |
|---|-----------|-----------------|
| T-16 | Initiate DRep delegation from a Trezor-backed wallet. | Trezor prompts to sign governance certificate (if firmware supports CIP-1694). |
| T-17 | Approve DRep delegation on Trezor. | Transaction confirmed; voting power delegation updated on-chain. |

---

## 11. Regression Checklist

Run these quick checks on every platform after the feature areas above are complete.

| # | Check |
|---|-------|
| REG-01 | App launches without crashing on a fresh OS user account. |
| REG-02 | Terms of Use screen shown on first launch; accepted and persisted. |
| REG-03 | Language selection persists across restarts. |
| REG-04 | Number/date format customisation persists. |
| REG-05 | Sidebar navigation between Wallets, Staking, and Governance works. |
| REG-06 | About dialog shows correct version numbers. |
| REG-07 | Newsfeed loads without errors. |
| REG-08 | App update notification appears when a new version is available (staging). |
| REG-09 | Wallet restore (15/24-word mnemonic) completes successfully. |
| REG-10 | Wallet create + recovery phrase verification flow completes. |
| REG-11 | Wallet delete dialog requires typing wallet name; deletes on confirm. |
| REG-12 | UTxO chart renders correctly on wallets with many UTxOs. |
| REG-13 | Node restart from Diagnostic screen completes without data loss. |
| REG-14 | No disk space dialog appears when the storage partition is full. |
| REG-15 | Local time difference warning appears when system clock is > 30 s off. |
| REG-16 | Daedalus does not leave zombie processes after quit. |

---

## 12. Pass/Fail Recording Template

Copy this table into your test tracking spreadsheet or issue tracker.

```
| Test ID | Description                          | Linux x64 | macOS Intel | macOS ARM | Win 10 | Win 11 | Notes |
|---------|--------------------------------------|-----------|-------------|-----------|--------|--------|-------|
| M-01    | Mithril decision screen on first run |           |             |           |        |        |       |
| M-02    | Default storage path correct         |           |             |           |        |        |       |
...
```

### Status Codes

| Code | Meaning |
|------|---------|
| `P`  | Pass |
| `F`  | Fail — link to bug report |
| `B`  | Blocked — dependency not ready |
| `S`  | Skip — not applicable for this build |
| `N`  | Not tested yet |

---

*End of test plan.*
