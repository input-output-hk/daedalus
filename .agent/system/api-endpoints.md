# API Endpoints Reference

> **Source of truth for API contracts in Daedalus.**

This document describes the APIs used by Daedalus: the cardano-wallet REST API and the Electron IPC channels.

---

## API Overview

Daedalus communicates with two types of APIs:

| API Type                      | Purpose                                | Location         |
|-------------------------------|----------------------------------------|------------------|
| **cardano-wallet REST API**   | Wallet operations, blockchain queries  | `localhost:8090` |
| **Electron IPC Channels**     | Main/renderer process communication    | In-process       |

---

## cardano-wallet REST API

The cardano-wallet backend provides a REST API for all wallet and blockchain operations.

### Base URL

```
https://localhost:8090/v2
```

The API uses mutual TLS authentication. The TLS config is obtained via the `CARDANO_TLS_CONFIG_CHANNEL` IPC channel.

### API Client

The API client is implemented in:
```
source/renderer/app/api/api.ts (104KB)
```

---

## Wallet Endpoints

### GET `/v2/wallets`
List all wallets.

Delegation status values in `delegation.active.status` include:
- `delegating`
- `not_delegating`
- `voting`
- `voting_and_delegating`

**Response:**
```json
[
  {
    "id": "wallet-id",
    "name": "My Wallet",
    "balance": {
      "available": { "quantity": 1000000, "unit": "lovelace" },
      "total": { "quantity": 1000000, "unit": "lovelace" }
    },
    "state": { "status": "ready" },
    "delegation": { "active": { "status": "not_delegating" } }
  }
]
```

### POST `/v2/wallets`
Create or restore a wallet.

**Request:**
```json
{
  "name": "My Wallet",
  "mnemonic_sentence": ["word1", "word2", "...24 words"],
  "passphrase": "spending-password"
}
```

### GET `/v2/wallets/{walletId}`
Get wallet details.

### DELETE `/v2/wallets/{walletId}`
Delete a wallet.

### PUT `/v2/wallets/{walletId}/passphrase`
Update wallet spending password.

---

## Address Endpoints

### GET `/v2/wallets/{walletId}/addresses`
List wallet addresses.

**Query Parameters:**
| Param   | Type   | Description              |
|---------|--------|--------------------------|
| `state` | string | Filter: `used`, `unused` |

**Response:**
```json
[
  {
    "id": "addr1...",
    "state": "unused",
    "derivation_path": ["1852H", "1815H", "0H", "0", "0"]
  }
]
```

### POST `/v2/wallets/{walletId}/addresses`
Generate a new address (for Byron wallets).

---

## Transaction Endpoints

### GET `/v2/wallets/{walletId}/transactions`
List wallet transactions.

**Query Parameters:**
| Param           | Type     | Description                 |
|-----------------|----------|-----------------------------|
| `start`         | datetime | Filter from date            |
| `end`           | datetime | Filter to date              |
| `order`         | string   | `ascending` or `descending` |
| `minWithdrawal` | int      | Minimum withdrawal amount   |

**Response:**
```json
[
  {
    "id": "tx-hash",
    "amount": { "quantity": -1500000, "unit": "lovelace" },
    "fee": { "quantity": 200000, "unit": "lovelace" },
    "inserted_at": { "time": "2024-01-15T12:00:00Z" },
    "status": "in_ledger",
    "direction": "outgoing",
    "inputs": [...],
    "outputs": [...]
  }
]
```

### POST `/v2/wallets/{walletId}/transactions`
Create and submit a transaction.

**Request:**
```json
{
  "payments": [
    {
      "address": "addr1...",
      "amount": { "quantity": 1000000, "unit": "lovelace" }
    }
  ],
  "passphrase": "spending-password"
}
```

### GET `/v2/wallets/{walletId}/transactions/{txId}`
Get transaction details.

### DELETE `/v2/wallets/{walletId}/transactions/{txId}`
Forget a pending transaction.

---

## Transaction Fees

### POST `/v2/wallets/{walletId}/payment-fees`
Estimate transaction fees.

**Request:**
```json
{
  "payments": [
    {
      "address": "addr1...",
      "amount": { "quantity": 1000000, "unit": "lovelace" }
    }
  ]
}
```

**Response:**
```json
{
  "estimated_min": { "quantity": 170000, "unit": "lovelace" },
  "estimated_max": { "quantity": 200000, "unit": "lovelace" }
}
```

---

## Staking Endpoints

### GET `/v2/stake-pools`
List available stake pools.

**Query Parameters:**
| Param   | Type | Description                      |
|---------|------|----------------------------------|
| `stake` | int  | Amount to delegate (for ranking) |

**Response:**
```json
[
  {
    "id": "pool-id",
    "metrics": {
      "saturation": 0.5,
      "produced_blocks": { "quantity": 1000 },
      "non_myopic_member_rewards": { "quantity": 5000000 }
    },
    "metadata": {
      "name": "Pool Name",
      "ticker": "POOL",
      "description": "...",
      "homepage": "https://..."
    }
  }
]
```

### PUT `/v2/wallets/{walletId}/delegations`
Delegate to a stake pool or quit staking.

**Request (delegate):**
```json
{
  "delegations": [{ "join": { "pool": "pool-id" } }],
  "passphrase": "spending-password"
}
```

**Request (quit):**
```json
{
  "delegations": [{ "quit": {} }],
  "passphrase": "spending-password"
}
```

---

## Native Assets

### GET `/v2/wallets/{walletId}/assets`
List native assets in wallet.

**Response:**
```json
[
  {
    "policy_id": "...",
    "asset_name": "...",
    "quantity": 1000,
    "metadata": { "name": "Token Name", "ticker": "TKN" }
  }
]
```

---

## Network Endpoints

### GET `/v2/network/information`
Get network sync status.

**Response:**
```json
{
  "sync_progress": { "status": "ready" },
  "node_era": "babbage",
  "node_tip": {
    "epoch_number": 400,
    "slot_number": 12345,
    "height": { "quantity": 1000000 }
  },
  "network_tip": {
    "epoch_number": 400,
    "slot_number": 12346
  }
}
```

### GET `/v2/network/clock`
Get network clock offset.

### GET `/v2/network/parameters`
Get current network parameters.

---

## Hardware Wallet Endpoints

### POST `/v2/wallets` (with account public key)
Create hardware wallet.

**Request:**
```json
{
  "name": "Ledger Wallet",
  "account_public_key": "xpub...",
  "address_pool_gap": 20
}
```

### POST `/v2/wallets/{walletId}/transactions-sign-hw`
Build transaction for hardware wallet signing.

---

## Voting Endpoints

### GET `/v2/wallets/{walletId}/voting/registrations`
Get voting registrations.

### POST `/v2/wallets/{walletId}/voting/registrations`
Create voting registration.

---

## Error Responses

All errors follow this format:

```json
{
  "code": "error_code",
  "message": "Human readable message"
}
```

### Common Error Codes

| Code                     | HTTP Status | Description                    |
|--------------------------|-------------|--------------------------------|
| `no_such_wallet`         | 404         | Wallet not found               |
| `wrong_passphrase`       | 403         | Invalid spending password      |
| `not_enough_money`       | 403         | Insufficient funds             |
| `utxo_too_small`         | 403         | Output below minimum           |
| `transaction_is_too_big` | 403         | Transaction exceeds size limit |
| `network_unreachable`    | 503         | Node not synced                |
| `pool_not_found`         | 404         | Stake pool not found           |

---

## Electron IPC Channels

Daedalus uses type-safe IPC channels for main/renderer communication. All channels are defined in `source/common/ipc/api.ts`.

### Channel Categories

| Category             | Channels     | Purpose                     |
|----------------------|--------------|-----------------------------|
| **Cardano**          | 8 channels   | Node/wallet lifecycle       |
| **Hardware Wallets** | 12 channels  | Ledger/Trezor operations    |
| **File Operations**  | 6 channels   | Dialogs, PDF/CSV generation |
| **Logging**          | 4 channels   | Log management              |
| **Downloads**        | 6 channels   | App updates                 |
| **System**           | 8 channels   | Disk space, locale, paths   |

### Cardano Channels

| Channel                             | Direction       | Purpose              |
|-------------------------------------|-----------------|----------------------|
| `CARDANO_STATE_CHANNEL`             | Main → Renderer | Node state updates   |
| `CARDANO_TLS_CONFIG_CHANNEL`        | Renderer → Main | Get TLS config       |
| `CARDANO_RESTART_CHANNEL`           | Renderer → Main | Restart node         |
| `CARDANO_AWAIT_UPDATE_CHANNEL`      | Renderer → Main | Prepare for update   |
| `GET_CACHED_CARDANO_STATUS_CHANNEL` | Renderer → Main | Get cached status    |
| `SET_CACHED_CARDANO_STATUS_CHANNEL` | Renderer → Main | Update cached status |
| `CARDANO_FAULT_INJECTION_CHANNEL`   | Renderer → Main | Testing only         |
| `GET_BLOCK_SYNC_PROGRESS_CHANNEL`   | Renderer → Main | Sync progress        |

### Hardware Wallet Channels

| Channel                                  | Direction       | Purpose           |
|------------------------------------------|-----------------|-------------------|
| `GET_HARDWARE_WALLET_TRANSPORT_CHANNEL`  | Renderer → Main | Connect device    |
| `GET_HARDWARE_WALLET_CONNECTION_CHANNEL` | Main → Renderer | Device events     |
| `GET_EXTENDED_PUBLIC_KEY_CHANNEL`        | Renderer → Main | Get xpub          |
| `GET_CARDANO_ADA_APP_CHANNEL`            | Renderer → Main | Check Cardano app |
| `SIGN_TRANSACTION_LEDGER_CHANNEL`        | Renderer → Main | Sign with Ledger  |
| `SIGN_TRANSACTION_TREZOR_CHANNEL`        | Renderer → Main | Sign with Trezor  |
| `DERIVE_ADDRESS_CHANNEL`                 | Renderer → Main | Derive address    |
| `SHOW_ADDRESS_CHANNEL`                   | Renderer → Main | Verify on device  |
| `GET_INIT_TREZOR_CONNECT_CHANNEL`        | Renderer → Main | Init Trezor       |
| `GET_INIT_LEDGER_CONNECT_CHANNEL`        | Renderer → Main | Init Ledger       |
| `DERIVE_XPUB_CHANNEL`                    | Renderer → Main | Derive xpub       |
| `RESET_ACTION_TREZOR_CHANNEL`            | Renderer → Main | Reset Trezor      |

### File Operation Channels

| Channel                         | Direction       | Purpose                   |
|---------------------------------|-----------------|---------------------------|
| `SHOW_OPEN_DIALOG_CHANNEL`      | Renderer → Main | Open file dialog          |
| `SHOW_SAVE_DIALOG_CHANNEL`      | Renderer → Main | Save file dialog          |
| `GENERATE_PAPER_WALLET_CHANNEL` | Renderer → Main | Generate paper wallet PDF |
| `GENERATE_ADDRESS_PDF_CHANNEL`  | Renderer → Main | Generate address PDF      |
| `GENERATE_VOTING_PDF_CHANNEL`   | Renderer → Main | Generate voting PDF       |
| `GENERATE_CSV_CHANNEL`          | Renderer → Main | Export transactions CSV   |

### Logging Channels

| Channel                          | Direction       | Purpose              |
|----------------------------------|-----------------|----------------------|
| `GET_LOGS_CHANNEL`               | Renderer → Main | Get log files        |
| `COMPRESS_LOGS_CHANNEL`          | Renderer → Main | Compress logs        |
| `DOWNLOAD_LOGS_CHANNEL`          | Renderer → Main | Save compressed logs |
| `SET_STATE_SNAPSHOT_LOG_CHANNEL` | Renderer → Main | Log state snapshot   |

### Download Channels

| Channel                     | Direction       | Purpose             |
|-----------------------------|-----------------|---------------------|
| `REQUEST_DOWNLOAD`          | Renderer → Main | Start download      |
| `RESUME_DOWNLOAD`           | Renderer → Main | Resume download     |
| `DELETE_DOWNLOADED_FILE`    | Renderer → Main | Delete file         |
| `GET_DOWNLOAD_LOCAL_DATA`   | Renderer → Main | Get download info   |
| `GET_DOWNLOADS_LOCAL_DATA`  | Renderer → Main | List downloads      |
| `CLEAR_DOWNLOAD_LOCAL_DATA` | Renderer → Main | Clear download data |

### System Channels

| Channel                              | Direction       | Purpose              |
|--------------------------------------|-----------------|----------------------|
| `GET_DISK_SPACE_STATUS_CHANNEL`      | Renderer → Main | Check disk space     |
| `GET_STATE_DIRECTORY_PATH_CHANNEL`   | Renderer → Main | Get state directory  |
| `GET_DESKTOP_DIRECTORY_PATH_CHANNEL` | Renderer → Main | Get desktop path     |
| `GET_SYSTEM_LOCALE_CHANNEL`          | Renderer → Main | Get system locale    |
| `GET_GPU_STATUS_CHANNEL`             | Renderer → Main | Get GPU status       |
| `OPEN_EXTERNAL_URL_CHANNEL`          | Renderer → Main | Open URL in browser  |
| `OPEN_LOCAL_DIRECTORY_CHANNEL`       | Renderer → Main | Open in file manager |
| `INTROSPECT_ADDRESS_CHANNEL`         | Renderer → Main | Analyze address      |

### UI Control Channels

| Channel                    | Direction       | Purpose             |
|----------------------------|-----------------|---------------------|
| `SHOW_UI_PART_CHANNEL`     | Main → Renderer | Show UI element     |
| `TOGGLE_UI_PART_CHANNEL`   | Main → Renderer | Toggle UI element   |
| `REBUILD_APP_MENU_CHANNEL` | Renderer → Main | Rebuild native menu |

---

## IPC Channel Types

Each channel has typed request and response types defined in `source/common/ipc/api.ts`:

```typescript
// Example: Hardware wallet transport
export const GET_HARDWARE_WALLET_TRANSPORT_CHANNEL = 'GET_HARDWARE_WALLET_TRANSPORT_CHANNEL';
export type getHardwareWalletTransportRendererRequest = HardwareWalletTransportDeviceRequest;
export type getHardwareWalletTransportMainResponse = HardwareWalletTransportDeviceResponse;

// Example: Cardano state
export const CARDANO_STATE_CHANNEL = 'CARDANO_STATE_CHANNEL';
export type CardanoStateRendererRequest = void;
export type CardanoStateRendererResponse = CardanoNodeState;
```

---

## API Documentation Resources

- **cardano-wallet OpenAPI**: https://cardano-foundation.github.io/cardano-wallet/api/edge/
- **IPC Channel Definitions**: `source/common/ipc/api.ts`
- **Type Definitions**: `source/common/types/`
