---
description: Ledger and Trezor hardware wallet development
---

# Hardware Wallets Workflow

This workflow guides development and testing of hardware wallet integration in Daedalus.

## Overview

Daedalus supports two hardware wallet types:
- **Ledger** (Nano S, Nano X, Nano S Plus)
- **Trezor** (Model T, Model One, Safe 3)

Hardware wallets provide secure transaction signing where private keys never leave the device.

---

## Architecture

```
┌─────────────────────────────────────────────────────────────────────────┐
│                          Renderer Process                               │
│  ┌──────────────────────────────────────────────────────────────────┐   │
│  │  HardwareWalletsStore (120KB)                                    │   │
│  │  - Device connection state                                       │   │
│  │  - Extended public key caching                                   │   │
│  │  - Transaction signing flow                                      │   │
│  └──────────────────────────────────────────────────────────────────┘   │
│                                    │                                    │
│                                    │ IPC Channels                       │
│                                    ▼                                    │
├─────────────────────────────────────────────────────────────────────────┤
│                           Main Process                                  │
│  ┌──────────────────────────────────────────────────────────────────┐   │
│  │  source/main/ipc/hardwareWallets/                                │   │
│  │  ├── ledger/                                                     │   │
│  │  │   ├── deviceDetection/  # USB HID detection                   │   │
│  │  │   └── api.ts            # Ledger API calls                    │   │
│  │  └── trezor/               # (via Trezor Connect)                │   │
│  └──────────────────────────────────────────────────────────────────┘   │
│                                    │                                    │
│                                    ▼                                    │
│  ┌─────────────────────────────────────────────────────────────────┐    │
│  │  Libraries                                                      │    │
│  │  - @cardano-foundation/ledgerjs-hw-app-cardano                  │    │
│  │  - @ledgerhq/hw-transport-node-hid                              │    │
│  │  - @trezor/connect                                              │    │
│  └─────────────────────────────────────────────────────────────────┘    │
└─────────────────────────────────────────────────────────────────────────┘
                                     │
                                     ▼
                         ┌─────────────────────┐
                         │  Hardware Device    │
                         │  (USB HID)          │
                         └─────────────────────┘
```

---

## Key Files

### Main Process

| File                                                      | Purpose                  |
|-----------------------------------------------------------|--------------------------|
| `source/main/ipc/hardwareWallets/ledger/`                 | Ledger integration       |
| `source/main/ipc/hardwareWallets/ledger/deviceDetection/` | USB device detection     |
| `source/main/trezor/connection.ts`                        | Trezor Connect setup     |
| `source/main/trezor/manifest.ts`                          | Trezor app manifest      |
| `source/main/ipc/createHardwareWalletIPCChannels.ts`      | IPC channel registration |

### Renderer Process

| File                                                     | Purpose            |
|----------------------------------------------------------|--------------------|
| `source/renderer/app/stores/HardwareWalletsStore.ts`     | Main state (120KB) |
| `source/renderer/app/components/hardware-wallet/`        | UI components      |

---

## IPC Channels

### Device Connection

| Channel                                  | Direction | Purpose              |
|------------------------------------------|-----------|----------------------|
| `GET_HARDWARE_WALLET_TRANSPORT_CHANNEL`  | R → M     | Connect to device    |
| `GET_HARDWARE_WALLET_CONNECTION_CHANNEL` | M → R     | Device status events |
| `WAIT_FOR_LEDGER_DEVICES`                | R → M     | Wait for Ledger      |

### Wallet Operations

| Channel                           | Direction | Purpose                      |
|-----------------------------------|-----------|------------------------------|
| `GET_EXTENDED_PUBLIC_KEY_CHANNEL` | R → M     | Get xpub for wallet creation |
| `GET_CARDANO_ADA_APP_CHANNEL`     | R → M     | Check Cardano app is open    |
| `DERIVE_ADDRESS_CHANNEL`          | R → M     | Derive address from xpub     |
| `SHOW_ADDRESS_CHANNEL`            | R → M     | Verify address on device     |

### Transaction Signing

| Channel                           | Direction | Purpose          |
|-----------------------------------|-----------|------------------|
| `SIGN_TRANSACTION_LEDGER_CHANNEL` | R → M     | Sign with Ledger |
| `SIGN_TRANSACTION_TREZOR_CHANNEL` | R → M     | Sign with Trezor |

### Initialization

| Channel                           | Direction | Purpose                      |
|-----------------------------------|-----------|------------------------------|
| `GET_INIT_LEDGER_CONNECT_CHANNEL` | R → M     | Initialize Ledger connection |
| `GET_INIT_TREZOR_CONNECT_CHANNEL` | R → M     | Initialize Trezor Connect    |
| `RESET_ACTION_TREZOR_CHANNEL`     | R → M     | Reset pending Trezor action  |

---

## Ledger Integration

### Device Detection

```typescript
// source/main/ipc/hardwareWallets/ledger/deviceDetection/

// Event-driven detection (preferred)
eventDrivenDetection.ts  // Uses USB events

// Polling fallback
pollingDrivenDetection.ts  // Periodic USB polling

// Device tracking
deviceTracker.ts  // Tracks connected devices
```

### Signing Flow

1. **User initiates transaction** in UI
2. **Store prepares transaction** with inputs/outputs
3. **IPC sends to main process** via `SIGN_TRANSACTION_LEDGER_CHANNEL`
4. **Main opens Ledger transport** via USB HID
5. **Transaction sent to device** for user confirmation
6. **User confirms on device** (physical button press)
7. **Signed transaction returned** to renderer
8. **Transaction submitted** to cardano-wallet

---

## Trezor Integration

### Trezor Connect

Trezor uses `@trezor/connect` which handles:
- Device communication
- Popup for user confirmation
- Multi-device support

### Setup

```typescript
// source/main/trezor/connection.ts
import TrezorConnect from '@trezor/connect';

TrezorConnect.init({
  manifest: {
    email: 'support@iohk.io',
    appUrl: 'https://daedaluswallet.io',
  },
});
```

### Signing Flow

1. **User initiates transaction** in UI
2. **Store prepares transaction** with inputs/outputs
3. **IPC sends to main process** via `SIGN_TRANSACTION_TREZOR_CHANNEL`
4. **TrezorConnect.cardanoSignTransaction()** called
5. **Trezor popup appears** for user confirmation
6. **User confirms on device** (touchscreen/buttons)
7. **Signed transaction returned** to renderer
8. **Transaction submitted** to cardano-wallet

---

## Development

### Testing Without Device

Use mock IPC responses:

```typescript
// In test/development
jest.mock('electron', () => ({
  ipcMain: {
    handle: jest.fn(),
  },
}));

// Mock signing response
const mockSignedTx = {
  signedTransactionChunks: ['chunk1', 'chunk2'],
};
```

### Hardware Wallet Test Script

```bash
yarn test:hardware-wallets
```

Runs `hardware-wallet-tests/index.ts` for interactive testing.

### Debugging

Enable verbose logging:

```typescript
// In main process
import { logger } from '../utils/logging';

logger.info('Hardware wallet operation', {
  channel: 'SIGN_TRANSACTION_LEDGER_CHANNEL',
  devicePath: transport.devicePath,
});
```

---

## Common Issues

### Device Not Detected

**Ledger:**
1. Ensure Ledger is unlocked (PIN entered)
2. Open Cardano app on device
3. Check USB cable/port
4. On Linux, verify udev rules

**Trezor:**
1. Ensure device is unlocked
2. Close Trezor Suite (may conflict)
3. Check USB connection

### Linux USB Permissions

Create udev rules for Ledger:

```bash
# /etc/udev/rules.d/20-hw1.rules
SUBSYSTEM=="usb", ATTR{idVendor}=="2c97", MODE="0666"
SUBSYSTEM=="usb", ATTR{idVendor}=="2581", MODE="0666"
```

For Trezor:

```bash
# /etc/udev/rules.d/51-trezor.rules
SUBSYSTEM=="usb", ATTR{idVendor}=="534c", MODE="0666"
SUBSYSTEM=="usb", ATTR{idVendor}=="1209", MODE="0666"
```

Then reload:
```bash
sudo udevadm control --reload-rules
sudo udevadm trigger
```

### Transaction Rejected

**"Ledger device: Action rejected by user"**
- User cancelled on device
- Transaction requires confirmation on device screen

**"Transaction too large"**
- Transaction has too many inputs/outputs
- Split into multiple transactions

### Timeout Errors

**"Device communication timeout"**
- Device may have locked (auto-lock)
- Re-enter PIN and try again

---

## HardwareWalletsStore

The main store (`120KB`) manages:

### State

```typescript
@observable devices: Map<string, HardwareWalletDevice>
@observable activeDevicePath: string | null
@observable isTransactionPending: boolean
@observable extendedPublicKeys: Map<string, ExtendedPublicKey>
```

### Key Methods

```typescript
// Device connection
connectDevice(deviceType: 'ledger' | 'trezor'): Promise<void>
disconnectDevice(devicePath: string): void

// Wallet creation
getExtendedPublicKey(devicePath: string): Promise<ExtendedPublicKey>
createHardwareWallet(params: CreateWalletParams): Promise<Wallet>

// Transaction signing
signTransaction(params: SignTxParams): Promise<SignedTransaction>

// Address verification
verifyAddress(params: VerifyAddressParams): Promise<void>
```

---

## Adding New Hardware Wallet Support

1. **Create main process integration** in `source/main/ipc/hardwareWallets/`
2. **Add IPC channels** in `source/common/ipc/api.ts`
3. **Update HardwareWalletsStore** with new device type
4. **Add UI components** for device-specific flows
5. **Add device detection** for USB HID identification
6. **Update tests** in `hardware-wallet-tests/`

---

## Security Considerations

1. **Private keys never leave device** - All signing on hardware
2. **Address verification** - Always verify receive addresses on device
3. **Transaction display** - User sees transaction details on device screen
4. **Physical confirmation** - Requires button press on device
5. **PIN protection** - Device locks after incorrect PINs
