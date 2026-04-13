---
description: Electron main process development
---

# Electron Workflow

This workflow guides development on the Electron main process in Daedalus.

## Overview

The Electron main process (`source/main/`) handles:
- **cardano-node and cardano-wallet lifecycle** - Starting, stopping, monitoring
- **IPC communication** - Bridging renderer and native capabilities
- **Hardware wallets** - Ledger and Trezor integration
- **Native menus** - Platform-specific application menus
- **Window management** - Main window creation and persistence

---

## Directory Structure

```
source/main/
├── index.ts                 # Entry point, app initialization
├── environment.ts           # Environment detection
├── config.ts                # Configuration
├── preload.ts               # Preload script for renderer
├── cardano/
│   ├── CardanoNode.ts       # Node process management
│   ├── CardanoWalletLauncher.ts  # Wallet backend launcher
│   ├── CardanoSelfnodeLauncher.ts # Self-node for testing
│   ├── config.ts            # Cardano configuration
│   ├── setup.ts             # Setup procedures
│   └── utils.ts             # Cardano utilities
├── ipc/
│   ├── cardano.ipc.ts       # Cardano-related IPC handlers
│   ├── lib/
│   │   ├── MainIpcChannel.ts      # IPC channel class
│   │   └── MainIpcConversation.ts # IPC conversation class
│   ├── hardwareWallets/
│   │   └── ledger/
│   │       └── deviceDetection/   # Ledger device detection
│   ├── get-logs.ts          # Log retrieval
│   ├── compress-logs.ts     # Log compression
│   └── ... (40+ IPC handlers)
├── menus/
│   ├── osx.ts               # macOS menu
│   ├── win-linux.ts         # Windows/Linux menu
│   └── submenuBuilders.ts   # Shared menu builders
├── trezor/
│   ├── connection.ts        # Trezor Connect integration
│   └── manifest.ts          # Trezor manifest
├── windows/
│   ├── main.ts              # Main window creation
│   └── windowBounds.ts      # Window bounds persistence
└── utils/
    └── ... (utilities)
```

---

## Quick Commands

### Development Mode

```bash
# Start development (main + renderer)
yarn dev

# Build main process only (watch mode)
yarn dev:main
```

### Production Build

```bash
# Build main process
yarn build:main

# Build everything
yarn build
```

---

## Main Process Entry Point

`source/main/index.ts` is the entry point:

```typescript
// Key initialization steps:
1. Configure Electron app settings
2. Create main window
3. Start cardano-node and cardano-wallet
4. Register IPC handlers
5. Set up native menus
```

### App Lifecycle Events

```typescript
app.on('ready', async () => {
  // Create main window
  // Start Cardano processes
  // Register IPC handlers
});

app.on('window-all-closed', () => {
  // Cleanup and quit
});

app.on('before-quit', async () => {
  // Stop Cardano processes
  // Save window state
});
```

---

## Cardano Process Management

### CardanoNode.ts

Manages the cardano-node process:

```typescript
class CardanoNode {
  start(): Promise<void>    // Start node
  stop(): Promise<void>     // Stop node
  restart(): Promise<void>  // Restart node
  getState(): CardanoNodeState  // Get current state
}
```

### CardanoWalletLauncher.ts

Manages the cardano-wallet process:

```typescript
class CardanoWalletLauncher {
  start(): Promise<void>    // Start wallet backend
  stop(): Promise<void>     // Stop wallet backend
  getTlsConfig(): TlsConfig // Get TLS configuration
}
```

### Node States

| State      | Description                 |
|------------|-----------------------------|
| `starting` | Node is starting up         |
| `running`  | Node is running and synced  |
| `syncing`  | Node is syncing blockchain  |
| `stopping` | Node is shutting down       |
| `stopped`  | Node is not running         |
| `crashed`  | Node crashed unexpectedly   |

---

## IPC Handler Development

### Creating a New IPC Handler

1. **Define channel in `source/common/ipc/api.ts`:**

```typescript
export const MY_CHANNEL = 'MY_CHANNEL';
export type MyChannelRequest = { param: string };
export type MyChannelResponse = { result: string };
```

2. **Create handler in `source/main/ipc/`:**

```typescript
// source/main/ipc/my-channel.ts
import { MainIpcChannel } from './lib/MainIpcChannel';
import {
  MY_CHANNEL,
  MyChannelRequest,
  MyChannelResponse,
} from '../../common/ipc/api';

const myChannel = new MainIpcChannel<MyChannelRequest, MyChannelResponse>(
  MY_CHANNEL
);

export default () => {
  myChannel.onReceive(async (request) => {
    // Handle request
    return { result: `Processed: ${request.param}` };
  });
};
```

3. **Register handler in main index:**

```typescript
// source/main/index.ts
import myChannelHandler from './ipc/my-channel';

// In initialization:
myChannelHandler();
```

### IPC Channel Types

| Type                 | Class                     | Use Case                       |
|----------------------|---------------------------|--------------------------------|
| **Request/Response** | `MainIpcChannel`          | Single request, single response |
| **Conversation**     | `MainIpcConversation`     | Bidirectional communication    |
| **Broadcast**        | Direct `webContents.send` | Main → All renderers           |

---

## Hardware Wallet Integration

### Ledger

Ledger integration uses `@cardano-foundation/ledgerjs-hw-app-cardano`:

```
source/main/ipc/hardwareWallets/ledger/
├── deviceDetection/
│   ├── index.ts
│   ├── deviceDetection.ts
│   ├── eventDrivenDetection.ts
│   ├── pollingDrivenDetection.ts
│   └── deviceTracker.ts
└── api.ts
```

Key channels:
- `GET_HARDWARE_WALLET_TRANSPORT_CHANNEL` - Connect to device
- `GET_EXTENDED_PUBLIC_KEY_CHANNEL` - Get xpub
- `SIGN_TRANSACTION_LEDGER_CHANNEL` - Sign transaction

### Trezor

Trezor integration uses `@trezor/connect`:

```
source/main/trezor/
├── connection.ts    # Trezor Connect setup
└── manifest.ts      # Trezor manifest
```

Key channels:
- `GET_INIT_TREZOR_CONNECT_CHANNEL` - Initialize Trezor Connect
- `SIGN_TRANSACTION_TREZOR_CHANNEL` - Sign transaction
- `RESET_ACTION_TREZOR_CHANNEL` - Reset pending actions

---

## Native Menus

### Platform-Specific Menus

| Platform        | File                        |
|-----------------|-----------------------------|
| macOS           | `source/main/menus/osx.ts`  |
| Windows/Linux   | `source/main/menus/win-linux.ts` |

### Menu Actions

Menus trigger actions via IPC:
- `SHOW_UI_PART_CHANNEL` - Show UI component
- `TOGGLE_UI_PART_CHANNEL` - Toggle UI component
- `REBUILD_APP_MENU_CHANNEL` - Rebuild menu (e.g., language change)

### Updating Menus

```typescript
// Rebuild menu after settings change
mainWindow.webContents.send(REBUILD_APP_MENU_CHANNEL, {
  isNavigationEnabled: true,
  walletSettingsState: 'enabled',
});
```

---

## Window Management

### Main Window Creation

```typescript
// source/main/windows/main.ts
const mainWindow = new BrowserWindow({
  width: 1150,
  height: 870,
  webPreferences: {
    preload: path.join(__dirname, 'preload.js'),
    contextIsolation: true,
    nodeIntegration: false,
  },
});
```

### Window Bounds Persistence

Window position and size are saved and restored:
- `source/main/windows/windowBounds.ts`

---

## Preload Script

`source/main/preload.ts` exposes limited APIs to renderer:

```typescript
contextBridge.exposeInMainWorld('daedalus', {
  // Exposed API
});
```

**Security note:** Keep exposed API minimal. Use IPC for all main process access.

---

## Logging

Main process logging uses `electron-log-daedalus`:

```typescript
import { logger } from '../utils/logging';

logger.info('Main process started');
logger.error('Error occurred', { error });
```

Log files are stored in the app data directory.

---

## Debugging

### Debug Main Process

```bash
# Start with Chrome DevTools
yarn dev

# In another terminal, attach debugger
# Chrome: chrome://inspect
```

### Inspect Main Process

Add to `source/main/index.ts`:
```typescript
app.commandLine.appendSwitch('inspect', '5858');
```

Then connect debugger to `localhost:5858`.

---

## Common Patterns

### Error Handling in IPC

```typescript
myChannel.onReceive(async (request) => {
  try {
    const result = await doSomething(request);
    return { success: true, data: result };
  } catch (error) {
    logger.error('Channel error', { error });
    throw error; // Will be serialized to renderer
  }
});
```

### Async Initialization

```typescript
app.on('ready', async () => {
  try {
    await initializeCardano();
    await createMainWindow();
    registerIpcHandlers();
  } catch (error) {
    logger.error('Initialization failed', { error });
    app.quit();
  }
});
```

### Graceful Shutdown

```typescript
app.on('before-quit', async (event) => {
  event.preventDefault();
  
  try {
    await cardanoNode.stop();
    await cardanoWallet.stop();
  } finally {
    app.exit(0);
  }
});
```

---

## Troubleshooting

### Cardano Node Won't Start

**Problem:** Node fails to start
**Solution:**
1. Check logs in app data directory
2. Verify Nix shell is active
3. Check if port 8090 is available
4. Ensure enough disk space

### IPC Handler Not Called

**Problem:** Renderer IPC call doesn't reach main
**Solution:**
1. Verify channel name matches exactly
2. Check handler is registered in main index
3. Ensure preload script is loaded

### Hardware Wallet Not Detected

**Problem:** Ledger/Trezor not found
**Solution:**
1. Check USB permissions (Linux: udev rules)
2. Verify device is unlocked
3. Check Cardano app is open (Ledger)
4. Try different USB port

---

## Testing Main Process

### Mock IPC for Testing

```typescript
import { ipcMain } from 'electron';

// In test setup
ipcMain.handle(MY_CHANNEL, (event, request) => {
  return mockResponse;
});
```

### Test Cardano Integration

Use the selfnode for isolated testing:
```bash
yarn nix:selfnode
yarn dev
```
