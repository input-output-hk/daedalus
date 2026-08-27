---
description: IPC communication patterns between main and renderer
---

# IPC Workflow

This workflow guides development of IPC (Inter-Process Communication) between Electron's main and renderer processes in Daedalus.

## Overview

Daedalus uses type-safe IPC channels for communication between:
- **Main Process** (Node.js) - Handles Cardano, hardware wallets, system access
- **Renderer Process** (React) - User interface

All channels are defined in `source/common/ipc/api.ts`.

---

## IPC Architecture

```
┌─────────────────────────────────────────────────────────────────────────┐
│                          Renderer Process                               │
│  ┌─────────────────────────────────────────────────────────────────┐    │
│  │  source/renderer/app/ipc/                                       │    │
│  │  └── RendererIpcChannel / RendererIpcConversation               │    │
│  └─────────────────────────────────────────────────────────────────┘    │
│                                    │                                    │
│                                    │ ipcRenderer.send / invoke          │
│                                    ▼                                    │
├─────────────────────────────────────────────────────────────────────────┤
│                          Preload Script                                 │
│                     source/main/preload.ts                              │
├─────────────────────────────────────────────────────────────────────────┤
│                                    │                                    │
│                                    │ ipcMain.handle / on                │
│                                    ▼                                    │
│  ┌─────────────────────────────────────────────────────────────────┐    │
│  │  source/main/ipc/                                               │    │
│  │  └── MainIpcChannel / MainIpcConversation                       │    │
│  └─────────────────────────────────────────────────────────────────┘    │
│                           Main Process                                  │
└─────────────────────────────────────────────────────────────────────────┘
```

---

## Quick Reference

### Create a New IPC Channel

1. Define channel in `source/common/ipc/api.ts`
2. Create handler in `source/main/ipc/`
3. Create client in `source/renderer/app/ipc/`
4. Register handler in main process

---

## Step-by-Step: Creating an IPC Channel

### Step 1: Define Channel Contract

```typescript
// source/common/ipc/api.ts

export const MY_NEW_CHANNEL = 'MY_NEW_CHANNEL';

// Request type (renderer → main)
export type MyNewChannelRequest = {
  param1: string;
  param2: number;
};

// Response type (main → renderer)
export type MyNewChannelResponse = {
  result: string;
  success: boolean;
};
```

### Step 2: Create Main Process Handler

```typescript
// source/main/ipc/my-new-channel.ts

import { MainIpcChannel } from './lib/MainIpcChannel';
import {
  MY_NEW_CHANNEL,
  MyNewChannelRequest,
  MyNewChannelResponse,
} from '../../common/ipc/api';
import { logger } from '../utils/logging';

const myNewChannel = new MainIpcChannel<
  MyNewChannelRequest,
  MyNewChannelResponse
>(MY_NEW_CHANNEL);

export default (): void => {
  myNewChannel.onReceive(async (request: MyNewChannelRequest) => {
    logger.info('MyNewChannel received', { channel: MY_NEW_CHANNEL });
    
    try {
      // Process request
      const result = await processRequest(request);
      
      return {
        result,
        success: true,
      };
    } catch (error) {
      logger.error('MyNewChannel error', { error });
      throw error;
    }
  });
};

async function processRequest(request: MyNewChannelRequest): Promise<string> {
  // Implementation
  return `Processed: ${request.param1}`;
}
```

### Step 3: Register Handler

```typescript
// source/main/index.ts

import myNewChannelHandler from './ipc/my-new-channel';

// In initialization
myNewChannelHandler();
```

### Step 4: Create Renderer Client

```typescript
// source/renderer/app/ipc/my-new-channel.ts

import { RendererIpcChannel } from './lib/RendererIpcChannel';
import {
  MY_NEW_CHANNEL,
  MyNewChannelRequest,
  MyNewChannelResponse,
} from '../../../common/ipc/api';

export const myNewChannel = new RendererIpcChannel<
  MyNewChannelRequest,
  MyNewChannelResponse
>(MY_NEW_CHANNEL);
```

### Step 5: Use in Store/Component

```typescript
// source/renderer/app/stores/MyStore.ts

import { myNewChannel } from '../ipc/my-new-channel';

class MyStore extends Store {
  @action async doSomething(param1: string): Promise<void> {
    const response = await myNewChannel.send({
      param1,
      param2: 42,
    });
    
    if (response.success) {
      console.log('Result:', response.result);
    }
  }
}
```

---

## IPC Channel Types

### Request/Response Channel

Single request, single response. Most common pattern.

```typescript
// Definition
const channel = new MainIpcChannel<Request, Response>(CHANNEL_NAME);

// Handler
channel.onReceive(async (request) => {
  return response;
});

// Client
const response = await channel.send(request);
```

### Conversation Channel

Bidirectional communication for ongoing interactions.

```typescript
// Main process
const conversation = new MainIpcConversation<
  MainRequest,
  MainResponse,
  RendererRequest,
  RendererResponse
>(CHANNEL_NAME);

// Listen for renderer messages
conversation.onRequest(async (request) => {
  return response;
});

// Send to renderer
conversation.send(mainRequest);
```

### Main → Trusted Renderer Notification

```typescript
consumeIpcResponse(
  channel.send(data, currentWindowSender.sender),
  CHANNEL_NAME
);
```

The hostile dApp renderer has one exception to the trusted-renderer wrapper:
`dapp-cip30-gateway`. Its sole raw `ipcMain.handle` owner is
`source/main/cip30/Cip30Broker.ts`, and the IPC audit permits only that exact
channel after broker authentication of the active guest top frame, canonical
origin, document generation, and route lease. Never add another raw guest
channel or route guest calls through `MainIpcChannel`, whose authority is the
trusted main renderer.

Broker-selected wallet work uses `DAPP_CIP30_WALLET_CHANNEL` in the opposite
direction through `MainIpcChannel`: main sends an immutable request to the
authenticated trusted renderer and independently validates its response.

---

## Channel Categories

### Cardano Channels

| Channel                             | Purpose                               |
|-------------------------------------|---------------------------------------|
| `CARDANO_STATE_CHANNEL`             | Node state updates (main → renderer)  |
| `CARDANO_TLS_CONFIG_CHANNEL`        | Get TLS config for API                |
| `CARDANO_RESTART_CHANNEL`           | Request node restart                  |
| `CARDANO_AWAIT_UPDATE_CHANNEL`      | Wait for update                       |
| `GET_CACHED_CARDANO_STATUS_CHANNEL` | Get cached status                     |
| `SET_CACHED_CARDANO_STATUS_CHANNEL` | Update cached status                  |
| `GET_BLOCK_SYNC_PROGRESS_CHANNEL`   | Get sync progress                     |

### Hardware Wallet Channels

| Channel                                  | Purpose                    |
|------------------------------------------|----------------------------|
| `GET_HARDWARE_WALLET_TRANSPORT_CHANNEL`  | Connect to device          |
| `GET_HARDWARE_WALLET_CONNECTION_CHANNEL` | Device events              |
| `GET_EXTENDED_PUBLIC_KEY_CHANNEL`        | Get extended public key    |
| `GET_CARDANO_ADA_APP_CHANNEL`            | Check Cardano app          |
| `SIGN_TRANSACTION_LEDGER_CHANNEL`        | Sign with Ledger           |
| `SIGN_TRANSACTION_TREZOR_CHANNEL`        | Sign with Trezor           |
| `DERIVE_ADDRESS_CHANNEL`                 | Derive address             |
| `SHOW_ADDRESS_CHANNEL`                   | Verify address on device   |

### File Operation Channels

| Channel                          | Purpose                     |
|----------------------------------|-----------------------------|
| `SHOW_OPEN_DIALOG_CHANNEL`       | Open file dialog            |
| `SHOW_SAVE_DIALOG_CHANNEL`       | Save file dialog            |
| `GENERATE_PAPER_WALLET_CHANNEL`  | Generate paper wallet PDF   |
| `GENERATE_ADDRESS_PDF_CHANNEL`   | Generate address PDF        |
| `GENERATE_VOTING_PDF_CHANNEL`    | Generate voting PDF         |
| `GENERATE_CSV_CHANNEL`           | Export transactions CSV     |

### Logging Channels

| Channel                          | Purpose               |
|----------------------------------|-----------------------|
| `GET_LOGS_CHANNEL`               | Get log files         |
| `COMPRESS_LOGS_CHANNEL`          | Compress logs         |
| `DOWNLOAD_LOGS_CHANNEL`          | Save compressed logs  |
| `SET_STATE_SNAPSHOT_LOG_CHANNEL` | Log state snapshot    |

### System Channels

| Channel                            | Purpose               |
|------------------------------------|-----------------------|
| `GET_DISK_SPACE_STATUS_CHANNEL`    | Check disk space      |
| `GET_STATE_DIRECTORY_PATH_CHANNEL` | Get state directory   |
| `GET_SYSTEM_LOCALE_CHANNEL`        | Get system locale     |
| `OPEN_EXTERNAL_URL_CHANNEL`        | Open URL in browser   |
| `OPEN_LOCAL_DIRECTORY_CHANNEL`     | Open in file manager  |
| `INTROSPECT_ADDRESS_CHANNEL`       | Analyze address       |

---

## Best Practices

### Authenticate The Trusted Renderer

Main-side wrapper handlers authenticate the active trusted main `WebContents`,
main frame, canonical document, and origin before invoking application code.
Keep the Electron event available as the optional second handler argument when a
handler needs already-authenticated context. Do not respond with a global window
lookup or `event.sender.send`; the wrappers use `event.reply` so responses return
to the originating frame.

Wrapper requests are correlated and register their response listener before
sending. Every production channel must remain in
`source/main/ipc/privilegedIpcManifest.ts`. Do not bypass these guarantees with a
raw `ipcMain`, scoped/frame/service-worker IPC, WebContents IPC event,
`MessagePortMain`, direct renderer `send`/`invoke`/`postMessage`, shared
uncorrelated response listener, or aliased equivalent. The symbol-aware audit
uses empty production bypass allowlists.

Every main-originated wrapper promise must be explicitly awaited or consumed.
Long-lived producers resolve `currentWindowSender.sender` at send time, and
expected navigation/destruction/replacement cancellation is settled without
sensitive error logging. Never select a target with
`BrowserWindow.getAllWindows()` or redirect a privileged response globally.

### Error Handling

```typescript
// Main handler
channel.onReceive(async (request) => {
  try {
    const result = await riskyOperation(request);
    return { success: true, data: result };
  } catch (error) {
    logger.error('Channel error', { error, channel: CHANNEL_NAME });
    // Errors are serialized and sent to renderer
    throw error;
  }
});

// Renderer usage
try {
  const response = await channel.send(request);
} catch (error) {
  // Handle error from main process
  console.error('IPC error:', error);
}
```

### Logging

Log only non-sensitive channel lifecycle metadata when debugging. Never log
wallet data, transaction/message bytes, signatures, credentials, origins, full
URLs, or arbitrary request/response payloads:

```typescript
import { logger } from '../utils/logging';

channel.onReceive(async (request) => {
  logger.info('Channel received', { channel: CHANNEL_NAME });
  
  const response = await processRequest(request);
  
  logger.info('Channel responding', { channel: CHANNEL_NAME });
  return response;
});
```

### Type Safety

Always define types in `api.ts`:

```typescript
// Good - types in common/ipc/api.ts
export type MyRequest = { ... };
export type MyResponse = { ... };

// Avoid - inline types
const channel = new MainIpcChannel<{ foo: string }, { bar: number }>(...);
```

### Avoid Heavy Payloads

Keep IPC payloads small. For large data:
- Use file paths instead of file contents
- Stream data if possible
- Compress before sending

---

## Debugging IPC

### Enable IPC Lifecycle Logging

```typescript
// Instrument named registrations without logging events or payload arguments.
logger.debug('IPC handler registered', { channel: CHANNEL_NAME });
```

### Common Issues

#### Channel Not Responding

**Problem:** Renderer call hangs
**Solutions:**
1. Check handler is registered in main index
2. Verify channel names match exactly
3. Look for errors in main process logs

#### Type Mismatch

**Problem:** Runtime errors from wrong types
**Solutions:**
1. Verify types in `api.ts` match usage
2. Check for serialization issues (functions, symbols)
3. Use runtime validation for critical channels

#### Handler Called Multiple Times

**Problem:** Handler registered multiple times
**Solutions:**
1. Ensure handler registration is idempotent
2. Check for hot-reload re-registrations
3. Use `onReceive` only once per channel

---

## Testing IPC

### Mock IPC in Renderer Tests

```typescript
import { ipcRenderer } from 'electron-mock-ipc';

// Mock response
jest.mock('electron', () => ({
  ipcRenderer: {
    invoke: jest.fn().mockResolvedValue({ success: true }),
  },
}));
```

### Test Main Handlers

```typescript
import myChannelHandler from '../ipc/my-channel';

describe('MyChannel', () => {
  beforeAll(() => {
    myChannelHandler();
  });

  it('processes request correctly', async () => {
    // Simulate IPC call to handler
    const response = await handleRequest({ param: 'test' });
    expect(response.success).toBe(true);
  });
});
```
