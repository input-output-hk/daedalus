---
description: IPC communication patterns between main and renderer
---

# IPC Workflow

This workflow guides development of IPC (Inter-Process Communication) between Electron's main and renderer processes in Daedalus.

## Overview

Daedalus uses type-safe IPC channels for communication between:
- **Main Process** (Node.js) - Handles Cardano, hardware wallets, system access
- **Renderer Process** (React) - User interface

All channel names are defined in `source/common/ipc/api.ts`.

There is no `ipcRenderer.invoke` and no `ipcMain.handle` anywhere in `source/`.
Both processes talk over `ipcRenderer.send` and `ipcMain.on`, wrapped by
`IpcChannel`. Two bare names bypass the wrapper and nothing else does.

---

## IPC Architecture

A channel is one shared name constant and one `IpcChannel` subclass instance per
side. The constructor derives three wire names from that constant
(`source/common/ipc/lib/IpcChannel.ts:91-93`):

```ts
this._broadcastChannel = `${channelName}-broadcast`;
this._requestChannel = `${channelName}-request`;
this._responseChannel = `${channelName}-response`;
```

Messages travel on those three names and never on the bare constant.

```
┌──────────────────────────────────────────────────────────────────────────┐
│                          Renderer Process                                │
│  source/renderer/app/ipc/                                                │
│  └── RendererIpcChannel / RendererIpcConversation                        │
│      sender and receiver default to global.ipcRenderer                   │
│                                                                          │
│      request(msg)  ──▶ ipcRenderer.send('<NAME>-request',   msg)         │
│      send(msg)     ──▶ ipcRenderer.send('<NAME>-broadcast', msg)         │
│      then          ──▶ ipcRenderer.once('<NAME>-response', cb)           │
└───────────────────────────────┬──────────────────────────────────────────┘
                                │
                                ▼
┌──────────────────────────────────────────────────────────────────────────┐
│                          Main Process                                    │
│  source/main/ipc/                                                        │
│  └── MainIpcChannel / MainIpcConversation                                │
│      receiver defaults to ipcMain                                        │
│                                                                          │
│      onRequest(fn)  ──▶ ipcMain.on('<NAME>-request',   …)                │
│      onReceive(fn)  ──▶ ipcMain.on('<NAME>-broadcast', …)                │
│      reply          ──▶ event.sender.send('<NAME>-response', isOk, body) │
└──────────────────────────────────────────────────────────────────────────┘
```

The preload script is **not** a hop. `source/main/preload.ts:31` assigns
Electron's `ipcRenderer` onto the renderer's `global`, which is what lets
`RendererIpcChannel` default its sender and receiver to it
(`source/renderer/app/ipc/lib/RendererIpcChannel.ts:16-17`). Nothing is
forwarded or re-wrapped there.

### Two verbs, and they must be paired

`send` and `request` differ only in which name they post on, and each has one
matching handler. Pairing them wrongly is a silent hang: the message is sent, no
listener is registered on that name, and the promise never settles.

| Caller | Posts on | Handler | Listens on |
|---|---|---|---|
| `channel.request(msg)` | `<NAME>-request` | `channel.onRequest(fn)` | `<NAME>-request` |
| `channel.send(msg)` | `<NAME>-broadcast` | `channel.onReceive(fn)` | `<NAME>-broadcast` |

Both wait for one message on `<NAME>-response`. The handler's return value is
sent back with `isOk` true; a throw is sent back with `isOk` false and rejects
the caller's promise.

### A channel name is a singleton

`IpcChannel`'s constructor throws `Channel <name> already exists` if a second
instance is built with the same name in the same process
(`source/common/ipc/lib/IpcChannel.ts:87-89`). One instance per side, declared at
module scope.

### Responses are not correlated

`send` and `request` both register a one-shot listener on the single
`<NAME>-response` name and resolve on the next message to arrive, whichever
request produced it. Two requests in flight on one channel are two listeners on
one stream, and each can resolve with the other's payload. Neither subclass adds
correlation.

This is safe for a channel that is single-shot and user-initiated, which most of
them are. It is not safe for a bulk read keyed on a list, or for any caller that
can be invoked again before the first call returns. A channel of that shape has
to carry its own request id and match on it; `source/renderer/app/ipc/assetMetadataChannel.ts`
does that, and `IpcConversation` in the same directory as `IpcChannel` solves it
generically with a `conversationId`. The full write-up is
`.agent/findings/ipc-channel-response-correlation.md`.

---

## Quick Reference

### Create a New IPC Channel

1. Define the name constant and the request and response types in
   `source/common/ipc/api.ts`
2. Create the handler in `source/main/ipc/`
3. Register it in `source/main/ipc/index.ts`
4. Create the client in `source/renderer/app/ipc/`

The pair to copy is `source/main/ipc/governanceAnchorChannel.ts` and
`source/renderer/app/ipc/governanceChannel.ts`.

---

## Step-by-Step: Creating an IPC Channel

### Step 1: Define Channel Contract

```typescript
// source/common/ipc/api.ts

export const MY_NEW_CHANNEL = 'MY_NEW_CHANNEL';

// Renderer to main.
export type MyNewChannelRendererRequest = {
  param1: string;
  param2: number;
};

// Main to renderer.
export type MyNewChannelMainResponse = {
  result: string;
  success: boolean;
};
```

The names say which direction each type travels, because the two sides declare
the same pair in opposite order and a name that says only "request" gives no
help with which order is which.

### Step 2: Create Main Process Handler

```typescript
// source/main/ipc/myNewChannel.ts

import { MainIpcChannel } from './lib/MainIpcChannel';
import { MY_NEW_CHANNEL } from '../../common/ipc/api';
import type {
  MyNewChannelRendererRequest,
  MyNewChannelMainResponse,
} from '../../common/ipc/api';
import { logger } from '../utils/logging';

// <Incoming, Outgoing>: on this side the renderer's request arrives and the
// main response leaves.
const myNewChannel: MainIpcChannel<
  MyNewChannelRendererRequest,
  MyNewChannelMainResponse
> = new MainIpcChannel(MY_NEW_CHANNEL);

export const handleMyNewChannelRequests = (): void => {
  myNewChannel.onRequest(async (request) => {
    try {
      return {
        result: await processRequest(request),
        success: true,
      };
    } catch (error) {
      // A throw is sent back with isOk false and rejects the caller's promise.
      // Log what happened and nothing from the request itself unless it is
      // known to carry nothing private.
      logger.error('MyNewChannel error', { error });
      throw error;
    }
  });
};

async function processRequest(
  request: MyNewChannelRendererRequest
): Promise<string> {
  return `Processed: ${request.param1}`;
}
```

### Step 3: Register Handler

```typescript
// source/main/ipc/index.ts

import { handleMyNewChannelRequests } from './myNewChannel';

export default (window: BrowserWindow) => {
  // ... alongside the other handlers
  handleMyNewChannelRequests();
};
```

`source/main/ipc/index.ts` is where every handler is wired. Its default export
is called from `source/main/windows/main.ts:72`, once the window exists, because
several handlers need it. `source/main/index.ts` registers nothing itself.

A handler that pushes to the renderer takes the window as an argument here,
rather than reaching for `BrowserWindow.getAllWindows()`, which would push to
whichever window happened to be first and would make the module untestable
without Electron.

### Step 4: Create Renderer Client

```typescript
// source/renderer/app/ipc/myNewChannel.ts

import { RendererIpcChannel } from './lib/RendererIpcChannel';
import { MY_NEW_CHANNEL } from '../../../common/ipc/api';
import type {
  MyNewChannelMainResponse,
  MyNewChannelRendererRequest,
} from '../../../common/ipc/api';

// <Incoming, Outgoing> again, and on this side the two are the other way round:
// the main response arrives and the renderer request leaves.
export const myNewChannel: RendererIpcChannel<
  MyNewChannelMainResponse,
  MyNewChannelRendererRequest
> = new RendererIpcChannel(MY_NEW_CHANNEL);
```

### Step 5: Use in Store/Component

```typescript
// source/renderer/app/stores/MyStore.ts

import { myNewChannel } from '../ipc/myNewChannel';

class MyStore extends Store {
  @action async doSomething(param1: string): Promise<void> {
    // `request`, because the handler above registered `onRequest`. `send` would
    // post on the broadcast name, where nothing is listening, and the promise
    // would never settle.
    const response = await myNewChannel.request({ param1, param2: 42 });

    if (response.success) {
      runInAction('MyStore::doSomething', () => {
        this.result = response.result;
      });
    }
  }
}
```

`configure({ enforceActions: 'observed' })` is live, so a mutation after an
`await` throws unless it is wrapped in `runInAction`.

---

## IPC Channel Types

### Request/Response Channel

Single request, single response. The common pattern.

```typescript
// Main
const channel: MainIpcChannel<Request, Response> = new MainIpcChannel(NAME);
channel.onRequest(async (request) => response);

// Renderer
const channel: RendererIpcChannel<Response, Request> = new RendererIpcChannel(NAME);
const response = await channel.request(request);
```

### Push, main to one renderer

There is no all-windows broadcast helper. `send` on a `MainIpcChannel` takes the
`webContents` to send to, so the module holds the window it was given:

```typescript
// Main: push a message the renderer did not ask for.
channel.send(message, window.webContents);

// Renderer: receive it.
channel.onReceive(async (message) => {
  handle(message);
});
```

`send` resolves when the other side answers on the response name, and
`onReceive`'s return value is that answer. A push whose answer nobody reads is
still answered, so the returned promise settles; ignore it with a `catch` rather
than leaving it unhandled, because a window that closed mid-flight rejects it.

`source/main/ipc/assetMetadataChannel.ts` is the worked example.

### Conversation Channel

`IpcConversation` is a separate primitive in the same directory. It uses one
channel name rather than three, mints a `conversationId` per request, sends it
alongside the message, and ignores a response that does not match
(`source/common/ipc/lib/IpcConversation.ts:65-96`). It is the primitive to reach
for when more than one request can be in flight at once.

```typescript
// Main
const conversation = new MainIpcConversation<Incoming, Outgoing>(NAME);
conversation.onRequest(async (request) => response);

// Renderer
const conversation = new RendererIpcConversation<Incoming, Outgoing>(NAME);
const response = await conversation.request(request);
```

`source/main/ipc/electronStoreConversation.ts` and its renderer counterpart are
the only users in the tree.

### Outside the channel mechanism

Two bare names are sent directly, with no channel and no response:
`source/renderer/app/stores/WindowStore.ts:17` sends `close-window` and `:25`
sends `resize-window`. They are the exception, not a pattern to copy.

`resize-window` has two handlers rather than one. `source/main/windows/main.ts:74`
registers it inline, and `source/main/ipc/resize-window.ts:5` registers an
identical one, wired in through `source/main/ipc/index.ts`. Both check the
sender and both call `window.setSize`, so a resize is applied twice. Recorded
here rather than worked around; a channel would not have allowed it, because
`IpcChannel` refuses a second instance of the same name.

---

## Channel Categories

These are a selection, not the whole set. `source/common/ipc/api.ts` is the
list, and it is the only place a channel name is declared.

### Backend And Watchdog Channels

The node and wallet are supervised by the watchdog, and the renderer learns
about them through these. Everything marked push is main to renderer and is
received with `onReceive`.

| Channel                              | Purpose                                    |
|--------------------------------------|--------------------------------------------|
| `GET_CACHED_BACKEND_STATUS_CHANNEL`  | Poll the current watchdog state snapshot   |
| `NODE_STARTUP_STATUS_CHANNEL`        | Node startup phase (push)                  |
| `NODE_BLOCK_SYNC_PROGRESS_CHANNEL`   | Block sync progress (push)                 |
| `WALLET_PORT_CHANNEL`                | The wallet port, once it is known (push)   |
| `WATCHDOG_STOPPED_CHANNEL`           | Clean shutdown (push)                      |
| `MITHRIL_COMMAND_CHANNEL`            | Send a Mithril command to the watchdog     |
| `MITHRIL_PROGRESS_CHANNEL`           | Mithril progress (push)                    |
| `MITHRIL_STATUS_CHANNEL`             | Mithril status (push)                      |
| `VALIDATE_CHAIN_STORAGE_CHANNEL`     | Validate a candidate chain path            |
| `CONFIRM_CHAIN_STORAGE_CHANNEL`      | Apply a chain path and restart the watchdog|

### Asset And Governance Channels

| Channel                         | Purpose                                        |
|---------------------------------|------------------------------------------------|
| `ASSET_METADATA_CHANNEL`        | Read the local asset metadata cache            |
| `ASSET_METADATA_UPDATE_CHANNEL` | Rows the cache resolved afterwards (push)      |
| `ASSET_IMAGE_CHANNEL`           | One asset logo, one subject per request        |
| `GOVERNANCE_DREP_ANCHOR_CHANNEL`| Resolve and verify a DRep anchor               |

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

### Error Handling

```typescript
// Main handler
channel.onRequest(async (request) => {
  try {
    return { success: true, data: await riskyOperation(request) };
  } catch (error) {
    logger.error('Channel error', { error });
    // Sent back with isOk false, which rejects the caller's promise.
    throw error;
  }
});

// Renderer usage
try {
  const response = await channel.request(request);
} catch (error) {
  // The error object as it survived structured cloning.
}
```

Decide deliberately whether a handler may reject at all. A rejection reaches the
caller as a rejected promise, which is right for an operation the user asked for
and is waiting on. It is wrong for a channel whose callers can have more than one
request in flight, because a rejection carries no request id and cannot be
attributed: answering with a failure value keeps the response shape and the
correlation intact. `source/main/ipc/assetMetadataChannel.ts` never rejects, for
that reason.

### Logging

Log what happened, not what was in the message. A request can carry a URL, an
address or an amount, and the log file is collected in a support report.

```typescript
channel.onRequest(async (request) => {
  const response = await processRequest(request);
  logger.info('Channel responding', { status: response.status });
  return response;
});
```

### Type Safety

Define both types in `api.ts`, and name them by direction:

```typescript
export type MyChannelRendererRequest = { ... };
export type MyChannelMainResponse = { ... };
```

Then remember that `IpcChannel<Incoming, Outgoing>` is written from the point of
view of the side declaring it, so the two sides list the same pair in opposite
order. Getting it backwards compiles on one side and fails on the other.

### Avoid Heavy Payloads

Keep payloads small. For large data:
- Use file paths instead of file contents
- Give bytes a channel of their own, requested one at a time, so a large payload
  never delays a small one. `ASSET_IMAGE_CHANNEL` exists for exactly that.
- A `Uint8Array` survives the structured clone intact; a `Buffer` arrives as a
  `Uint8Array` on the other side.

---

## Debugging IPC

### Watch the wire

Electron has no wildcard channel, so there is nothing to subscribe to that sees
everything. Listen for the three derived names of the channel under suspicion:

```typescript
// Main process, temporarily.
['-request', '-broadcast', '-response'].forEach((suffix) => {
  ipcMain.on(`${MY_NEW_CHANNEL}${suffix}`, (_event, ...args) => {
    logger.debug(`IPC ${MY_NEW_CHANNEL}${suffix}`, { args });
  });
});
```

### Common Issues

#### Channel Not Responding

**Problem:** the renderer call never settles.
**Solutions:**
1. Check the verbs are paired: `request` with `onRequest`, `send` with
   `onReceive`. Mixing them sends on a name nothing is listening to, and neither
   side reports anything.
2. Check the handler is registered in `source/main/ipc/index.ts`.
3. Check both sides construct from the same constant in
   `source/common/ipc/api.ts` rather than from a string literal.

#### Channel <name> already exists

**Problem:** the constructor throws at startup.
**Solutions:**
1. One instance per name per process, declared at module scope. A channel built
   inside a function is built again on every call.
2. Under Jest, a module re-imported in a second test file constructs it a second
   time. Wrap `jest.isolateModules` around the require, as
   `source/main/ipc/assetMetadataChannel.realfs.spec.ts` does.

#### A response arrives on the wrong promise

**Problem:** two calls in flight, each resolving with the other's payload.
**Cause:** the response name is shared and nothing correlates. See the
architecture section above and `.agent/findings/ipc-channel-response-correlation.md`.
**Solutions:** carry a request id in the payload and match on it, or use
`IpcConversation`, which does it for you.

#### Handler Called Multiple Times

**Problem:** one request produces two responses, and the second is taken by
another request's one-shot listener.
**Solutions:**
1. Make registration idempotent. `handleAssetMetadataRequests` returns early on
   a second call.
2. Register once, from `source/main/ipc/index.ts`.

---

## Testing IPC

### Renderer clients

The renderer channels default their sender and receiver to `global.ipcRenderer`,
so a fake assigned to that global is the whole harness. Make it behave as
Electron does: fire one-shot listeners in registration order and unregister each
as it fires, whatever message it fires on. A fake that matches responses to
listeners tests the fake.

```typescript
const sent: Array<{ channel: string; message: any }> = [];
const onceListeners: Array<{ channel: string; handler: Function }> = [];

(global as any).ipcRenderer = {
  send: (channel: string, message: any) => sent.push({ channel, message }),
  once: (channel: string, handler: Function) =>
    onceListeners.push({ channel, handler }),
  on: () => {},
  removeListener: () => {},
};
```

`source/renderer/app/ipc/assetMetadataChannel.spec.ts` is the worked example.

### Main handlers

Mock `./lib/MainIpcChannel` so no Electron object is touched, and export the
handler set so the spec can call it directly rather than through a channel.

```typescript
jest.mock('./lib/MainIpcChannel', () => ({
  MainIpcChannel: jest.fn().mockImplementation(() => ({
    onRequest: jest.fn(),
    send: jest.fn().mockResolvedValue(undefined),
  })),
}));
```

`source/main/ipc/assetMetadataChannel.realfs.spec.ts` and
`source/main/ipc/governanceAnchorChannel.spec.ts` are the worked examples.
