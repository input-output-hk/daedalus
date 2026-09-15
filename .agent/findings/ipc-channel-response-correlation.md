# Finding: `IpcChannel` resolves on whoever answers next

**Status:** open, not scheduled
**Raised from:** asset metadata cache plan review
**Scope:** every channel built on `IpcChannel`, which is most of them
**Severity:** low today, because the channels in use are shaped in a way that
hides it. No funds at risk and nothing attacker-controlled: both ends are the
same application and nothing outside it influences message ordering. It is a
correctness defect that becomes reachable the moment a channel carries
concurrent requests.

---

## What it does

`IpcChannel.send` (`source/common/ipc/lib/IpcChannel.ts:101-120`) and
`IpcChannel.request` (`:126-145`) both register a one-shot listener and resolve
on the next message that arrives:

```ts
receiver.once(
  this._responseChannel,
  (event, isOk: boolean, response: Incoming) => {
    if (isOk) {
      resolve(response);
    } else {
      reject(response);
    }
  }
);
```

Nothing ties that response to the request that caused it. The doc comment above
`send` states the behavior without flagging it as a limitation: "waits for the
next response on the same channel".

A channel derives three names from one base in the constructor (`:91-93`):
`-broadcast`, `-request` and `-response`. All callers of one channel share the
single `-response` name, so two in-flight requests on the same channel are two
listeners on one stream, each resolving on whichever message lands first. Each
can receive the other's payload.

Neither subclass adds correlation. `source/main/ipc/lib/MainIpcChannel.ts` and
`source/renderer/app/ipc/lib/RendererIpcChannel.ts` only supply default sender
and receiver objects and delegate to `super`.

## Why nothing is broken today

The channels that exist are almost all single-shot and user-initiated: a
request is issued in response to an action, and the next one cannot start until
the first resolves. Under that shape there is never a second listener on the
response stream, so the defect is unreachable. `source/common/ipc/api.ts`
exports 63 constants, 62 of which construct a channel, and the ones that matter
here follow that pattern.

It stops being unreachable the first time a channel serves a bulk read keyed on
a list, or any caller that can be invoked twice before the first call returns.

## The repository already contains the fix

`IpcConversation` solves exactly this problem, in the same directory, and
predates the finding. `source/common/ipc/lib/IpcConversation.ts:65-96` mints a
`conversationId` per request, sends it alongside the message, and drops anything
that does not match before resolving:

```ts
const conversationId = uuidv4();
const handler = (event, messageId, isOk, response) => {
  // Only handle messages with matching conversation id!
  if (messageId !== conversationId) return;
  ...
  receiver.removeListener(this._channelName, handler);
};
receiver.on(this._channelName, handler);
sender.send(this._channelName, conversationId, message);
```

It also uses `on` plus an explicit `removeListener` rather than `once`, which is
what lets it discard a non-matching message and keep waiting. `once` cannot do
that: it unregisters on the first message whether or not that message was
wanted.

So the two primitives in `source/common/ipc/lib/` disagree about whether
responses need correlating. Today
`IpcConversation` is used only by the electron-store conversation
(`source/main/ipc/electronStoreConversation.ts`,
`source/renderer/app/ipc/electronStoreConversation.ts`).

## What a fix would have to decide

**Whether to fix the primitive or the callers.** Correcting `IpcChannel` itself
means changing the wire shape for every channel built on it: the responder has
to echo an id it currently never sees. Main and renderer have to change
together, and any channel whose responder is not updated stops resolving
entirely. That is a wide, coordinated change to code that works.

**Whether new channels should use `IpcChannel` at all.** If `IpcConversation` is
already the correct primitive, the cheaper answer is to route new concurrent
channels through it and leave the single-shot ones where they are. That leaves
two primitives in the tree with an undocumented rule about which to pick.

**What `once` should become.** Any correlating version needs `on` with an
explicit `removeListener`, plus a decision about what happens to a response that
matches no outstanding request, and whether a request that never receives its
correlated response times out or waits forever.

Whichever route, the comment on `IpcChannel.send` should stop describing
resolve-on-next-message as if it were the intended contract.

## Relevant files

- `source/common/ipc/lib/IpcChannel.ts` (`send` at `:101-120`, `request` at
  `:126-145`, name derivation at `:91-93`)
- `source/common/ipc/lib/IpcConversation.ts` (`request` at `:65-96`)
- `source/main/ipc/lib/MainIpcChannel.ts`
- `source/renderer/app/ipc/lib/RendererIpcChannel.ts`
- `source/common/ipc/api.ts` (63 exported constants, 62 of them channels)
