require('@swc-node/register');

const assert = require('assert');
const http = require('http');
const path = require('path');
const { pathToFileURL } = require('url');
const { app, BrowserWindow, ipcMain } = require('electron');
const {
  MainIpcChannel,
} = require('../../../source/main/ipc/lib/MainIpcChannel');
const {
  MainIpcConversation,
} = require('../../../source/main/ipc/lib/MainIpcConversation');
const {
  bindTrustedRenderer,
} = require('../../../source/main/ipc/lib/trustedRendererIpcAuthority');
const {
  privilegedIpcManifest,
} = require('../../../source/main/ipc/privilegedIpcManifest');

const deadline = setTimeout(() => {
  process.stderr.write('trusted-ipc fixture timed out\n');
  app.exit(1);
}, 15000);

const incomingEntries = privilegedIpcManifest.filter(
  ({ receive }) => receive !== 'none'
);
const channels = new Map();
const unhandledRejections = [];
let invocationCount = 0;
process.on('unhandledRejection', (error) => unhandledRejections.push(error));

const fail = (error) => {
  clearTimeout(deadline);
  process.stderr.write(`${error.stack || error}\n`);
  app.exit(1);
};

const createWindow = () =>
  new BrowserWindow({
    show: false,
    webPreferences: {
      contextIsolation: false,
      nodeIntegration: false,
      nodeIntegrationInSubFrames: true,
      preload: path.join(__dirname, 'preload.js'),
    },
  });

const runWindow = async (trustedUrl) => {
  const window = createWindow();
  bindTrustedRenderer(window, trustedUrl);
  const inactive = !window.webContents.mainFrame.url.startsWith(
    trustedUrl.origin
  );
  const resultsPromise = new Promise((resolve) => {
    window.webContents.ipc.once('trusted-ipc-results', (event, results) => {
      assert.strictEqual(event.sender, window.webContents);
      resolve(results);
    });
  });
  const originPromise = new Promise((resolve) => {
    ipcMain.once('trusted-ipc-origin-probe', (event) =>
      resolve(event.senderFrame.origin)
    );
  });
  await window.loadURL(trustedUrl.href);
  window.webContents.send('trusted-ipc-start', incomingEntries);
  const actualOrigin = await originPromise;
  const expectedOrigin =
    trustedUrl.protocol === 'file:' ? 'file://' : trustedUrl.origin;
  assert.strictEqual(actualOrigin, expectedOrigin);
  const results = await resultsPromise;
  assert.deepStrictEqual(
    results,
    incomingEntries.map(({ channel }) => `${channel}-result`)
  );
  return { inactive, window };
};

const sendHostileTraffic = async (
  window,
  frame = window.webContents.mainFrame
) => {
  const sent = new Promise((resolve) => {
    window.webContents.ipc.once('hostile-ipc-sent', (event) => {
      assert.strictEqual(event.senderFrame, frame);
      resolve();
    });
  });
  frame.send('hostile-ipc-start', incomingEntries);
  await sent;
  await new Promise((resolve) => setTimeout(resolve, 30));
};

const setupOutgoingTarget = async (window, entries = privilegedIpcManifest) => {
  const ready = new Promise((resolve) => {
    window.webContents.ipc.once('outgoing-ipc-ready', resolve);
  });
  window.webContents.send('setup-outgoing-ipc', entries);
  await ready;
};

const outgoingEndpoint = ({ channel, transport }) =>
  transport === 'conversation' ? channel : `${channel}-response`;

const listenerCount = (entries) =>
  entries.reduce(
    (count, entry) => count + ipcMain.listenerCount(outgoingEndpoint(entry)),
    0
  );

const startOutgoing = (window, entries) =>
  entries.map((entry) =>
    channels.get(entry.channel).send(entry.channel, window)
  );

const proveSpoofAndCleanup = async (trustedUrl) => {
  const trusted = createWindow();
  bindTrustedRenderer(trusted, trustedUrl);
  await trusted.loadURL(trustedUrl.href);
  await setupOutgoingTarget(trusted);
  const hostile = createWindow();
  await hostile.loadURL(trustedUrl.href);
  const baselineListeners = listenerCount(privilegedIpcManifest);
  const pendingReported = new Promise((resolve) => {
    trusted.webContents.ipc.once('outgoing-ipc-pending', (_event, pending) =>
      resolve(pending)
    );
  });
  let settled = 0;
  const promises = startOutgoing(trusted, privilegedIpcManifest).map(
    (promise) =>
      promise.then((value) => {
        settled += 1;
        return value;
      })
  );
  const pending = await pendingReported;
  assert.strictEqual(pending.length, privilegedIpcManifest.length);
  const spoofed = new Promise((resolve) => {
    hostile.webContents.ipc.once('hostile-spoof-sent', resolve);
  });
  hostile.webContents.send('hostile-spoof-responses', pending);
  await spoofed;
  await new Promise((resolve) => setTimeout(resolve, 30));
  assert.strictEqual(settled, 0);
  trusted.webContents.send('release-outgoing-ipc');
  await Promise.all(promises);
  assert.strictEqual(settled, privilegedIpcManifest.length);
  assert.strictEqual(listenerCount(privilegedIpcManifest), baselineListeners);
  trusted.destroy();
  hostile.destroy();
};

const proveLifecycleCancellation = async (trustedUrl) => {
  const entry = privilegedIpcManifest[0];
  const baselineListeners = listenerCount([entry]);
  const first = createWindow();
  bindTrustedRenderer(first, trustedUrl);
  await first.loadURL(trustedUrl.href);
  await setupOutgoingTarget(first, [entry]);
  const pending = startOutgoing(first, [entry])[0];
  first.destroy();
  await assert.rejects(pending, /IPC request cancelled/);
  assert.strictEqual(listenerCount([entry]), baselineListeners);

  const replaced = createWindow();
  bindTrustedRenderer(replaced, trustedUrl);
  await replaced.loadURL(trustedUrl.href);
  await setupOutgoingTarget(replaced, [entry]);
  const replacedPending = startOutgoing(replaced, [entry])[0];
  const replacement = createWindow();
  bindTrustedRenderer(replacement, trustedUrl);
  await assert.rejects(replacedPending, /IPC request cancelled/);
  assert.strictEqual(listenerCount([entry]), baselineListeners);
  replaced.destroy();
  replacement.destroy();
};

app.whenReady().then(async () => {
  try {
    const server = http.createServer((_request, response) => {
      response.writeHead(200, { 'Content-Type': 'text/html' });
      response.end(
        '<!doctype html><body>trusted ipc fixture<iframe src="/frame"></iframe></body>'
      );
    });
    await new Promise((resolve) => server.listen(0, '127.0.0.1', resolve));
    const trustedUrl = new URL(`http://127.0.0.1:${server.address().port}/`);

    for (const entry of privilegedIpcManifest) {
      const channel =
        entry.transport === 'conversation'
          ? new MainIpcConversation(entry.channel)
          : new MainIpcChannel(entry.channel);
      channels.set(entry.channel, channel);
      if (entry.receive === 'none') continue;
      const register =
        entry.receive === 'broadcast'
          ? channel.onReceive.bind(channel)
          : channel.onRequest.bind(channel);
      register(async (_value, event) => {
        assert.strictEqual(event.senderFrame, event.sender.mainFrame);
        invocationCount += 1;
        return `${entry.channel}-result`;
      });
    }

    const first = await runWindow(trustedUrl);
    const second = await runWindow(trustedUrl);
    const packaged = await runWindow(
      pathToFileURL(path.join(__dirname, 'index.html'))
    );
    assert.strictEqual(first.inactive, true);
    assert.strictEqual(second.inactive, true);
    assert.strictEqual(packaged.inactive, true);
    assert.strictEqual(invocationCount, incomingEntries.length * 3);

    const beforeHostile = invocationCount;
    const untrusted = createWindow();
    await untrusted.loadURL(trustedUrl.href);
    await sendHostileTraffic(untrusted);

    const wrongDocument = createWindow();
    bindTrustedRenderer(wrongDocument, trustedUrl);
    await wrongDocument.loadURL(`http://localhost:${trustedUrl.port}/wrong`);
    await sendHostileTraffic(wrongDocument);

    const subframe = createWindow();
    bindTrustedRenderer(subframe, trustedUrl);
    await subframe.loadURL(trustedUrl.href);
    const hostileFrame = subframe.webContents.mainFrame.frames[0];
    assert.ok(hostileFrame);
    await sendHostileTraffic(subframe, hostileFrame);

    const stale = createWindow();
    bindTrustedRenderer(stale, trustedUrl);
    await stale.loadURL(trustedUrl.href);
    await stale.loadURL(new URL('/stale', trustedUrl).href);
    await sendHostileTraffic(stale);
    assert.strictEqual(invocationCount, beforeHostile);

    await proveSpoofAndCleanup(trustedUrl);
    await proveLifecycleCancellation(trustedUrl);
    assert.deepStrictEqual(unhandledRejections, []);

    first.window.destroy();
    second.window.destroy();
    packaged.window.destroy();
    untrusted.destroy();
    wrongDocument.destroy();
    subframe.destroy();
    stale.destroy();
    await new Promise((resolve) => server.close(resolve));
    clearTimeout(deadline);
    process.stdout.write(
      `${JSON.stringify({
        actualWrappers: true,
        senderFrame: true,
        httpOrigin: true,
        packagedFileOrigin: true,
        inactiveBeforeLoad: true,
        concurrency: true,
        recreation: true,
        manifestChannels: privilegedIpcManifest.length,
        actualTransportEndpoints: true,
        untrustedWindowRejected: true,
        wrongOriginRejected: true,
        subframeRejected: true,
        staleLifecycleRejected: true,
        pendingSpoofRejected: true,
        responseCleanup: true,
        destructionCancellation: true,
        replacementCancellation: true,
        zeroUnhandledRejections: true,
        zeroHostileEffects: true,
      })}\n`
    );
    app.exit(0);
  } catch (error) {
    fail(error);
  }
}, fail);
