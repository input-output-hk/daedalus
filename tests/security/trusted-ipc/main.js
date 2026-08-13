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
  bindTrustedRenderer,
} = require('../../../source/main/ipc/lib/trustedRendererIpcAuthority');

const deadline = setTimeout(() => {
  process.stderr.write('trusted-ipc fixture timed out\n');
  app.exit(1);
}, 15000);

let invocationCount = 0;
let channel;

const fail = (error) => {
  clearTimeout(deadline);
  process.stderr.write(`${error.stack || error}\n`);
  app.exit(1);
};

const runWindow = async (trustedUrl) => {
  const window = new BrowserWindow({
    show: false,
    webPreferences: {
      contextIsolation: false,
      nodeIntegration: false,
      preload: path.join(__dirname, 'preload.js'),
    },
  });
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
  window.webContents.send('trusted-ipc-start');
  const actualOrigin = await originPromise;
  const expectedOrigin =
    trustedUrl.protocol === 'file:' ? 'file://' : trustedUrl.origin;
  assert.strictEqual(actualOrigin, expectedOrigin);
  const results = await resultsPromise;
  assert.deepStrictEqual(results, ['first-result', 'second-result']);
  return { inactive, window };
};

app.whenReady().then(async () => {
  try {
    const server = http.createServer((_request, response) => {
      response.writeHead(200, { 'Content-Type': 'text/html' });
      response.end('<!doctype html><body>trusted ipc fixture</body>');
    });
    await new Promise((resolve) => server.listen(0, '127.0.0.1', resolve));
    const trustedUrl = new URL(`http://127.0.0.1:${server.address().port}/`);

    channel = new MainIpcChannel('trusted-ipc-probe');
    channel.onRequest(async (value, event) => {
      assert.strictEqual(event.senderFrame, event.sender.mainFrame);
      assert.ok(
        event.senderFrame.origin === trustedUrl.origin ||
          event.senderFrame.origin === 'file://'
      );
      invocationCount += 1;
      if (value === 'first')
        await new Promise((resolve) => setTimeout(resolve, 20));
      return `${value}-result`;
    });

    const first = await runWindow(trustedUrl);
    const second = await runWindow(trustedUrl);
    const packaged = await runWindow(
      pathToFileURL(path.join(__dirname, 'index.html'))
    );
    assert.strictEqual(first.inactive, true);
    assert.strictEqual(second.inactive, true);
    assert.strictEqual(packaged.inactive, true);
    assert.strictEqual(invocationCount, 6);
    first.window.destroy();
    second.window.destroy();
    packaged.window.destroy();
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
      })}\n`
    );
    app.exit(0);
  } catch (error) {
    fail(error);
  }
}, fail);
