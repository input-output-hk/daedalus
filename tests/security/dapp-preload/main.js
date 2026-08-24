const assert = require('assert');
const path = require('path');
const { app, BrowserWindow, ipcMain } = require('electron');

const CHANNEL = 'dapp-cip30-gateway';
const deadline = setTimeout(() => {
  process.stderr.write('dApp preload fixture timed out\n');
  app.exit(1);
}, 15000);

const fail = (error) => {
  clearTimeout(deadline);
  process.stderr.write(`${error.stack || error}\n`);
  app.exit(1);
};

app.whenReady().then(async () => {
  ipcMain.handle(CHANNEL, (_event, request) => {
    assert.deepStrictEqual(request, {
      method: 'provider.isEnabled',
      args: [],
    });
    return {
      status: 'rejected',
      rejection: {
        type: 'api-error',
        value: { code: -3, info: 'fixture refusal' },
      },
    };
  });

  const guest = new BrowserWindow({
    show: false,
    webPreferences: {
      preload: path.resolve(__dirname, '../../../dist/main/dapp.js'),
      sandbox: true,
      contextIsolation: true,
      nodeIntegration: false,
      devTools: false,
      partition: `dapp-preload-${process.pid}`,
    },
  });

  await guest.loadFile(path.join(__dirname, 'index.html'));
  const result = await guest.webContents.executeJavaScript(
    'window.dappPreloadFixtureResult'
  );

  assert.strictEqual(result.providerReadyBeforePageScript, true);
  assert.deepStrictEqual(result.cardanoKeys, ['daedalus']);
  assert.deepStrictEqual(result.rejection, {
    code: -3,
    info: 'fixture refusal',
  });
  assert.strictEqual(result.rejectionWasError, false);
  assert.deepStrictEqual(result.leakedGlobals, []);

  process.stdout.write(`${JSON.stringify(result)}\n`);
  clearTimeout(deadline);
  guest.destroy();
  ipcMain.removeHandler(CHANNEL);
  app.exit(0);
}, fail);
