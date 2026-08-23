require('@swc-node/register');

const assert = require('assert');
const { app, BrowserWindow } = require('electron');
const {
  getDappSandboxAvailability,
  startDappSandboxAvailabilityCheck,
} = require('../../../source/main/sandbox/dappSandboxAvailability');

const deadline = setTimeout(() => {
  process.stderr.write('sandbox-canary fixture timed out\n');
  app.exit(1);
}, 15000);
const created = [];
app.on('web-contents-created', (_event, webContents) => {
  const record = { webContents, pid: undefined };
  created.push(record);
  webContents.once('did-finish-load', () => {
    record.pid = webContents.getOSProcessId();
  });
});

const fail = (error) => {
  clearTimeout(deadline);
  process.stderr.write(`${error.stack || error}\n`);
  app.exit(1);
};

app.whenReady().then(async () => {
  const trustedWindow = new BrowserWindow({ show: false });
  await trustedWindow.loadURL(
    'data:text/html;charset=utf-8,%3C!doctype%20html%3E%3Ctitle%3Etrusted%3C%2Ftitle%3E'
  );
  const trustedPid = trustedWindow.webContents.getOSProcessId();
  const installedMode = process.env.DAEDALUS_SANDBOX_CANARY_INSTALLED === '1';
  const options = {
    isDevelopment: !installedMode,
    cluster: 'mainnet',
    installRoot: installedMode ? '/opt/daedalus/mainnet' : undefined,
  };
  const firstResult = await startDappSandboxAvailabilityCheck(options);
  const secondResult = await startDappSandboxAvailabilityCheck(options);
  if (process.env.DAEDALUS_SANDBOX_CANARY_EXPECT_BYPASS === '1') {
    assert.deepStrictEqual(firstResult, {
      status: 'unavailable',
      reason: 'sandbox-bypass',
    });
    assert.strictEqual(secondResult, firstResult);
    assert.strictEqual(created.length, 1);
    assert.strictEqual(created[0].webContents, trustedWindow.webContents);
    assert.strictEqual(created[0].webContents.isDestroyed(), false);
    process.stdout.write(
      `${JSON.stringify({
        available: false,
        reason: 'sandbox-bypass',
        trustedRendererAlive: true,
        canaryNotCreated: true,
        cachedWithoutRetry: true,
      })}\n`
    );
    clearTimeout(deadline);
    trustedWindow.destroy();
    app.exit(0);
    return;
  }

  assert.deepStrictEqual(firstResult, { status: 'available' });
  assert.strictEqual(secondResult, firstResult);
  assert.deepStrictEqual(getDappSandboxAvailability(), { status: 'available' });
  assert.strictEqual(created.length, 2);
  assert.strictEqual(created[0].webContents, trustedWindow.webContents);
  assert.strictEqual(created[0].pid, trustedPid);
  assert.strictEqual(created[0].webContents.isDestroyed(), false);
  assert.notStrictEqual(created[1].pid, trustedPid);
  assert.strictEqual(created[1].webContents.isDestroyed(), true);

  process.stdout.write(
    `${JSON.stringify({
      available: true,
      trustedRendererAlive: true,
      sandboxedCanaryDestroyed: true,
      distinctRenderers: true,
      cachedWithoutRetry: true,
    })}\n`
  );
  clearTimeout(deadline);
  trustedWindow.destroy();
  app.exit(0);
}, fail);
