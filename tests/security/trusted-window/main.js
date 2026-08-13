require('@swc-node/register');

const http = require('http');
const { app, BrowserWindow, ipcMain, shell } = require('electron');
const {
  OPEN_EXTERNAL_URL_CHANNEL,
  OPEN_LOCAL_DIRECTORY_CHANNEL,
} = require('../../../source/common/ipc/api');
const {
  registerShellIpc,
} = require('../../../source/main/ipc/registerShellIpc');
const {
  installFailedLoadRecovery,
  installGlobalPopupPolicy,
  installTrustedWindowNavigationPolicy,
} = require('../../../source/main/windows/navigationPolicy');

const results = {};
let webContentsCreated = 0;
let externalOpenCalls = 0;
const timeout = setTimeout(() => {
  process.stderr.write('trusted-window fixture timed out\n');
  app.exit(1);
}, 10000);

shell.openExternal = async () => {
  externalOpenCalls += 1;
};
installGlobalPopupPolicy(app);
app.on('web-contents-created', () => {
  webContentsCreated += 1;
});

const listen = (server) =>
  new Promise((resolve) => server.listen(0, '127.0.0.1', resolve));
const close = (server) => new Promise((resolve) => server.close(resolve));
const delay = () => new Promise((resolve) => setTimeout(resolve, 100));

app.whenReady().then(async () => {
  let adversaryHits = 0;
  const adversaryServer = http.createServer((_request, response) => {
    adversaryHits += 1;
    response.writeHead(200, { 'Content-Type': 'text/html' });
    response.end('<!doctype html><title>adversary</title>');
  });
  await listen(adversaryServer);
  const adversaryUrl = `http://127.0.0.1:${adversaryServer.address().port}/`;

  const trustedServer = http.createServer((request, response) => {
    if (request.url === '/redirect') {
      response.writeHead(302, { Location: adversaryUrl });
      response.end();
      return;
    }
    response.writeHead(200, { 'Content-Type': 'text/html' });
    response.end('<!doctype html><title>trusted</title><body>trusted</body>');
  });
  await listen(trustedServer);

  const trustedUrl = new URL(
    `http://127.0.0.1:${trustedServer.address().port}/`
  );
  const window = new BrowserWindow({ show: false });
  installTrustedWindowNavigationPolicy(window.webContents, trustedUrl);
  let recoveries = 0;
  installFailedLoadRecovery(window.webContents, () => {
    recoveries += 1;
  });

  await window.loadURL(trustedUrl.href);
  results.initialLoad = window.webContents.getURL() === trustedUrl.href;

  await window.webContents.executeJavaScript(
    `location.href = '${adversaryUrl}'`
  );
  await delay();
  results.remoteNavigationDenied =
    adversaryHits === 0 && window.webContents.getURL() === trustedUrl.href;

  const redirectUrl = new URL(`${trustedUrl.href}redirect`);
  const redirectWindow = new BrowserWindow({ show: false });
  installTrustedWindowNavigationPolicy(redirectWindow.webContents, redirectUrl);
  await redirectWindow.loadURL(redirectUrl.href).catch(() => undefined);
  results.remoteRedirectDenied =
    adversaryHits === 0 && redirectWindow.webContents.getURL() !== adversaryUrl;

  await window.webContents.executeJavaScript(
    "history.pushState({}, '', '#history'); location.hash = '#wallets'"
  );
  results.hashAndHistoryRouting =
    window.webContents.getURL() === `${trustedUrl.href}#wallets`;

  results.popupDenied = await window.webContents.executeJavaScript(
    `window.open('${adversaryUrl}') === null`
  );
  results.popupHasNoShellSideEffect = externalOpenCalls === 0;

  await window.webContents.executeJavaScript(
    `const frame = document.createElement('iframe'); frame.src = '${adversaryUrl}frame'; document.body.appendChild(frame);`
  );
  await delay();
  results.subframeDenied = adversaryHits === 0;
  results.noDeniedLoadRecovery = recoveries === 0;

  const externalChannel = `${OPEN_EXTERNAL_URL_CHANNEL}-broadcast`;
  const directoryChannel = `${OPEN_LOCAL_DIRECTORY_CHANNEL}-broadcast`;
  results.importHasNoShellListeners =
    ipcMain.listenerCount(externalChannel) === 0 &&
    ipcMain.listenerCount(directoryChannel) === 0;
  registerShellIpc();
  registerShellIpc();
  const listenersAfterRegistration = [
    ipcMain.listenerCount(externalChannel),
    ipcMain.listenerCount(directoryChannel),
  ];

  const replacementWindow = new BrowserWindow({ show: false });
  installTrustedWindowNavigationPolicy(
    replacementWindow.webContents,
    trustedUrl
  );
  registerShellIpc();
  results.singlePolicyRegistrationAcrossRecreation = [
    'will-navigate',
    'will-redirect',
    'will-frame-navigate',
  ].every(
    (eventName) =>
      window.webContents.listenerCount(eventName) === 1 &&
      replacementWindow.webContents.listenerCount(eventName) === 1
  );
  results.singleShellRegistrationAcrossRecreation =
    listenersAfterRegistration.every((count) => count === 1) &&
    ipcMain.listenerCount(externalChannel) === 1 &&
    ipcMain.listenerCount(directoryChannel) === 1;
  results.noUnexpectedWebContents = webContentsCreated === 3;

  const passed = Object.values(results).every(Boolean);
  process.stdout.write(`${JSON.stringify(results)}\n`);
  clearTimeout(timeout);
  window.destroy();
  redirectWindow.destroy();
  replacementWindow.destroy();
  await close(trustedServer);
  await close(adversaryServer);
  app.exit(passed ? 0 : 1);
});
