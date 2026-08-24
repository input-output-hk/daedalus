require('@swc-node/register');

const assert = require('assert');
const path = require('path');
const { app, BrowserWindow } = require('electron');
const {
  clearDappSession,
  createDappSession,
  installDappSessionPolicy,
  installDappTransportRestrictions,
  installGuestDenialHandlers,
} = require('../../../source/main/dapp/DappSessionPolicy');

installDappTransportRestrictions(app.commandLine);
app.on('window-all-closed', () => undefined);

const deadline = setTimeout(() => {
  process.stderr.write('dApp session fixture timed out\n');
  app.exit(1);
}, 15000);

const fail = (error) => {
  clearTimeout(deadline);
  process.stderr.write(`${error.stack || error}\n`);
  app.exit(1);
};

app
  .whenReady()
  .then(async () => {
    const guestSession = createDappSession();
    assert.strictEqual(guestSession.isPersistent(), false);
    assert.strictEqual(guestSession.getStoragePath(), null);
    const egressPolicy = await installDappSessionPolicy(
      guestSession,
      new Set(['https://example.com'])
    );

    const guest = new BrowserWindow({
      show: false,
      frame: true,
      fullscreenable: false,
      autoHideMenuBar: true,
      webPreferences: {
        preload: path.resolve(__dirname, '../../../dist/main/dapp.js'),
        session: guestSession,
        nodeIntegration: false,
        nodeIntegrationInWorker: false,
        nodeIntegrationInSubFrames: false,
        contextIsolation: true,
        sandbox: true,
        webSecurity: true,
        allowRunningInsecureContent: false,
        webviewTag: false,
        devTools: false,
        plugins: false,
        spellcheck: false,
        enableWebSQL: false,
        navigateOnDragDrop: false,
        disableDialogs: true,
        autoplayPolicy: 'document-user-activation-required',
        disableBlinkFeatures: 'DirectSockets,WebTransport',
      },
    });
    await guest.loadURL('about:blank');
    installGuestDenialHandlers(guest.webContents);

    const result = await guest.webContents.executeJavaScript(`({
    rtcPeerConnection: typeof RTCPeerConnection,
    rtcDataChannel: typeof RTCDataChannel,
    webTransport: typeof WebTransport,
    tcpSocket: typeof TCPSocket,
    udpSocket: typeof UDPSocket,
    webSocket: typeof WebSocket,
    popupDenied: window.open('https://example.com') === null,
    node: typeof process,
    require: typeof require
  })`);

    assert.deepStrictEqual(result, {
      rtcPeerConnection: 'undefined',
      rtcDataChannel: 'undefined',
      webTransport: 'undefined',
      tcpSocket: 'undefined',
      udpSocket: 'undefined',
      webSocket: 'function',
      popupDenied: true,
      node: 'undefined',
      require: 'undefined',
    });
    assert.strictEqual(
      guest.webContents.getWebRTCIPHandlingPolicy(),
      'disable_non_proxied_udp'
    );
    assert.strictEqual(app.commandLine.hasSwitch('disable-quic'), true);

    guest.destroy();
    await egressPolicy.close();
    await clearDappSession(guestSession);
    assert.deepStrictEqual(await guestSession.cookies.get({}), []);

    await new Promise((resolve) =>
      process.stderr.write(`${JSON.stringify(result)}\n`, resolve)
    );
    clearTimeout(deadline);
    app.exit(0);
  })
  .catch(fail);
