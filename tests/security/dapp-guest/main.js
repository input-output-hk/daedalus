require('@swc-node/register');

const assert = require('assert');
const { once } = require('events');
const net = require('net');
const { app, BrowserWindow, session } = require('electron');
const {
  installDappTransportRestrictions,
  installGuestDenialHandlers,
} = require('../../../source/main/dapp/DappSessionPolicy');
const {
  DappEgressPolicy,
} = require('../../../source/main/dapp/DappEgressPolicy');

installDappTransportRestrictions(app.commandLine);
app.on('window-all-closed', () => undefined);

const deadline = setTimeout(() => {
  process.stderr.write('dApp egress fixture timed out\n');
  app.exit(1);
}, 20000);

const fail = (error) => {
  clearTimeout(deadline);
  process.stderr.write(`${error.stack || error}\n`);
  app.exit(1);
};

const listen = async (host) => {
  const target = { connections: 0, server: net.createServer() };
  target.server.on('connection', (socket) => {
    target.connections += 1;
    socket.destroy();
  });
  target.server.listen(0, host);
  await once(target.server, 'listening');
  target.port = target.server.address().port;
  return target;
};

const close = async (server) => {
  server.close();
  await once(server, 'close');
};

app
  .whenReady()
  .then(async () => {
    const ipv4 = await listen('127.0.0.1');
    const httpsUrl = `https://127.0.0.1:${ipv4.port}/`;
    const wssUrl = `wss://[::ffff:127.0.0.1]:${ipv4.port}/`;
    const guestSession = session.fromPartition(`dapp-egress-${Date.now()}`, {
      cache: false,
    });
    const policy = await DappEgressPolicy.install(
      guestSession,
      new Set([new URL(httpsUrl).origin, new URL(wssUrl).origin])
    );

    const httpsProxy = await guestSession.resolveProxy(httpsUrl);
    const wssProxy = await guestSession.resolveProxy(wssUrl);
    assert.match(httpsProxy, /^PROXY 127\.0\.0\.1:\d+$/u);
    assert.match(wssProxy, /^PROXY 127\.0\.0\.1:\d+$/u);
    assert.strictEqual(wssProxy, httpsProxy);

    const guest = new BrowserWindow({
      show: false,
      webPreferences: {
        session: guestSession,
        nodeIntegration: false,
        nodeIntegrationInWorker: false,
        nodeIntegrationInSubFrames: false,
        contextIsolation: true,
        sandbox: true,
        webSecurity: true,
        devTools: false,
        disableBlinkFeatures: 'DirectSockets,WebTransport',
      },
    });
    installGuestDenialHandlers(guest.webContents);

    await assert.rejects(guest.loadURL(httpsUrl));
    await guest.loadURL('about:blank');
    const websocketResult = await guest.webContents.executeJavaScript(`
      new Promise((resolve) => {
        const socket = new WebSocket(${JSON.stringify(wssUrl)});
        socket.onopen = () => resolve('opened');
        socket.onerror = () => resolve('blocked');
        setTimeout(() => resolve('timeout'), 5000);
      })
    `);
    assert.strictEqual(websocketResult, 'blocked');
    assert.strictEqual(ipv4.connections, 0);

    guest.destroy();
    await policy.close();
    await close(ipv4.server);

    const result = {
      httpsProxy,
      wssProxy,
      ipv4Connections: ipv4.connections,
      mappedIpv6Connections: ipv4.connections,
      websocketResult,
    };
    process.stderr.write(`${JSON.stringify(result)}\n`);
    clearTimeout(deadline);
    app.exit(0);
  })
  .catch(fail);
