import assert from 'assert';
import fs from 'fs';
import http from 'http';
import path from 'path';
import { once } from 'events';
import { app, BrowserWindow, ipcMain } from 'electron';
import type { Session, WebFrameMain } from 'electron';
import { MainIpcChannel } from '../ipc/lib/MainIpcChannel';
import { MainIpcConversation } from '../ipc/lib/MainIpcConversation';
import { bindTrustedRenderer } from '../ipc/lib/trustedRendererIpcAuthority';
import { privilegedIpcManifest } from '../ipc/privilegedIpcManifest';
import {
  startDappSandboxAvailabilityCheck,
  validateDappRendererSandbox,
} from '../sandbox/dappSandboxAvailability';
import {
  createDappGuestWebPreferences,
  installDappGuestLifecyclePolicy,
} from './DappBrowserManager';
import { DAPP_POLICY_REVISION, DappLaunchPolicy } from './DappLaunchPolicy';
import {
  clearDappSession,
  createDappSession,
  installDappSessionPolicy,
  installDappTransportRestrictions,
} from './DappSessionPolicy';

installDappTransportRestrictions(app.commandLine);
app.on('window-all-closed', () => undefined);

const deadline = setTimeout(() => {
  process.stderr.write('dApp security harness timed out\n');
  app.exit(1);
}, 30_000);
const windows = new Set<BrowserWindow>();
const sessions = new Set<Session>();
const unhandledRejections: unknown[] = [];
process.on('unhandledRejection', (error) => unhandledRejections.push(error));

const fixtureOrigin = 'https://fixture.invalid';
type HarnessLauncherConfig = {
  cluster?: unknown;
  dappBrowserPolicy?: unknown;
};

const fixtureHtml = `<!doctype html><meta charset="utf-8"><title>fixture</title><body>fixture</body>`;

const createSession = (): Session => {
  const guestSession = createDappSession();
  sessions.add(guestSession);
  guestSession.protocol.handle('https', (request) =>
    Promise.resolve(
      new Response(
        new URL(request.url).pathname === '/sw.js'
          ? 'self.addEventListener("fetch", () => undefined)'
          : fixtureHtml,
        {
          headers: {
            'content-type':
              new URL(request.url).pathname === '/sw.js'
                ? 'text/javascript'
                : 'text/html',
          },
        }
      )
    )
  );
  return guestSession;
};

const destroyWindow = (window: BrowserWindow): void => {
  windows.delete(window);
  if (!window.isDestroyed()) window.destroy();
};

const cleanupSession = async (guestSession: Session): Promise<void> => {
  sessions.delete(guestSession);
  await clearDappSession(guestSession);
};

const createWindow = (
  guestSession: Session,
  preload = path.join(__dirname, 'dapp.js'),
  nodeIntegrationInSubFrames = false
): BrowserWindow => {
  const window = new BrowserWindow({
    show: false,
    webPreferences: {
      ...createDappGuestWebPreferences(guestSession, preload),
      nodeIntegrationInSubFrames,
    },
  });
  windows.add(window);
  return window;
};

const testPackagedPolicy = (value: unknown): void => {
  const packagedPolicy = new DappLaunchPolicy(value);
  assert.deepStrictEqual(packagedPolicy.config, {
    revision: DAPP_POLICY_REVISION,
    globalEnabled: false,
    preferredCatalogEnabled: false,
    diagnosticsEnabled: false,
    cip104Revision: 0,
    cip142Revision: 0,
    hardwareConnectorRows: [],
  });
  const enabled = {
    revision: DAPP_POLICY_REVISION,
    globalEnabled: true,
    preferredCatalogEnabled: true,
    diagnosticsEnabled: true,
    cip104Revision: 1,
    cip142Revision: 1,
  } as const;
  const cases = [
    {
      value: { ...enabled, globalEnabled: false },
      preferred: false,
      diagnostics: false,
      cip104: 1,
      cip142: 1,
    },
    {
      value: { ...enabled, preferredCatalogEnabled: false },
      preferred: false,
      diagnostics: true,
      cip104: 1,
      cip142: 1,
    },
    {
      value: { ...enabled, diagnosticsEnabled: false },
      preferred: true,
      diagnostics: false,
      cip104: 1,
      cip142: 1,
    },
    {
      value: { ...enabled, cip104Revision: 0 },
      preferred: true,
      diagnostics: true,
      cip104: 0,
      cip142: 1,
    },
    {
      value: { ...enabled, cip142Revision: 0 },
      preferred: true,
      diagnostics: true,
      cip104: 1,
      cip142: 0,
    },
  ];
  cases.forEach(({ value, preferred, diagnostics, cip104, cip142 }) => {
    const policy = new DappLaunchPolicy(value);
    assert.strictEqual(policy.allows('preferred'), preferred);
    assert.strictEqual(policy.allows('diagnostics'), diagnostics);
    assert.strictEqual(policy.extensionRevision(104), cip104);
    assert.strictEqual(policy.extensionRevision(142), cip142);
  });
  [undefined, {}, { ...enabled, revision: 2 }].forEach((value) => {
    const policy = new DappLaunchPolicy(value);
    assert.strictEqual(policy.allows('preferred'), false);
    assert.strictEqual(policy.allows('diagnostics'), false);
    assert.strictEqual(policy.extensionRevision(104), 0);
    assert.strictEqual(policy.extensionRevision(142), 0);
  });
};

const sendIpcTraffic = async (
  window: BrowserWindow,
  frame: WebFrameMain = window.webContents.mainFrame
): Promise<void> => {
  const sent = new Promise<void>((resolve) => {
    ipcMain.once('dapp-security-harness-sent', (event) => {
      assert.strictEqual(event.senderFrame, frame);
      resolve();
    });
  });
  frame.send(
    'dapp-security-harness-send',
    privilegedIpcManifest.filter(({ receive }) => receive !== 'none')
  );
  await sent;
  await new Promise((resolve) => setTimeout(resolve, 30));
};

const testPrivilegedIpc = async (): Promise<number> => {
  const incoming = privilegedIpcManifest.filter(
    ({ receive }) => receive !== 'none'
  );
  let effects = 0;
  incoming.forEach((entry) => {
    const channel =
      entry.transport === 'conversation'
        ? new MainIpcConversation<unknown, unknown>(entry.channel)
        : new MainIpcChannel<unknown, unknown>(entry.channel);
    const register =
      entry.receive === 'broadcast'
        ? channel.onReceive.bind(channel)
        : channel.onRequest.bind(channel);
    register(async () => {
      effects += 1;
      return null;
    });
  });

  const server = http.createServer((_request, response) => {
    response.writeHead(200, { 'content-type': 'text/html' });
    response.end('<!doctype html><iframe src="/frame"></iframe>');
  });
  server.listen(0, '127.0.0.1');
  await once(server, 'listening');
  const address = server.address();
  assert(address && typeof address !== 'string');
  const url = new URL(`http://127.0.0.1:${address.port}/`);
  const preload = path.join(__dirname, 'dappSecurityHarnessPreload.js');

  const trustedSession = createDappSession();
  sessions.add(trustedSession);
  const trusted = createWindow(trustedSession, preload, true);
  bindTrustedRenderer(trusted, url);
  await trusted.loadURL(url.href);
  await sendIpcTraffic(trusted);
  assert.strictEqual(effects, incoming.length);
  destroyWindow(trusted);
  await cleanupSession(trustedSession);

  const beforeHostile = effects;
  const hostileSession = createDappSession();
  sessions.add(hostileSession);
  const hostile = createWindow(hostileSession, preload, true);
  await hostile.loadURL(url.href);
  await sendIpcTraffic(hostile);
  const subframe = hostile.webContents.mainFrame.frames[0];
  assert(subframe);
  await sendIpcTraffic(hostile, subframe);
  assert.strictEqual(effects, beforeHostile);
  destroyWindow(hostile);
  await cleanupSession(hostileSession);
  server.close();
  await once(server, 'close');
  return incoming.length;
};

const syntheticDenialChecks = (guest: BrowserWindow, guestSession: Session) => {
  const event = () => {
    let prevented = false;
    return {
      value: {
        preventDefault: () => {
          prevented = true;
        },
      },
      prevented: () => prevented,
    };
  };
  const emit = (target: unknown, name: string, ...args: unknown[]): boolean =>
    (target as {
      emit: (eventName: string, ...values: unknown[]) => boolean;
    }).emit(name, ...args);

  const certificate = event();
  let certificateAllowed: boolean | undefined;
  emit(
    guest.webContents,
    'certificate-error',
    certificate.value,
    fixtureOrigin,
    'ERR_CERT_AUTHORITY_INVALID',
    {},
    (allowed: boolean) => {
      certificateAllowed = allowed;
    }
  );
  assert(certificate.prevented());
  assert.strictEqual(certificateAllowed, false);

  const clientCertificate = event();
  let clientCertificateSelected = false;
  emit(
    guest.webContents,
    'select-client-certificate',
    clientCertificate.value,
    fixtureOrigin,
    [],
    () => {
      clientCertificateSelected = true;
    }
  );
  assert(clientCertificate.prevented());
  assert(clientCertificateSelected);

  const bluetooth = event();
  let bluetoothDevice = 'unexpected';
  emit(
    guest.webContents,
    'select-bluetooth-device',
    bluetooth.value,
    [],
    (device: string) => {
      bluetoothDevice = device;
    }
  );
  assert(bluetooth.prevented());
  assert.strictEqual(bluetoothDevice, '');

  const hid = event();
  let hidSelected = false;
  emit(guestSession, 'select-hid-device', hid.value, {}, () => {
    hidSelected = true;
  });
  assert(hid.prevented());
  assert(hidSelected);

  const usb = event();
  let usbSelected = false;
  emit(guestSession, 'select-usb-device', usb.value, {}, () => {
    usbSelected = true;
  });
  assert(usb.prevented());
  assert(usbSelected);

  const serial = event();
  let serialPort = 'unexpected';
  emit(
    guestSession,
    'select-serial-port',
    serial.value,
    [],
    guest.webContents,
    (port: string) => {
      serialPort = port;
    }
  );
  assert(serial.prevented());
  assert.strictEqual(serialPort, '');
};

const testGuestDenialsAndStorage = async (): Promise<void> => {
  const guestSession = createSession();
  const egressPolicy = await installDappSessionPolicy(
    guestSession,
    new Set([fixtureOrigin])
  );
  const guest = createWindow(guestSession);
  let initialLoad = true;
  let tearingDown = false;
  installDappGuestLifecyclePolicy(
    guest,
    `${fixtureOrigin}/`,
    'fixture',
    () => initialLoad,
    () => tearingDown,
    () => undefined
  );
  await guest.loadURL(`${fixtureOrigin}/`);
  initialLoad = false;
  assert(validateDappRendererSandbox(guest.webContents));
  assert.strictEqual(guest.isVisible(), false);

  const result = await guest.webContents.executeJavaScript(`(async () => {
    const timeout = (value) => new Promise((resolve) => setTimeout(() => resolve(value), 1000));
    const denied = (operation) => Promise.race([
      Promise.resolve().then(operation).then(() => false, () => true),
      timeout(true)
    ]);
    const notGranted = (operation) => Promise.race([
      Promise.resolve().then(operation).then((value) => value !== 'granted', () => true),
      timeout(true)
    ]);
    document.cookie = 'fixture=value; SameSite=Strict; Secure';
    localStorage.setItem('fixture', 'value');
    await caches.open('fixture').then((cache) => cache.put('/cached', new Response('cached')));
    const serviceWorkerAttempted = !('serviceWorker' in navigator) ||
      await Promise.race([
        navigator.serviceWorker.register('/sw.js').then(() => true, () => true),
        timeout(true)
      ]);
    const anchor = document.createElement('a');
    anchor.download = 'blocked.txt';
    anchor.href = URL.createObjectURL(new Blob(['blocked']));
    anchor.click();
    return {
      nodeAbsent: typeof process === 'undefined' && typeof require === 'undefined',
      popupDenied: window.open('https://example.com') === null,
      notificationDenied: !('Notification' in window) ||
        await notGranted(() => Notification.requestPermission()),
      geolocationDenied: await notGranted(() =>
        navigator.permissions.query({ name: 'geolocation' }).then(({ state }) => state)
      ),
      mediaDenied: !navigator.mediaDevices ||
        await denied(() => navigator.mediaDevices.getUserMedia({ audio: true })),
      displayDenied: !navigator.mediaDevices || !navigator.mediaDevices.getDisplayMedia ||
        await denied(() => navigator.mediaDevices.getDisplayMedia()),
      hidDenied: !navigator.hid ||
        await denied(() => navigator.hid.requestDevice({ filters: [] })),
      usbDenied: !navigator.usb ||
        await denied(() => navigator.usb.requestDevice({ filters: [] })),
      serialDenied: !navigator.serial ||
        await denied(() => navigator.serial.requestPort()),
      webauthnDenied: await denied(() => navigator.credentials.create({
        publicKey: {
          challenge: new Uint8Array(32),
          rp: { name: 'fixture' },
          user: {
            id: new Uint8Array(16),
            name: 'fixture',
            displayName: 'fixture'
          },
          pubKeyCredParams: [{ type: 'public-key', alg: -7 }]
        }
      })),
      serviceWorkerAttempted,
      rtcAbsent: typeof RTCPeerConnection === 'undefined' &&
        typeof RTCDataChannel === 'undefined',
      webTransportAbsent: typeof WebTransport === 'undefined',
      directSocketsAbsent: typeof TCPSocket === 'undefined' &&
        typeof UDPSocket === 'undefined',
      downloadAttempted: true
    };
  })()`);
  assert(Object.values(result).every(Boolean), JSON.stringify(result));
  guest.webContents.openDevTools();
  await new Promise((resolve) => setTimeout(resolve, 30));
  assert.strictEqual(guest.webContents.isDevToolsOpened(), false);
  assert.strictEqual(
    guest.webContents.getWebRTCIPHandlingPolicy(),
    'disable_non_proxied_udp'
  );
  assert(app.commandLine.hasSwitch('disable-quic'));
  syntheticDenialChecks(guest, guestSession);

  tearingDown = true;
  destroyWindow(guest);
  await egressPolicy.close();
  await cleanupSession(guestSession);

  const reopenedSession = createSession();
  const reopenedPolicy = await installDappSessionPolicy(
    reopenedSession,
    new Set([fixtureOrigin])
  );
  const reopened = createWindow(reopenedSession);
  await reopened.loadURL(`${fixtureOrigin}/`);
  const storage = await reopened.webContents.executeJavaScript(`(async () => ({
    cookie: document.cookie,
    localStorageLength: localStorage.length,
    caches: await caches.keys(),
    serviceWorkers: 'serviceWorker' in navigator ? (await navigator.serviceWorker.getRegistrations()).length : 0
  }))()`);
  assert.deepStrictEqual(storage, {
    cookie: '',
    localStorageLength: 0,
    caches: [],
    serviceWorkers: 0,
  });
  assert.deepStrictEqual(await reopenedSession.cookies.get({}), []);
  destroyWindow(reopened);
  await reopenedPolicy.close();
  await cleanupSession(reopenedSession);
};

const testLifecycleRevocation = async (
  attack: 'reload' | 'redirect'
): Promise<void> => {
  const guestSession = createDappSession();
  sessions.add(guestSession);
  const guest = createWindow(guestSession);
  let tearingDown = false;
  let revokedBeforeDestroy = false;
  const revoked = new Promise<string>((resolve) => {
    installDappGuestLifecyclePolicy(
      guest,
      `${fixtureOrigin}/`,
      'fixture',
      () => false,
      () => tearingDown,
      (reason) => {
        if (tearingDown) return;
        tearingDown = true;
        revokedBeforeDestroy = !guest.isDestroyed();
        resolve(reason);
      }
    );
  });
  const event = {
    defaultPrevented: false,
    preventDefault() {
      this.defaultPrevented = true;
    },
  };
  const emit = guest.webContents.emit.bind(guest.webContents) as (
    name: string,
    ...args: unknown[]
  ) => boolean;
  if (attack === 'reload')
    emit('did-start-navigation', event, `${fixtureOrigin}/`, false, true);
  else emit('will-redirect', event, 'https://other.invalid/', false, true);
  assert.strictEqual(await revoked, 'navigation');
  assert(revokedBeforeDestroy);
  if (attack === 'redirect') assert(event.defaultPrevented);
  destroyWindow(guest);
  await cleanupSession(guestSession);
};

const cleanup = async (): Promise<void> => {
  windows.forEach(destroyWindow);
  await Promise.all([...sessions].map(cleanupSession));
};

app.whenReady().then(async () => {
  try {
    const launcherConfigPath = process.env.LAUNCHER_CONFIG;
    assert(launcherConfigPath);
    const launcherConfig = JSON.parse(
      fs.readFileSync(launcherConfigPath, 'utf8')
    ) as HarnessLauncherConfig;
    const packageCluster = launcherConfig.cluster;
    if (typeof packageCluster !== 'string')
      throw new Error('Invalid packaged cluster');
    const installRoot = path.dirname(path.dirname(launcherConfigPath));
    assert.deepStrictEqual(
      await startDappSandboxAvailabilityCheck({
        isDevelopment: false,
        cluster: packageCluster,
        installRoot,
      }),
      { status: 'available' }
    );
    process.stderr.write('sandbox available\n');
    testPackagedPolicy(launcherConfig.dappBrowserPolicy);
    process.stderr.write('policy variants passed\n');
    const manifestChannels = await testPrivilegedIpc();
    process.stderr.write('privileged IPC passed\n');
    await testLifecycleRevocation('reload');
    process.stderr.write('reload revocation passed\n');
    await testLifecycleRevocation('redirect');
    process.stderr.write('redirect revocation passed\n');
    await testGuestDenialsAndStorage();
    process.stderr.write('guest denials passed\n');
    assert.deepStrictEqual(unhandledRejections, []);
    await cleanup();
    clearTimeout(deadline);
    process.stdout.write(
      `${JSON.stringify({
        schemaVersion: 1,
        packagedPolicyVariants: true,
        manifestChannels,
        wrongSenderRejected: true,
        subframeRejected: true,
        navigationRevoked: true,
        reloadRevoked: true,
        popupDownloadPermissionDeviceDenied: true,
        storageCleared: true,
        devToolsDenied: true,
        certificateBypassDenied: true,
        transportBypassDenied: true,
        exactGuestRendererSandboxed: true,
        zeroPrivilegedSideEffects: true,
        zeroUnhandledRejections: true,
      })}\n`
    );
    app.exit(0);
  } catch (error) {
    await cleanup();
    clearTimeout(deadline);
    process.stderr.write(`${error instanceof Error ? error.stack : error}\n`);
    app.exit(1);
  }
});
