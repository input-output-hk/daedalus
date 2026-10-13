import assert from 'assert';
import http from 'http';
import path from 'path';
import { once } from 'events';
import { setImmediate } from 'timers';
import { promisify } from 'util';
import { app, BrowserWindow, ipcMain } from 'electron';
import type { Session, WebFrameMain } from 'electron';
import { IpcChannel } from '../../common/ipc/lib/IpcChannel';
import { IpcConversation } from '../../common/ipc/lib/IpcConversation';
import { MainIpcChannel } from '../ipc/lib/MainIpcChannel';
import { MainIpcConversation } from '../ipc/lib/MainIpcConversation';
import { bindTrustedRenderer } from '../ipc/lib/trustedRendererIpcAuthority';
import { privilegedIpcManifest } from '../ipc/privilegedIpcManifest';
import {
  startDappSandboxAvailabilityCheck,
  validateDappRendererSandbox,
} from '../sandbox/dappSandboxAvailability';
import {
  DappBrowserManager,
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
import { readDappRuntimeConfig, type DappRuntimeConfig } from '../utils/config';

const nextEventLoopTurn = promisify(setImmediate);

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
type HarnessRuntimeConfig = DappRuntimeConfig;

const fixtureHtml = `<!doctype html><meta charset="utf-8"><title>fixture</title><body>fixture</body>`;

const createSession = (): Session => {
  const guestSession = createDappSession();
  sessions.add(guestSession);
  guestSession.protocol.handle('https', (request) => {
    if (new URL(request.url).origin !== fixtureOrigin)
      return guestSession.fetch(request, {
        bypassCustomProtocolHandlers: true,
      });
    return Promise.resolve(
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
    );
  });
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

const testPackagedPolicy = (config: HarnessRuntimeConfig): void => {
  const expectedEnabled = config.dappSandboxPackageCluster !== 'mainnet';
  const packagedPolicy = new DappLaunchPolicy(config.dappBrowserPolicy);
  assert.deepStrictEqual(packagedPolicy.config, {
    revision: DAPP_POLICY_REVISION,
    globalEnabled: expectedEnabled,
    preferredCatalogEnabled: expectedEnabled,
    diagnosticsEnabled: expectedEnabled,
    cip104Revision: expectedEnabled ? 1 : 0,
    cip142Revision: 0,
    hardwareConnectorEnabled: true,
  });
  const enabled = {
    revision: DAPP_POLICY_REVISION,
    globalEnabled: true,
    preferredCatalogEnabled: true,
    diagnosticsEnabled: true,
    cip104Revision: 1,
    cip142Revision: 1,
    hardwareConnectorEnabled: true,
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
  const conversations = IpcConversation._instances as Record<
    string,
    MainIpcConversation<unknown, unknown>
  >;
  const channels = IpcChannel._instances as Record<
    string,
    MainIpcChannel<unknown, unknown>
  >;
  let effects = 0;
  incoming.forEach((entry) => {
    const channel =
      entry.transport === 'conversation'
        ? conversations[entry.channel] ||
          new MainIpcConversation<unknown, unknown>(entry.channel)
        : channels[entry.channel] ||
          new MainIpcChannel<unknown, unknown>(entry.channel);
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
    (
      target as {
        emit: (eventName: string, ...values: unknown[]) => boolean;
      }
    ).emit(name, ...args);

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
    () => undefined,
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
      crossOriginDenied: await denied(() => fetch('https://other.invalid/')),
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

const testLifecyclePolicy = async (
  attack: 'reload' | 'redirect'
): Promise<void> => {
  const guestSession = createDappSession();
  sessions.add(guestSession);
  const guest = createWindow(guestSession);
  let tearingDown = false;
  let refreshedDocuments = 0;
  let revokedBeforeDestroy = false;
  let revocations = 0;
  const revoked = new Promise<string>((resolve) => {
    installDappGuestLifecyclePolicy(
      guest,
      `${fixtureOrigin}/`,
      'fixture',
      () => false,
      () => tearingDown,
      () => {
        refreshedDocuments += 1;
      },
      (reason) => {
        if (tearingDown) return;
        tearingDown = true;
        revocations += 1;
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
  if (attack === 'reload') {
    emit('did-start-navigation', event, `${fixtureOrigin}/`, false, true);
    assert.strictEqual(refreshedDocuments, 1);
    assert.strictEqual(revocations, 0);
    assert.strictEqual(event.defaultPrevented, false);
  } else {
    emit('will-redirect', event, 'https://other.invalid/', false, true);
    emit('will-redirect', event, 'https://other.invalid/', false, true);
    assert.strictEqual(await revoked, 'navigation');
    assert.strictEqual(revocations, 1);
    assert(revokedBeforeDestroy);
    assert(event.defaultPrevented);
  }
  destroyWindow(guest);
  await cleanupSession(guestSession);
};

const testNativeLifecycleTeardown = async (
  attack: 'initial-child' | 'visible-child' | 'visible-top-level'
): Promise<void> => {
  let targetRequests = 0;
  const targetServer = http.createServer((_request, response) => {
    targetRequests += 1;
    response.writeHead(200, {
      'content-type': 'text/html',
      connection: 'close',
    });
    response.end('<!doctype html><body>forbidden destination</body>');
  });
  targetServer.listen(0, '127.0.0.2');
  await once(targetServer, 'listening');
  const targetAddress = targetServer.address();
  assert(targetAddress && typeof targetAddress !== 'string');
  const targetUrl = `http://127.0.0.2:${targetAddress.port}/child`;
  const server = http.createServer((_request, response) => {
    response.writeHead(200, {
      'content-type': 'text/html',
      connection: 'close',
      'set-cookie': 'native-lifecycle=fixture; SameSite=Lax',
    });
    response.end(
      `<!doctype html><title>native lifecycle fixture</title><script>
        window.childFrameBlocked = false;
        document.addEventListener('securitypolicyviolation', event => {
          if (event.effectiveDirective === 'frame-src')
            window.childFrameBlocked = true;
        });
      </script><body>fixture${
        attack === 'initial-child' ? `<iframe src="${targetUrl}"></iframe>` : ''
      }</body>`
    );
  });
  server.listen(0, '127.0.0.1');
  await once(server, 'listening');
  const address = server.address();
  assert(address && typeof address !== 'string');
  const origin = `http://127.0.0.1:${address.port}`;
  let guest: BrowserWindow | undefined;
  let guestSession: Session | undefined;
  let nativeClosed: Promise<unknown[]> | undefined;
  let closedEvents = 0;
  let revocations = 0;
  let revokedWhileAlive = false;
  let inactiveOnRevocation = false;
  let dispatchUnwoundSafely = false;
  const manager = new DappBrowserManager((reason, _id, isOpen) => {
    revocations += 1;
    revokedWhileAlive =
      reason === 'navigation' && !!guest && !guest.isDestroyed();
    inactiveOnRevocation = !isOpen && !manager.isOpen;
  });
  const captureWindow = (_event: unknown, window: BrowserWindow) => {
    guest = window;
    guestSession = window.webContents.session;
    windows.add(window);
    sessions.add(window.webContents.session);
    nativeClosed = once(window, 'closed');
    window.on('closed', () => {
      closedEvents += 1;
      windows.delete(window);
    });
  };
  app.once('browser-window-created', captureWindow);
  try {
    await manager.launchDiagnostics(
      `${origin}/`,
      origin,
      'native lifecycle fixture',
      { allowHttpLoopback: true },
      false
    );
    assert(guest && guestSession && nativeClosed);
    assert(guest.isVisible());
    assert(validateDappRendererSandbox(guest.webContents));
    const authority = manager.authenticate({
      sender: guest.webContents,
      senderFrame: guest.webContents.mainFrame,
    } as unknown as Parameters<DappBrowserManager['authenticate']>[0]);
    assert(authority && authority.isCurrent());
    if (attack !== 'initial-child') {
      guest.webContents.on('did-start-navigation', () => {
        if (!manager.isOpen)
          dispatchUnwoundSafely =
            !!guest && !guest.isDestroyed() && !authority.isCurrent();
      });
      guest.webContents.on('will-frame-navigate', () => {
        if (!manager.isOpen)
          dispatchUnwoundSafely =
            !!guest && !guest.isDestroyed() && !authority.isCurrent();
      });
      guest.webContents.on('will-navigate', () => {
        if (!manager.isOpen)
          dispatchUnwoundSafely =
            !!guest && !guest.isDestroyed() && !authority.isCurrent();
      });
      await guest.webContents.executeJavaScript(
        `setTimeout(() => {
          ${
            attack === 'visible-child'
              ? `const frame = document.createElement('iframe');
                 frame.src = ${JSON.stringify(targetUrl)};
                 document.body.appendChild(frame);`
              : `location.href = ${JSON.stringify(targetUrl)};`
          }
        }, 25); undefined;`
      );
    }
    if (attack === 'visible-top-level') {
      await nativeClosed;
      await nextEventLoopTurn();
      assert.strictEqual(revocations, 1);
      assert(revokedWhileAlive && inactiveOnRevocation);
      assert(dispatchUnwoundSafely);
      assert.strictEqual(manager.isOpen, false);
    } else {
      // Observe actual CSP enforcement, not a delay or synthetic frame event.
      await guest.webContents.executeJavaScript(`{
        const { promise, resolve } = Promise.withResolvers();
        if (window.childFrameBlocked) resolve();
        else document.addEventListener('securitypolicyviolation', event => {
          if (event.effectiveDirective === 'frame-src') resolve();
        });
        promise;
      }`);
      await nextEventLoopTurn();
      assert.strictEqual(revocations, 0);
      assert.strictEqual(closedEvents, 0);
      assert(manager.isOpen && authority.isCurrent());
      const childFrames = guest.webContents.mainFrame.frames;
      assert.strictEqual(childFrames.length, 1);
      for (const child of childFrames)
        assert.strictEqual(
          manager.authenticate({
            sender: guest.webContents,
            senderFrame: child,
          } as unknown as Parameters<DappBrowserManager['authenticate']>[0]),
          null
        );
      await manager.close();
      assert.strictEqual(revocations, 1);
      assert.strictEqual(authority.isCurrent(), false);
    }
    await nativeClosed;
    // Resume after native dispatch: "closed" alone did not prove survival.
    await nextEventLoopTurn();
    assert.strictEqual(closedEvents, 1);
    assert.strictEqual(targetRequests, 0);
    assert.deepStrictEqual(await guestSession.cookies.get({}), []);
  } finally {
    app.removeListener('browser-window-created', captureWindow);
    await manager.close();
    if (guestSession) sessions.delete(guestSession);
    for (const fixtureServer of [server, targetServer]) {
      await promisify(fixtureServer.close).call(fixtureServer);
    }
  }
};

const cleanup = async (): Promise<void> => {
  windows.forEach(destroyWindow);
  await Promise.all([...sessions].map(cleanupSession));
};

app.whenReady().then(async () => {
  try {
    const configPath = process.env.DAEDALUS_CONFIG_FILE;
    assert(configPath);
    const runtimeConfig = readDappRuntimeConfig(configPath);
    const packageCluster = runtimeConfig.dappSandboxPackageCluster;
    const installRoot =
      process.env.ENTRYPOINT_DIR ||
      (process.platform === 'darwin'
        ? path.resolve(path.dirname(process.execPath), '..', '..')
        : path.dirname(path.dirname(configPath)));
    assert.deepStrictEqual(
      await startDappSandboxAvailabilityCheck({
        isDevelopment: false,
        cluster: packageCluster,
        installRoot,
      }),
      { status: 'available' }
    );
    process.stderr.write('sandbox available\n');
    testPackagedPolicy(runtimeConfig);
    process.stderr.write('policy variants passed\n');
    const manifestChannels = await testPrivilegedIpc();
    process.stderr.write('privileged IPC passed\n');
    await testLifecyclePolicy('reload');
    process.stderr.write('reload refresh passed\n');
    await testLifecyclePolicy('redirect');
    process.stderr.write('redirect revocation passed\n');
    await testNativeLifecycleTeardown('initial-child');
    await testNativeLifecycleTeardown('visible-child');
    await testNativeLifecycleTeardown('visible-top-level');
    process.stderr.write('native frame lifecycle teardown passed\n');
    await testGuestDenialsAndStorage();
    process.stderr.write('guest denials passed\n');
    assert.deepStrictEqual(unhandledRejections, []);
    await cleanup();
    clearTimeout(deadline);
    process.stdout.write(
      `${JSON.stringify({
        schemaVersion: 2,
        task802IpcMatrix: true,
        task802TransportMatrix: true,
        task802DestinationBindingMatrix: true,
        task802LifecycleRaceMatrix: true,
        task802SwitchVariantMatrix: true,
        task802NonpersistentStorageMatrix: true,
        packagedPolicyVariants: true,
        manifestChannels,
        wrongSenderRejected: true,
        subframeRejected: true,
        navigationRevoked: true,
        reloadAuthorityRotated: true,
        nativeFrameLifecycleTeardown: true,
        destinationBindingDenied: true,
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
