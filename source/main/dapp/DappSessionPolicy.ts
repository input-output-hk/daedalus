import crypto from 'crypto';
import { session } from 'electron';
import type { CommandLine, Session, WebContents } from 'electron';
import { isAllowedDappResourceUrl } from './urlPolicy';

const DISABLED_BLINK_FEATURES = ['DirectSockets', 'WebTransport'];

export const installDappTransportRestrictions = (
  commandLine: CommandLine
): void => {
  const disabled = new Set(
    commandLine
      .getSwitchValue('disable-features')
      .split(',')
      .map((feature) => feature.trim())
      .filter(Boolean)
  );
  DISABLED_BLINK_FEATURES.forEach((feature) => disabled.add(feature));
  commandLine.appendSwitch('disable-features', [...disabled].join(','));
  commandLine.appendSwitch('disable-quic');
};

export const createDappSession = (): Session => {
  const partition = `daedalus-dapp-${crypto.randomBytes(16).toString('hex')}`;
  const guestSession = session.fromPartition(partition, { cache: false });
  if (guestSession.isPersistent() || guestSession.getStoragePath() !== null)
    throw new Error('DApp session is persistent');
  return guestSession;
};

export const installDappSessionPolicy = (
  guestSession: Session,
  allowedResourceOrigins: ReadonlySet<string>
): void => {
  guestSession.setPermissionCheckHandler(() => false);
  guestSession.setPermissionRequestHandler((_contents, _permission, callback) =>
    callback(false)
  );
  guestSession.setDevicePermissionHandler(() => false);
  guestSession.setDisplayMediaRequestHandler((_request, callback) =>
    callback({})
  );

  guestSession.on('will-download', (event) => event.preventDefault());
  guestSession.on('select-hid-device', (event, _details, callback) => {
    event.preventDefault();
    callback();
  });
  guestSession.on(
    'select-serial-port',
    (event, _ports, _contents, callback) => {
      event.preventDefault();
      callback('');
    }
  );
  guestSession.on('select-usb-device', (event, _details, callback) => {
    event.preventDefault();
    callback();
  });

  guestSession.webRequest.onBeforeRequest(
    { urls: ['<all_urls>'] },
    (details, callback) =>
      callback({
        cancel: !isAllowedDappResourceUrl(details.url, allowedResourceOrigins),
      })
  );
  guestSession.webRequest.onHeadersReceived(
    { urls: ['<all_urls>'] },
    (details, callback) => {
      const responseHeaders = { ...details.responseHeaders };
      const existingKey = Object.keys(responseHeaders).find(
        (key) => key.toLowerCase() === 'content-security-policy'
      );
      const key = existingKey || 'Content-Security-Policy';
      responseHeaders[key] = [
        ...(responseHeaders[key] || []),
        "frame-src 'none'; child-src 'none'; worker-src 'none'",
      ];
      callback({ responseHeaders });
    }
  );
};

export const installGuestDenialHandlers = (webContents: WebContents): void => {
  webContents.setWebRTCIPHandlingPolicy('disable_non_proxied_udp');
  webContents.setWindowOpenHandler(() => ({ action: 'deny' }));
  webContents.on('login', (event, _details, _authInfo, callback) => {
    event.preventDefault();
    callback();
  });
  webContents.on(
    'certificate-error',
    (event, _url, _error, _certificate, callback) => {
      event.preventDefault();
      callback(false);
    }
  );
  webContents.on(
    'select-client-certificate',
    (event, _url, _certificates, callback) => {
      event.preventDefault();
      (callback as () => void)();
    }
  );
  webContents.on('select-bluetooth-device', (event, _devices, callback) => {
    event.preventDefault();
    callback('');
  });
};

export const clearDappSession = async (
  guestSession: Session
): Promise<void> => {
  await guestSession.closeAllConnections();
  await guestSession.clearStorageData();
  await guestSession.clearCache();
  await guestSession.clearAuthCache();
  await guestSession.clearHostResolverCache();
};
