import { EventEmitter } from 'events';
import { session } from 'electron';
import type { CommandLine, Session, WebContents } from 'electron';
import { DappEgressPolicy } from './DappEgressPolicy';
import {
  createDappSession,
  installDappSessionPolicy,
  installDappTransportRestrictions,
  installGuestDenialHandlers,
} from './DappSessionPolicy';

jest.mock('electron', () => ({
  session: { fromPartition: jest.fn() },
}));
jest.mock('./DappEgressPolicy', () => ({
  DappEgressPolicy: { install: jest.fn() },
}));

const makeSession = () => {
  const events = new EventEmitter();
  const handlers: Record<string, jest.Mock> = {};
  const guestSession = (Object.assign(events, {
    isPersistent: jest.fn(() => false),
    getStoragePath: jest.fn(() => null),
    setPermissionCheckHandler: jest.fn((handler) => {
      handlers.permissionCheck = handler;
    }),
    setPermissionRequestHandler: jest.fn((handler) => {
      handlers.permissionRequest = handler;
    }),
    setDevicePermissionHandler: jest.fn((handler) => {
      handlers.devicePermission = handler;
    }),
    setDisplayMediaRequestHandler: jest.fn((handler) => {
      handlers.displayMedia = handler;
    }),
    webRequest: {
      onBeforeRequest: jest.fn((_filter, handler) => {
        handlers.beforeRequest = handler;
      }),
      onHeadersReceived: jest.fn((_filter, handler) => {
        handlers.headersReceived = handler;
      }),
    },
  }) as unknown) as Session;
  return { guestSession, handlers };
};

const preventedEvent = () => ({ preventDefault: jest.fn() });

describe('DappSessionPolicy', () => {
  test('composes transport restrictions without dropping existing features', () => {
    const appendSwitch = jest.fn();
    installDappTransportRestrictions(({
      getSwitchValue: jest.fn(() => 'ExistingFeature'),
      appendSwitch,
    } as unknown) as CommandLine);
    expect(appendSwitch).toHaveBeenNthCalledWith(
      1,
      'disable-features',
      'ExistingFeature,DirectSockets,WebTransport'
    );
    expect(appendSwitch).toHaveBeenNthCalledWith(2, 'disable-quic');
  });

  test('creates a fresh nonpersistent cacheless partition every time', () => {
    const first = makeSession().guestSession;
    const second = makeSession().guestSession;
    (session.fromPartition as jest.Mock)
      .mockReturnValueOnce(first)
      .mockReturnValueOnce(second);

    expect(createDappSession()).toBe(first);
    expect(createDappSession()).toBe(second);
    const [
      firstPartition,
      firstOptions,
    ] = (session.fromPartition as jest.Mock).mock.calls[0];
    const [
      secondPartition,
    ] = (session.fromPartition as jest.Mock).mock.calls[1];
    expect(firstPartition).toMatch(/^daedalus-dapp-[0-9a-f]{32}$/u);
    expect(secondPartition).not.toBe(firstPartition);
    expect(firstOptions).toEqual({ cache: false });
  });

  test('denies permissions, devices, display capture, downloads, and other origins', async () => {
    const { guestSession, handlers } = makeSession();
    const egressPolicy = { close: jest.fn() };
    (DappEgressPolicy.install as jest.Mock).mockResolvedValue(egressPolicy);
    await expect(
      installDappSessionPolicy(guestSession, new Set(['https://example.com']))
    ).resolves.toBe(egressPolicy);

    expect(handlers.permissionCheck()).toBe(false);
    const permissionCallback = jest.fn();
    handlers.permissionRequest(null, 'media', permissionCallback);
    expect(permissionCallback).toHaveBeenCalledWith(false);
    expect(handlers.devicePermission()).toBe(false);
    const displayCallback = jest.fn();
    handlers.displayMedia(null, displayCallback);
    expect(displayCallback).toHaveBeenCalledWith({});

    const downloadEvent = preventedEvent();
    guestSession.emit('will-download', downloadEvent);
    expect(downloadEvent.preventDefault).toHaveBeenCalled();

    const allowed = jest.fn();
    handlers.beforeRequest({ url: 'https://example.com/resource' }, allowed);
    expect(allowed).toHaveBeenCalledWith({ cancel: false });
    const denied = jest.fn();
    handlers.beforeRequest({ url: 'https://evil.test/resource' }, denied);
    expect(denied).toHaveBeenCalledWith({ cancel: true });
    const headersCallback = jest.fn();
    handlers.headersReceived(
      {
        responseHeaders: {
          'content-security-policy': ["default-src 'self'"],
        },
      },
      headersCallback
    );
    expect(headersCallback).toHaveBeenCalledWith({
      responseHeaders: {
        'content-security-policy': [
          "default-src 'self'",
          "frame-src 'none'; child-src 'none'; worker-src 'none'",
        ],
      },
    });
    expect(DappEgressPolicy.install).toHaveBeenCalledWith(
      guestSession,
      new Set(['https://example.com'])
    );
  });

  test('denies popups, authentication, certificate exceptions, and client certificates', () => {
    const events = new EventEmitter();
    const setWindowOpenHandler = jest.fn();
    const setWebRTCIPHandlingPolicy = jest.fn();
    const contents = (Object.assign(events, {
      setWindowOpenHandler,
      setWebRTCIPHandlingPolicy,
    }) as unknown) as WebContents;
    installGuestDenialHandlers(contents);

    expect(setWindowOpenHandler.mock.calls[0][0]()).toEqual({ action: 'deny' });
    expect(setWebRTCIPHandlingPolicy).toHaveBeenCalledWith(
      'disable_non_proxied_udp'
    );

    const loginEvent = preventedEvent();
    const loginCallback = jest.fn();
    contents.emit('login', loginEvent, {}, {}, loginCallback);
    expect(loginEvent.preventDefault).toHaveBeenCalled();
    expect(loginCallback).toHaveBeenCalledWith();

    const certificateEvent = preventedEvent();
    const certificateCallback = jest.fn();
    contents.emit(
      'certificate-error',
      certificateEvent,
      'https://example.com',
      'ERR_CERT_INVALID',
      {},
      certificateCallback
    );
    expect(certificateEvent.preventDefault).toHaveBeenCalled();
    expect(certificateCallback).toHaveBeenCalledWith(false);

    const clientCertificateEvent = preventedEvent();
    const clientCertificateCallback = jest.fn();
    contents.emit(
      'select-client-certificate',
      clientCertificateEvent,
      'https://example.com',
      [{}],
      clientCertificateCallback
    );
    expect(clientCertificateEvent.preventDefault).toHaveBeenCalled();
    expect(clientCertificateCallback).toHaveBeenCalledWith();
  });
});
