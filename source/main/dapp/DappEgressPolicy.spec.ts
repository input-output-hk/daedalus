import http from 'http';
import dns from 'dns';
import { EventEmitter, once } from 'events';
import net from 'net';
import type { Session } from 'electron';
import { DappEgressPolicy, isPublicGuestIp } from './DappEgressPolicy';

const install = async (origins = new Set(['https://example.com'])) => {
  const guestSession = ({ setProxy: jest.fn() } as unknown) as Session;
  const policy = await DappEgressPolicy.install(guestSession, origins);
  return { guestSession, policy };
};

const connect = (
  policy: DappEgressPolicy,
  host: string,
  port = 443
): Promise<net.Socket> =>
  ((policy as unknown) as Record<'connect', jest.Mock>).connect({ host, port });

const connectedSocket = (): net.Socket => {
  const socket = Object.assign(new EventEmitter(), {
    destroy: jest.fn(),
  });
  process.nextTick(() => socket.emit('connect'));
  return (socket as unknown) as net.Socket;
};

describe('DappEgressPolicy', () => {
  afterEach(() => jest.restoreAllMocks());

  test.each([
    '0.0.0.0',
    '255.255.255.255',
    '100.64.0.1',
    '127.0.0.1',
    '10.0.0.1',
    '172.16.0.1',
    '192.168.0.1',
    '169.254.1.1',
    '224.0.0.1',
    '192.0.2.1',
    '198.18.0.1',
    '::',
    '::1',
    'fc00::1',
    'fe80::1',
    'ff02::1',
    '2001:db8::1',
    '::ffff:127.0.0.1',
    '::ffff:192.168.0.1',
    'not-an-address',
  ])('rejects forbidden destination %s', (address) => {
    expect(isPublicGuestIp(address)).toBe(false);
  });

  test.each(['1.1.1.1', '8.8.8.8', '2606:4700:4700::1111'])(
    'allows public unicast destination %s',
    (address) => {
      expect(isPublicGuestIp(address)).toBe(true);
    }
  );

  test('installs one fixed proxy with no direct fallback', async () => {
    const { guestSession, policy } = await install();

    expect(guestSession.setProxy).toHaveBeenCalledWith({
      mode: 'fixed_servers',
      proxyRules: expect.stringMatching(
        /^http=127\.0\.0\.1:\d+;https=127\.0\.0\.1:\d+$/u
      ),
      proxyBypassRules: '<-loopback>',
    });

    await policy.close();
  });

  test('dials only the numeric public address from the current DNS answer', async () => {
    const lookup = jest.spyOn(dns.promises, 'lookup') as jest.Mock;
    lookup.mockResolvedValueOnce([
      { address: '93.184.216.34', family: 4 },
      { address: '192.168.0.1', family: 4 },
    ]);
    const createConnection = jest
      .spyOn(net, 'createConnection')
      .mockImplementation(() => connectedSocket());
    const { policy } = await install();

    await expect(connect(policy, 'example.com')).resolves.toBeDefined();
    expect(createConnection).toHaveBeenCalledTimes(1);
    expect(createConnection).toHaveBeenCalledWith({
      host: '93.184.216.34',
      family: 4,
      port: 443,
    });

    await policy.close();
  });

  test('rejects a rebinding answer without a hostname or private fallback dial', async () => {
    const lookup = jest.spyOn(dns.promises, 'lookup') as jest.Mock;
    lookup
      .mockResolvedValueOnce([{ address: '93.184.216.34', family: 4 }])
      .mockResolvedValueOnce([{ address: '127.0.0.1', family: 4 }]);
    const createConnection = jest
      .spyOn(net, 'createConnection')
      .mockImplementation(() => connectedSocket());
    const { policy } = await install();

    await connect(policy, 'example.com');
    await expect(connect(policy, 'example.com')).rejects.toThrow(
      'DApp destination unavailable'
    );
    expect(createConnection).toHaveBeenCalledTimes(1);
    expect(createConnection.mock.calls[0][0]).toMatchObject({
      host: '93.184.216.34',
    });

    await policy.close();
  });
  test('rejects hostnames resolving only to private IPv4 or IPv6', async () => {
    const lookup = jest.spyOn(dns.promises, 'lookup') as jest.Mock;
    lookup
      .mockResolvedValueOnce([{ address: '192.168.0.1', family: 4 }])
      .mockResolvedValueOnce([{ address: 'fd00::1', family: 6 }]);
    const createConnection = jest.spyOn(net, 'createConnection');
    const { policy } = await install();

    await expect(connect(policy, 'example.com')).rejects.toThrow(
      'DApp destination unavailable'
    );
    await expect(connect(policy, 'example.com')).rejects.toThrow(
      'DApp destination unavailable'
    );
    expect(createConnection).not.toHaveBeenCalled();

    await policy.close();
  });

  test('an allowlisted private literal remains forbidden for HTTPS and WSS', async () => {
    const createConnection = jest.spyOn(net, 'createConnection');
    const { policy } = await install(
      new Set(['https://127.0.0.1', 'wss://[::1]'])
    );

    await expect(connect(policy, '127.0.0.1')).rejects.toThrow(
      'DApp destination unavailable'
    );
    await expect(connect(policy, '::1')).rejects.toThrow(
      'DApp destination unavailable'
    );
    expect(createConnection).not.toHaveBeenCalled();

    await policy.close();
  });

  test('proxies development HTTP only to an actual loopback destination', async () => {
    const target = http.createServer((_request, response) =>
      response.end('ok')
    );
    target.listen(0, '127.0.0.1');
    await once(target, 'listening');
    const targetAddress = target.address();
    if (!targetAddress || typeof targetAddress === 'string')
      throw new Error('Expected target address');
    const guestSession = ({ setProxy: jest.fn() } as unknown) as Session;
    const policy = await DappEgressPolicy.install(
      guestSession,
      undefined,
      true
    );
    // Test-only inspection of the private listener used by the configured proxy.
    const inspectedPolicy = (policy as unknown) as { server: http.Server };
    const proxyAddress = inspectedPolicy.server.address();
    if (!proxyAddress || typeof proxyAddress === 'string')
      throw new Error('Expected proxy address');

    const body = await new Promise<string>((resolve, reject) => {
      http
        .get(
          {
            host: '127.0.0.1',
            port: proxyAddress.port,
            path: `http://127.0.0.1:${targetAddress.port}/fixture`,
          },
          (response) => {
            let value = '';
            response.setEncoding('utf8');
            response.on('data', (chunk) => {
              value += chunk;
            });
            response.once('end', () => resolve(value));
          }
        )
        .once('error', reject);
    });

    expect(body).toBe('ok');
    await policy.close();
    target.close();
    await once(target, 'close');
  });
});
