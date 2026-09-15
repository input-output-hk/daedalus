import { EventEmitter } from 'events';
import { checkAssetMetadataSourceHealth } from './checkAssetMetadataSourceHealth';

type Scripted = {
  statusCode?: number;
  body?: string;
  chunks?: Array<string>;
  responseError?: boolean;
  requestError?: boolean;
  silent?: boolean;
};

type Recorded = {
  hostname: string;
  port: number | string;
  path: string;
  method: string;
};

const calls: Array<Recorded> = [];

/**
 * A stand-in for `global.https`, which the preload supplies in the application
 * and nothing supplies under jsdom. It records what was asked for and plays back
 * a scripted response on the next tick, so the cases drive the parsing and the
 * timeout without opening a socket.
 */
const install = (script: Scripted) => {
  const requestEmitter = new EventEmitter() as EventEmitter & {
    end: () => void;
    destroy: () => void;
  };
  requestEmitter.end = () => {};
  requestEmitter.destroy = () => {};

  // jsdom has no `https` on the global; the preload puts one there in the
  // application, so a spec has to supply its own.
  global.https = {
    request: (options: Recorded, onResponse: (response: unknown) => void) => {
      calls.push(options);
      if (script.silent) return requestEmitter;
      setTimeout(() => {
        if (script.requestError) {
          requestEmitter.emit('error', new Error('socket'));
          return;
        }
        const response = new EventEmitter() as EventEmitter & {
          statusCode: number;
          destroy: () => void;
        };
        response.statusCode = script.statusCode ?? 200;
        response.destroy = () => {};
        onResponse(response);
        if (script.responseError) {
          response.emit('error', new Error('stream'));
          return;
        }
        (script.chunks ?? [script.body ?? '']).forEach((chunk) =>
          response.emit('data', chunk)
        );
        response.emit('end');
      }, 0);
      return requestEmitter;
    },
  } as unknown as typeof global.https;
};

const TIP = JSON.stringify([
  {
    hash: 'e8c6992d52cd74b577b79251e0351be25070797a0dbc486b2c284d0bf7aeea9c',
    epoch_no: 308,
    abs_slot: 131545218,
    epoch_slot: 345218,
    block_no: 5078119,
    block_time: 1787228418,
  },
]);

describe('checkAssetMetadataSourceHealth', () => {
  beforeEach(() => {
    calls.length = 0;
  });

  it('resolves the absolute slot a well-formed tip reports', async () => {
    install({ body: TIP });
    await expect(
      checkAssetMetadataSourceHealth('https://preprod.koios.rest/api/v1')
    ).resolves.toEqual({ absoluteSlot: 131545218 });
  });

  it('appends tip to the configured path without doubling the separator', async () => {
    install({ body: TIP });
    await checkAssetMetadataSourceHealth('https://preprod.koios.rest/api/v1/');
    expect(calls[0].path).toBe('/api/v1/tip');
    expect(calls[0].hostname).toBe('preprod.koios.rest');
    expect(calls[0].method).toBe('GET');
  });

  it('carries a non-default port through to the request', async () => {
    install({ body: TIP });
    await checkAssetMetadataSourceHealth('https://koios.example.com:8443');
    expect(calls[0].port).toBe('8443');
    expect(calls[0].path).toBe('/tip');
  });

  it('reads a tip that arrives in several chunks', async () => {
    install({ chunks: [TIP.slice(0, 20), TIP.slice(20)] });
    await expect(
      checkAssetMetadataSourceHealth('https://preprod.koios.rest/api/v1')
    ).resolves.toEqual({ absoluteSlot: 131545218 });
  });

  it('rejects a 200 carrying an object rather than an array', async () => {
    install({ body: JSON.stringify({ abs_slot: 131545218 }) });
    await expect(
      checkAssetMetadataSourceHealth('https://preprod.koios.rest/api/v1')
    ).rejects.toThrow('did not answer a tip');
  });

  it('rejects a 200 whose element carries no absolute slot', async () => {
    install({ body: JSON.stringify([{ block_no: 5078119 }]) });
    await expect(
      checkAssetMetadataSourceHealth('https://preprod.koios.rest/api/v1')
    ).rejects.toThrow('did not answer a tip');
  });

  it('rejects a 200 whose absolute slot is a string', async () => {
    install({ body: JSON.stringify([{ abs_slot: '131545218' }]) });
    await expect(
      checkAssetMetadataSourceHealth('https://preprod.koios.rest/api/v1')
    ).rejects.toThrow('did not answer a tip');
  });

  it('rejects a 200 carrying a page rather than JSON', async () => {
    install({ body: '<html><body>Sign in</body></html>' });
    await expect(
      checkAssetMetadataSourceHealth('https://preprod.koios.rest/api/v1')
    ).rejects.toThrow('did not answer a tip');
  });

  it('rejects a server error', async () => {
    install({ statusCode: 503, body: '' });
    await expect(
      checkAssetMetadataSourceHealth('https://preprod.koios.rest/api/v1')
    ).rejects.toThrow('answered with 503');
  });

  it('rejects a socket error', async () => {
    install({ requestError: true });
    await expect(
      checkAssetMetadataSourceHealth('https://preprod.koios.rest/api/v1')
    ).rejects.toThrow('connection failed');
  });

  it('rejects an error on the response stream', async () => {
    install({ responseError: true });
    await expect(
      checkAssetMetadataSourceHealth('https://preprod.koios.rest/api/v1')
    ).rejects.toThrow('connection failed');
  });

  it('rejects a body over the cap', async () => {
    install({ chunks: ['x'.repeat(65 * 1024)] });
    await expect(
      checkAssetMetadataSourceHealth('https://preprod.koios.rest/api/v1')
    ).rejects.toThrow('too much');
  });

  it('rejects a URL that cannot be parsed', async () => {
    install({ body: TIP });
    await expect(checkAssetMetadataSourceHealth('not a url')).rejects.toThrow(
      'could not be parsed'
    );
    expect(calls).toHaveLength(0);
  });

  it('rejects when nothing answers within the timeout', async () => {
    install({ silent: true });
    await expect(
      checkAssetMetadataSourceHealth('https://preprod.koios.rest/api/v1', 5)
    ).rejects.toThrow('did not answer in time');
  });
});
