import type { DappCip30Rejection } from '../../common/cip30/errors';
import { DAPP_CIP30_GATEWAY_CHANNEL } from '../../common/cip30/wire';
import type {
  DaedalusApi,
  DaedalusProvider,
  DappCip30GatewayRequest,
} from '../../common/cip30/wire';

const exposeInMainWorld = jest.fn();
const executeInMainWorld = jest.fn();
const invoke = jest.fn();

jest.mock('electron', () => ({
  contextBridge: { executeInMainWorld, exposeInMainWorld },
  ipcRenderer: { invoke },
}));

require('./dapp');

const exposureCallCount = exposeInMainWorld.mock.calls.length;
const exposureWorld = exposeInMainWorld.mock.calls[0][0];
const exposedCardano = (exposeInMainWorld.mock.calls[0][1] as unknown) as {
  daedalus: DaedalusProvider;
};
const provider = exposedCardano.daedalus;
const transportBlockCallCount = executeInMainWorld.mock.calls.length;
const transportBlock = executeInMainWorld.mock.calls[0][0];

describe('dApp preload', () => {
  it('removes bypass transports from the page world', () => {
    expect(transportBlockCallCount).toBe(1);
    transportBlock.func();
    expect(
      Object.getOwnPropertyDescriptor(globalThis, 'RTCPeerConnection')
    ).toEqual({
      value: undefined,
      configurable: false,
      enumerable: false,
      writable: false,
    });
    expect(Object.getOwnPropertyDescriptor(globalThis, 'WebTransport')).toEqual(
      {
        value: undefined,
        configurable: false,
        enumerable: false,
        writable: false,
      }
    );
  });

  beforeEach(() => invoke.mockReset());

  it('exposes only window.cardano.daedalus during preload evaluation', () => {
    expect(exposureCallCount).toBe(1);
    expect(exposureWorld).toBe('cardano');
    expect(exposedCardano).toEqual({ daedalus: provider });
    expect(Object.keys(provider)).toEqual([
      'apiVersion',
      'name',
      'icon',
      'supportedExtensions',
      'isEnabled',
      'enable',
    ]);
    expect(provider.supportedExtensions).toEqual([{ cip: 95 }, { cip: 103 }]);
  });
  it('rejects malformed calls before IPC access', async () => {
    const isEnabled = provider.isEnabled as (
      ...args: unknown[]
    ) => Promise<boolean>;

    await expect(isEnabled(undefined)).rejects.toEqual({
      code: -1,
      info: 'Invalid request',
    });
    expect(invoke).not.toHaveBeenCalled();
  });

  it('routes every adapter through one channel and adds only negotiated namespaces', async () => {
    invoke.mockImplementation(
      async (_channel: string, request: DappCip30GatewayRequest) => {
        if (request.method === 'api.getExtensions') {
          return {
            status: 'fulfilled',
            value: [{ cip: 95 }, { cip: 103 }],
          };
        }
        return {
          status: 'fulfilled',
          value: request.method === 'provider.enable' ? {} : 0,
        };
      }
    );

    const api = await provider.enable({ extensions: [{ cip: 95 }] });
    expect(Object.prototype.hasOwnProperty.call(api, 'cip95')).toBe(true);
    expect(Object.prototype.hasOwnProperty.call(api, 'cip103')).toBe(true);
    expect(Object.prototype.hasOwnProperty.call(api, 'cip104')).toBe(false);
    expect(Object.prototype.hasOwnProperty.call(api, 'cip142')).toBe(false);

    await api.getNetworkId();
    expect(invoke).toHaveBeenLastCalledWith(DAPP_CIP30_GATEWAY_CHANNEL, {
      method: 'api.getNetworkId',
      args: [],
    });
  });

  it('exposes CIP-142 only from the authoritative negotiated set', async () => {
    invoke.mockImplementation(
      async (_channel: string, request: DappCip30GatewayRequest) =>
        request.method === 'api.getExtensions'
          ? { status: 'fulfilled', value: [{ cip: 142 }] }
          : {
              status: 'fulfilled',
              value: request.method === 'provider.enable' ? {} : 42,
            }
    );
    const api = await provider.enable({ extensions: [{ cip: 142 }] });
    expect(Object.prototype.hasOwnProperty.call(api, 'cip142')).toBe(true);
    await expect(api.cip142!.getNetworkMagic()).resolves.toBe(42);
    expect(invoke).toHaveBeenLastCalledWith(DAPP_CIP30_GATEWAY_CHANNEL, {
      method: 'api.cip142.getNetworkMagic',
      args: [],
    });
  });

  it.each([
    [
      { type: 'api-error', value: { code: -3, info: 'refused' } },
      (api: DaedalusApi) => api.getNetworkId(),
    ],
    [
      { type: 'paginate-error', value: { maxSize: 3 } },
      (api: DaedalusApi) => api.getUsedAddresses(),
    ],
    [
      { type: 'tx-sign-error', value: { code: 2, info: 'declined' } },
      (api: DaedalusApi) => api.signTx('00'),
    ],
    [
      { type: 'data-sign-error', value: { code: 3, info: 'declined' } },
      (api: DaedalusApi) => api.signData('00', ''),
    ],
    [
      { type: 'tx-send-error', value: { code: 1, info: 'refused' } },
      (api: DaedalusApi) => api.submitTx('00'),
    ],
    [
      {
        type: 'cip103-submit-error',
        value: [
          'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa',
          { code: 2, info: 'failed' },
        ],
      },
      (api: DaedalusApi) => api.cip103!.submitTxs(['00']),
    ],
  ] as Array<[DappCip30Rejection, (api: DaedalusApi) => Promise<unknown>]>)(
    'rejects directly with a reconstructed plain $type value',
    async (rejection, call) => {
      invoke
        .mockResolvedValueOnce({ status: 'fulfilled', value: {} })
        .mockResolvedValueOnce({
          status: 'fulfilled',
          value: [{ cip: 103 }],
        })
        .mockResolvedValueOnce({ status: 'rejected', rejection });
      const api = await provider.enable();

      let caught: unknown;
      try {
        await call(api);
      } catch (error) {
        caught = error;
      }

      expect(caught).toEqual(rejection.value);
      expect(caught).not.toBe(rejection.value);
      expect(caught).not.toBeInstanceOf(Error);
    }
  );

  it.each([
    Promise.resolve({ status: 'rejected', rejection: { type: 'api-error' } }),
    Promise.reject(new Error('transport details')),
  ])(
    'maps malformed and transport failures to a plain internal error',
    async (result) => {
      invoke.mockReturnValue(result);

      await expect(provider.isEnabled()).rejects.toEqual({
        code: -2,
        info: 'The wallet connector is unavailable.',
      });
    }
  );
});
