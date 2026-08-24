import type {
  DaedalusProvider,
  DappCip30GatewayRequest,
  DappCip30Rejection,
} from '../../common/ipc/dapp';
import { DAPP_CIP30_GATEWAY_CHANNEL } from '../../common/ipc/dapp';

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
    expect(provider.supportedExtensions).toEqual([]);
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
        return { status: 'fulfilled', value: 0 };
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

  it.each<DappCip30Rejection>([
    { type: 'api-error', value: { code: -3, info: 'refused' } },
    { type: 'paginate-error', value: { maxSize: 3 } },
    { type: 'tx-sign-error', value: { code: 2, info: 'declined' } },
    { type: 'data-sign-error', value: { code: 3, info: 'declined' } },
    { type: 'tx-send-error', value: { code: 1, info: 'refused' } },
    {
      type: 'cip103-submit-error',
      value: [
        'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa',
        { code: 2, info: 'failed' },
      ],
    },
  ])('rejects directly with the typed $type value', async (rejection) => {
    invoke.mockResolvedValue({ status: 'rejected', rejection });

    let caught: unknown;
    try {
      await provider.isEnabled();
    } catch (error) {
      caught = error;
    }

    expect(caught).toBe(rejection.value);
    expect(caught).not.toBeInstanceOf(Error);
  });

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
