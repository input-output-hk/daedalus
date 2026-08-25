import { contextBridge, ipcRenderer } from 'electron';
import { reconstructPublicRejection } from '../../common/cip30/errors';
import {
  parseDappCip30GatewayRequest,
  parseDappCip30ResultEnvelope,
} from '../../common/cip30/schemas';
import type {
  ApiError,
  DaedalusApi,
  DaedalusProvider,
  DappCip30Method,
  Extension,
} from '../../common/ipc/dapp';
import { DAPP_CIP30_GATEWAY_CHANNEL } from '../../common/ipc/dapp';

const INTERNAL_ERROR: ApiError = {
  code: -2,
  info: 'The wallet connector is unavailable.',
};

const invokeGateway = async <T>(
  method: DappCip30Method,
  args: unknown[]
): Promise<T> => {
  const request = parseDappCip30GatewayRequest({ method, args });
  let result: unknown;
  try {
    result = await ipcRenderer.invoke(DAPP_CIP30_GATEWAY_CHANNEL, request);
  } catch {
    throw { ...INTERNAL_ERROR };
  }

  try {
    const envelope = parseDappCip30ResultEnvelope(method, result);
    if (envelope.status === 'rejected') {
      throw reconstructPublicRejection(envelope.rejection);
    }
    return envelope.value as T;
  } catch (error) {
    if (!(error instanceof Error)) throw error;
    throw { ...INTERNAL_ERROR };
  }
};

const method = <T extends (...args: never[]) => Promise<unknown>>(
  path: DappCip30Method
): T => (((...args: unknown[]) => invokeGateway(path, args)) as unknown) as T;

const getExtensions = (): Promise<Extension[]> =>
  invokeGateway('api.getExtensions', []);

const createApi = (extensions: Extension[]): DaedalusApi => {
  const enabled = new Set(extensions.map(({ cip }) => cip));
  const api: DaedalusApi = {
    getExtensions,
    getNetworkId: method('api.getNetworkId'),
    getUtxos: method('api.getUtxos'),
    getCollateral: method('api.getCollateral'),
    getBalance: method('api.getBalance'),
    getUsedAddresses: method('api.getUsedAddresses'),
    getUnusedAddresses: method('api.getUnusedAddresses'),
    getChangeAddress: method('api.getChangeAddress'),
    getRewardAddresses: method('api.getRewardAddresses'),
    signTx: method('api.signTx'),
    signData: method('api.signData'),
    submitTx: method('api.submitTx'),
  };

  if (enabled.has(95)) {
    api.cip95 = {
      getPubDRepKey: method('api.cip95.getPubDRepKey'),
      getRegisteredPubStakeKeys: method('api.cip95.getRegisteredPubStakeKeys'),
      getUnregisteredPubStakeKeys: method(
        'api.cip95.getUnregisteredPubStakeKeys'
      ),
      signData: method('api.cip95.signData'),
    };
  }
  if (enabled.has(103)) {
    api.cip103 = {
      signTxs: method('api.cip103.signTxs'),
      submitTxs: method('api.cip103.submitTxs'),
    };
  }
  if (enabled.has(104)) {
    api.cip104 = { getAccountPub: method('api.cip104.getAccountPub') };
  }
  if (enabled.has(142)) {
    api.cip142 = { getNetworkMagic: method('api.cip142.getNetworkMagic') };
  }

  return api;
};

const provider: DaedalusProvider = {
  apiVersion: '1',
  name: 'Daedalus',
  icon:
    'data:image/svg+xml,<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 1 1"/>',
  supportedExtensions: [],
  isEnabled: method('provider.isEnabled'),
  enable: (async (...args: unknown[]) => {
    await invokeGateway('provider.enable', args);
    return createApi(await getExtensions());
  }) as DaedalusProvider['enable'],
};

contextBridge.executeInMainWorld({
  func: () => {
    [
      'RTCPeerConnection',
      'webkitRTCPeerConnection',
      'RTCDataChannel',
      'WebTransport',
      'TCPSocket',
      'UDPSocket',
    ].forEach((name) =>
      Object.defineProperty(globalThis, name, {
        value: undefined,
        configurable: false,
        writable: false,
      })
    );
  },
});

contextBridge.exposeInMainWorld('cardano', { daedalus: provider });
