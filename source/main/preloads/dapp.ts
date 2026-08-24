import Ajv from 'ajv';
import { contextBridge, ipcRenderer } from 'electron';
import commonSchema from '../../common/cip30/contracts/schemas/common.schema.json';
import envelopeSchema from '../../common/cip30/contracts/schemas/envelope.schema.json';
import errorsSchema from '../../common/cip30/contracts/schemas/errors.schema.json';
import {
  DAPP_CIP30_GATEWAY_CHANNEL,
  DappCip30Method,
} from '../../common/ipc/dapp';
import type {
  ApiError,
  DaedalusApi,
  DaedalusProvider,
  DappCip30ResultEnvelope,
  Extension,
} from '../../common/ipc/dapp';

const INTERNAL_ERROR: ApiError = {
  code: -2,
  info: 'The wallet connector is unavailable.',
};

const ajv = new Ajv({ schemaId: 'auto' });
ajv.addSchema(commonSchema);
ajv.addSchema(errorsSchema);
ajv.addSchema(envelopeSchema);
const validateEnvelope = ajv.compile({
  $ref: `${envelopeSchema.$id}#/definitions/resultEnvelope`,
});
const validateExtensions = ajv.compile({
  $ref: `${commonSchema.$id}#/definitions/extensions`,
});

const invokeGateway = async <T>(
  method: DappCip30Method,
  args: unknown[]
): Promise<T> => {
  let result: unknown;
  try {
    result = await ipcRenderer.invoke(DAPP_CIP30_GATEWAY_CHANNEL, {
      method,
      args,
    });
  } catch {
    throw { ...INTERNAL_ERROR };
  }

  if (!validateEnvelope(result)) throw { ...INTERNAL_ERROR };
  const envelope = result as DappCip30ResultEnvelope<T>;
  if (envelope.status === 'rejected') throw envelope.rejection.value;
  return envelope.value;
};

const method = <T extends (...args: never[]) => Promise<unknown>>(
  path: DappCip30Method
): T => (((...args: unknown[]) => invokeGateway(path, args)) as unknown) as T;

const getExtensions = async (): Promise<Extension[]> => {
  const extensions = await invokeGateway<unknown>('api.getExtensions', []);
  if (!validateExtensions(extensions)) throw { ...INTERNAL_ERROR };
  return extensions as Extension[];
};

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

contextBridge.exposeInMainWorld('cardano', { daedalus: provider });
