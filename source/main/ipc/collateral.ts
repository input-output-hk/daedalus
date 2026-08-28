import {
  DAPP_COLLATERAL_CHANNEL,
  DappCollateralMainResponse,
  DappCollateralRendererRequest,
} from '../../common/ipc/api';
import { getCollateralService } from '../cip30/Cip30Broker';
import { getCurrentDappRouteLease } from './dappBrowser';
import { MainIpcChannel } from './lib/MainIpcChannel';

const channel = new MainIpcChannel<
  DappCollateralRendererRequest,
  DappCollateralMainResponse
>(DAPP_COLLATERAL_CHANNEL);

export const parseDappCollateralRequest = (
  value: unknown
): DappCollateralRendererRequest => {
  if (
    !value ||
    typeof value !== 'object' ||
    Array.isArray(value) ||
    Object.getPrototypeOf(value) !== Object.prototype
  )
    throw new Error('Invalid collateral request');
  const type = Object.getOwnPropertyDescriptor(value, 'type')?.value;
  if (type === 'track-preparation') {
    const transactionId = Object.getOwnPropertyDescriptor(
      value,
      'transactionId'
    )?.value;
    if (
      Object.keys(value).length !== 2 ||
      typeof transactionId !== 'string' ||
      !/^[0-9a-f]{64}$/u.test(transactionId)
    )
      throw new Error('Invalid collateral request');
    return Object.freeze({ type, transactionId });
  }
  if (
    Object.keys(value).length !== 1 ||
    (type !== 'snapshot' &&
      type !== 'prepare' &&
      type !== 'cancel-preparation' &&
      type !== 'clear' &&
      type !== 'repair')
  )
    throw new Error('Invalid collateral request');
  return Object.freeze({ type });
};

export const handleDappCollateralRequests = (): void => {
  channel.onRequest(async (unknownRequest) => {
    const request = parseDappCollateralRequest(unknownRequest);
    const lease = getCurrentDappRouteLease();
    if (!lease) throw new Error('DApp route required');
    const service = getCollateralService();
    switch (request.type) {
      case 'snapshot':
        return service.snapshot(lease);
      case 'prepare':
        return service.prepare(lease);
      case 'track-preparation':
        return service.trackPreparation(lease, request.transactionId);
      case 'cancel-preparation':
        return service.cancelPreparation(lease);
      case 'clear':
        return service.clear(lease);
      case 'repair':
        return service.repair(lease);
      default:
        throw new Error('Invalid collateral request');
    }
  });
};
