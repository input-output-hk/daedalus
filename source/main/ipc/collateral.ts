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
    Object.getPrototypeOf(value) !== Object.prototype ||
    Object.keys(value).length !== 1
  )
    throw new Error('Invalid collateral request');
  const type = Object.getOwnPropertyDescriptor(value, 'type')?.value;
  if (
    type !== 'snapshot' &&
    type !== 'prepare' &&
    type !== 'cancel-preparation' &&
    type !== 'clear' &&
    type !== 'repair'
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
