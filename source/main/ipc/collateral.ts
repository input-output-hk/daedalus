import {
  DAPP_COLLATERAL_CHANNEL,
  DappCollateralMainResponse,
  DappCollateralRendererRequest,
  WALLET_INPUT_SELECTION_CHANNEL,
  WalletInputSelectionMainResponse,
  WalletInputSelectionRendererRequest,
} from '../../common/ipc/api';
import type { CollateralPreparationOperation } from '../../common/types/collateral.types';
import { getCollateralService } from '../cip30/Cip30Broker';
import { dappRuntimeConfig } from '../config';
import { getCurrentDappRouteLease } from './dappBrowser';
import { MainIpcChannel } from './lib/MainIpcChannel';
import { onTrustedRendererInvalidated } from './lib/trustedRendererIpcAuthority';

const channel = new MainIpcChannel<
  DappCollateralRendererRequest,
  DappCollateralMainResponse
>(DAPP_COLLATERAL_CHANNEL);

const inputSelectionChannel = new MainIpcChannel<
  WalletInputSelectionRendererRequest,
  WalletInputSelectionMainResponse
>(WALLET_INPUT_SELECTION_CHANNEL);

const ownData = (
  value: unknown,
  keys: readonly string[]
): Record<string, unknown> => {
  if (
    !value ||
    typeof value !== 'object' ||
    Array.isArray(value) ||
    Object.getPrototypeOf(value) !== Object.prototype ||
    Reflect.ownKeys(value).length !== keys.length ||
    !keys.every((key) => {
      const descriptor = Object.getOwnPropertyDescriptor(value, key);
      return descriptor !== undefined && 'value' in descriptor;
    })
  )
    throw new Error('Invalid collateral request');
  return value as Record<string, unknown>;
};

const parsePreparation = (value: unknown): CollateralPreparationOperation => {
  const fields = ownData(value, ['operationId', 'walletId', 'networkGenesis']);
  if (
    typeof fields.operationId !== 'string' ||
    !/^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/u.test(
      fields.operationId
    ) ||
    typeof fields.walletId !== 'string' ||
    !/^[0-9a-f]{40}$/u.test(fields.walletId) ||
    typeof fields.networkGenesis !== 'string' ||
    !/^[0-9a-f]{64}$/u.test(fields.networkGenesis)
  )
    throw new Error('Invalid collateral request');
  return Object.freeze({
    operationId: fields.operationId,
    walletId: fields.walletId,
    networkGenesis: fields.networkGenesis,
  });
};

export const parseDappCollateralRequest = (
  value: unknown
): DappCollateralRendererRequest => {
  if (!value || typeof value !== 'object')
    throw new Error('Invalid collateral request');
  const type = Object.getOwnPropertyDescriptor(value, 'type')?.value;
  if (type === 'track-preparation') {
    const fields = ownData(value, ['type', 'preparation', 'transactionId']);
    if (
      typeof fields.transactionId !== 'string' ||
      !/^[0-9a-f]{64}$/u.test(fields.transactionId)
    )
      throw new Error('Invalid collateral request');
    return Object.freeze({
      type,
      preparation: parsePreparation(fields.preparation),
      transactionId: fields.transactionId,
    });
  }
  if (type === 'cancel-preparation') {
    const fields = ownData(value, ['type', 'preparation']);
    return Object.freeze({
      type,
      preparation: parsePreparation(fields.preparation),
    });
  }
  ownData(value, ['type']);
  if (
    type !== 'snapshot' &&
    type !== 'prepare' &&
    type !== 'clear' &&
    type !== 'repair'
  )
    throw new Error('Invalid collateral request');
  return Object.freeze({ type });
};

export const handleDappCollateralRequests = (): void => {
  inputSelectionChannel.onRequest(async (request) => {
    if (!dappRuntimeConfig.cip30Enabled)
      throw new Error('CIP-30 is unavailable');
    if (
      !request ||
      Object.getPrototypeOf(request) !== Object.prototype ||
      Object.keys(request).length !== 1
    )
      throw new Error('Invalid wallet input selection request');
    const walletId = Object.getOwnPropertyDescriptor(
      request,
      'walletId'
    )?.value;
    if (typeof walletId !== 'string' || !/^[0-9a-f]{40}$/u.test(walletId))
      throw new Error('Invalid wallet input selection request');
    return {
      preferred_collateral: getCollateralService()
        .preferredInputs(walletId)
        .map(({ transactionId, index }) => ({ id: transactionId, index })),
    };
  });
  channel.onRequest(async (unknownRequest) => {
    if (!dappRuntimeConfig.cip30Enabled)
      throw new Error('CIP-30 is unavailable');
    const request = parseDappCollateralRequest(unknownRequest);
    const service = getCollateralService();
    if (request.type === 'track-preparation')
      return service.trackPreparation(
        request.preparation,
        request.transactionId
      );
    if (request.type === 'cancel-preparation')
      return service.cancelPreparation(request.preparation);
    const lease = getCurrentDappRouteLease();
    if (!lease) throw new Error('DApp route required');
    switch (request.type) {
      case 'snapshot':
        return service.snapshot(lease);
      case 'prepare':
        return service.prepare(lease, onTrustedRendererInvalidated);
      case 'clear':
        return service.clear(lease);
      case 'repair':
        return service.repair(lease);
      default:
        throw new Error('Invalid collateral request');
    }
  });
};
