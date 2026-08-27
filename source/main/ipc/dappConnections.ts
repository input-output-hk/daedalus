import {
  DAPP_CONNECTIONS_CHANNEL,
  DappConnectionIdentity,
  DappConnectionsMainResponse,
  DappConnectionsRendererRequest,
} from '../../common/ipc/api';
import { getDappConnectionService } from '../cip30/Cip30Broker';
import { MainIpcChannel } from './lib/MainIpcChannel';

const channel = new MainIpcChannel<
  DappConnectionsRendererRequest,
  DappConnectionsMainResponse
>(DAPP_CONNECTIONS_CHANNEL);
const ownKeysAre = (value: Record<string, unknown>, keys: readonly string[]) =>
  Object.keys(value).sort().join('\0') === [...keys].sort().join('\0');
const object = (value: unknown): value is Record<string, unknown> =>
  !!value && typeof value === 'object' && !Array.isArray(value);
const text = (value: unknown): value is string =>
  typeof value === 'string' && value.length > 0 && value.length <= 2048;

const identity = (value: unknown): value is DappConnectionIdentity => {
  if (
    !object(value) ||
    !ownKeysAre(value, ['origin', 'walletId', 'networkGenesis', 'launch']) ||
    !text(value.origin) ||
    !text(value.walletId) ||
    !text(value.networkGenesis) ||
    !object(value.launch)
  )
    return false;
  return value.launch.kind === 'diagnostics'
    ? ownKeysAre(value.launch, ['kind'])
    : value.launch.kind === 'catalog' &&
        ownKeysAre(value.launch, [
          'kind',
          'catalogEntryId',
          'catalogEntryIdentity',
        ]) &&
        text(value.launch.catalogEntryId) &&
        text(value.launch.catalogEntryIdentity);
};

export const parseDappConnectionsRequest = (
  value: unknown
): DappConnectionsRendererRequest => {
  if (!object(value) || !text(value.type)) throw new Error('Invalid request');
  if (
    (value.type === 'list' || value.type === 'repair') &&
    ownKeysAre(value, ['type'])
  )
    return value as DappConnectionsRendererRequest;
  if (
    (value.type === 'disconnect' || value.type === 'forget') &&
    ownKeysAre(value, ['type', 'identity']) &&
    identity(value.identity)
  )
    return value as DappConnectionsRendererRequest;
  if (
    value.type === 'revoke-scope' &&
    ownKeysAre(value, ['type', 'identity', 'scope']) &&
    identity(value.identity) &&
    (value.scope === 'governance-key-disclosure' ||
      value.scope === 'account-public-key-disclosure')
  )
    return value as DappConnectionsRendererRequest;
  if (
    value.type === 'remove-wallet' &&
    ownKeysAre(value, ['type', 'walletId']) &&
    text(value.walletId)
  )
    return value as DappConnectionsRendererRequest;
  if (
    value.type === 'prune-wallets' &&
    ownKeysAre(value, ['type', 'walletIds']) &&
    Array.isArray(value.walletIds) &&
    value.walletIds.length <= 10000 &&
    value.walletIds.every(text) &&
    new Set(value.walletIds).size === value.walletIds.length
  )
    return value as DappConnectionsRendererRequest;
  throw new Error('Invalid request');
};

export const handleDappConnectionRequests = (): void => {
  channel.onRequest(async (unknownRequest) => {
    const request = parseDappConnectionsRequest(unknownRequest);
    const service = getDappConnectionService();
    switch (request.type) {
      case 'list':
        return service.snapshot();
      case 'disconnect':
        return service.disconnect(request.identity);
      case 'forget':
        return service.forget(request.identity);
      case 'revoke-scope':
        return service.revokeScope(request.identity, request.scope);
      case 'remove-wallet':
        return service.removeWallet(request.walletId);
      case 'prune-wallets':
        return service.pruneWallets(request.walletIds);
      case 'repair':
        return service.repair();
      default:
        throw new Error('Invalid request');
    }
  });
};
