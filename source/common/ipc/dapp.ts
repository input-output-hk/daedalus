import type {
  DappConsentPresentation,
  DappConsentRenderMainRequest,
} from './api';

export const DAPP_CIP30_GATEWAY_CHANNEL = 'dapp-cip30-gateway';

const isRecord = (value: unknown): value is Record<string, unknown> =>
  value !== null && typeof value === 'object' && !Array.isArray(value);
const hasKeys = (value: Record<string, unknown>, keys: readonly string[]) =>
  Object.keys(value).sort().join('\0') === [...keys].sort().join('\0');
const isText = (value: unknown): value is string =>
  typeof value === 'string' && value.length > 0;

const parsePresentation = (value: unknown): DappConsentPresentation => {
  if (
    !isRecord(value) ||
    !hasKeys(value, [
      'requestId',
      'kind',
      'origin',
      'walletName',
      'networkName',
      'scopes',
      'extensions',
    ]) ||
    !isText(value.requestId) ||
    (value.kind !== 'connection' && value.kind !== 'key-disclosure') ||
    !isText(value.origin) ||
    !isText(value.walletName) ||
    !isText(value.networkName) ||
    !Array.isArray(value.scopes) ||
    !value.scopes.every(isText) ||
    !Array.isArray(value.extensions) ||
    !value.extensions.every(
      (extension) => Number.isSafeInteger(extension) && extension > 0
    )
  )
    throw new Error('Invalid dApp consent presentation');
  return Object.freeze({
    requestId: value.requestId,
    kind: value.kind,
    origin: value.origin,
    walletName: value.walletName,
    networkName: value.networkName,
    scopes: Object.freeze([...value.scopes]),
    extensions: Object.freeze([...value.extensions]),
  });
};

export const parseDappConsentRender = (
  value: unknown
): DappConsentRenderMainRequest => {
  if (!isRecord(value)) throw new Error('Invalid dApp consent render request');
  if (
    value.type === 'terminal' &&
    isText(value.requestId) &&
    hasKeys(value, ['type', 'requestId'])
  )
    return Object.freeze({ type: 'terminal', requestId: value.requestId });
  if (value.type === 'present' && hasKeys(value, ['type', 'request']))
    return Object.freeze({
      type: 'present',
      request: parsePresentation(value.request),
    });
  throw new Error('Invalid dApp consent render request');
};

export * from '../cip30/errors';
export * from '../cip30/wire';
