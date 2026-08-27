export type Cip30WalletNetwork = Readonly<{
  networkId: 0 | 1;
  networkMagic: number;
  genesisHash: string;
}>;

export type Cip30WalletOperation = 'capabilities' | 'context' | 'addresses';

export type Cip30WalletRequest = Readonly<{
  operation: Cip30WalletOperation;
  walletId: string;
  network: Cip30WalletNetwork;
  sourceRevision: string;
}>;

export type Cip30WalletCapabilities = Readonly<{
  walletId: string;
  walletName: string;
  walletKind: 'shelley-software' | 'ledger' | 'trezor';
  network: Cip30WalletNetwork;
  backendApiVersion: 1;
  backendExtensions: readonly number[];
}>;

export type Cip30WalletAddresses = Readonly<{
  walletId: string;
  network: Cip30WalletNetwork;
  used: readonly string[];
  unused: readonly string[];
  change: string;
  reward: readonly string[];
}>;

export type Cip30WalletResponse =
  | Readonly<{
      status: 'fulfilled';
      operation: 'capabilities';
      value: Cip30WalletCapabilities;
    }>
  | Readonly<{
      status: 'fulfilled';
      operation: 'context';
      value: unknown;
    }>
  | Readonly<{
      status: 'fulfilled';
      operation: 'addresses';
      value: Cip30WalletAddresses;
    }>
  | Readonly<{
      status: 'rejected';
      reason: 'account-change' | 'unavailable' | 'internal';
    }>;

const ownData = (
  value: unknown,
  keys: readonly string[]
): value is Record<string, unknown> => {
  if (!value || typeof value !== 'object' || Array.isArray(value)) return false;
  const prototype = Object.getPrototypeOf(value);
  if (prototype !== Object.prototype && prototype !== null) return false;
  if (Object.getOwnPropertySymbols(value).length) return false;
  const descriptors = Object.getOwnPropertyDescriptors(value);
  return (
    Object.keys(descriptors).length === keys.length &&
    keys.every((key) => {
      const descriptor = descriptors[key];
      return (
        descriptor?.enumerable === true &&
        Object.prototype.hasOwnProperty.call(descriptor, 'value')
      );
    })
  );
};

const plainData = (value: unknown): boolean => {
  if (value === null || typeof value === 'string' || typeof value === 'boolean')
    return true;
  if (typeof value === 'number') return Number.isFinite(value);
  if (Array.isArray(value)) return value.every(plainData);
  if (!value || typeof value !== 'object') return false;
  const keys = Object.keys(value);
  return ownData(value, keys) && keys.every((key) => plainData(value[key]));
};

const text = (value: unknown): value is string =>
  typeof value === 'string' && value.length > 0;
const uint32 = (value: unknown): value is number =>
  Number.isSafeInteger(value) &&
  Number(value) >= 0 &&
  Number(value) <= 0xffffffff;
const lowerHex = /^[0-9a-f]+$/u;
const hex = (value: unknown, bytes: number): value is string =>
  typeof value === 'string' &&
  value.length === bytes * 2 &&
  lowerHex.test(value);

const parseNetwork = (value: unknown): Cip30WalletNetwork => {
  if (!ownData(value, ['networkId', 'networkMagic', 'genesisHash']))
    throw new Error('Invalid CIP-30 wallet network');
  if (
    (value.networkId !== 0 && value.networkId !== 1) ||
    !uint32(value.networkMagic) ||
    !hex(value.genesisHash, 32)
  )
    throw new Error('Invalid CIP-30 wallet network');
  return Object.freeze({
    networkId: value.networkId,
    networkMagic: value.networkMagic,
    genesisHash: value.genesisHash,
  });
};

const sameNetwork = (
  left: Cip30WalletNetwork,
  right: Cip30WalletNetwork
): boolean =>
  left.networkId === right.networkId &&
  left.networkMagic === right.networkMagic &&
  left.genesisHash === right.genesisHash;

export const parseCip30WalletRequest = (value: unknown): Cip30WalletRequest => {
  if (!ownData(value, ['operation', 'walletId', 'network', 'sourceRevision']))
    throw new Error('Invalid CIP-30 wallet request');
  if (
    !['capabilities', 'context', 'addresses'].includes(
      value.operation as string
    ) ||
    !text(value.walletId) ||
    !hex(value.sourceRevision, 20)
  )
    throw new Error('Invalid CIP-30 wallet request');
  return Object.freeze({
    operation: value.operation as Cip30WalletOperation,
    walletId: value.walletId,
    network: parseNetwork(value.network),
    sourceRevision: value.sourceRevision,
  });
};

const parseCapabilities = (
  value: unknown,
  request: Cip30WalletRequest
): Cip30WalletCapabilities => {
  if (
    !ownData(value, [
      'walletId',
      'walletName',
      'walletKind',
      'network',
      'backendApiVersion',
      'backendExtensions',
    ]) ||
    value.walletId !== request.walletId ||
    !text(value.walletName) ||
    !['shelley-software', 'ledger', 'trezor'].includes(
      value.walletKind as string
    ) ||
    value.backendApiVersion !== 1 ||
    !Array.isArray(value.backendExtensions) ||
    new Set(value.backendExtensions).size !== value.backendExtensions.length ||
    !value.backendExtensions.every(
      (cip) => Number.isSafeInteger(cip) && Number(cip) > 0
    )
  )
    throw new Error('Invalid CIP-30 wallet capabilities');
  const network = parseNetwork(value.network);
  if (!sameNetwork(network, request.network))
    throw new Error('Invalid CIP-30 wallet capabilities');
  return Object.freeze({
    walletId: value.walletId,
    walletName: value.walletName,
    walletKind: value.walletKind as Cip30WalletCapabilities['walletKind'],
    network,
    backendApiVersion: 1,
    backendExtensions: Object.freeze([...value.backendExtensions]),
  });
};

const parseAddresses = (
  value: unknown,
  request: Cip30WalletRequest
): Cip30WalletAddresses => {
  if (
    !ownData(value, [
      'walletId',
      'network',
      'used',
      'unused',
      'change',
      'reward',
    ]) ||
    value.walletId !== request.walletId ||
    !Array.isArray(value.used) ||
    !Array.isArray(value.unused) ||
    !Array.isArray(value.reward) ||
    !value.used.every(text) ||
    !value.unused.every(text) ||
    !value.reward.every(text) ||
    !text(value.change)
  )
    throw new Error('Invalid CIP-30 wallet addresses');
  const network = parseNetwork(value.network);
  if (!sameNetwork(network, request.network))
    throw new Error('Invalid CIP-30 wallet addresses');
  return Object.freeze({
    walletId: value.walletId,
    network,
    used: Object.freeze([...value.used]),
    unused: Object.freeze([...value.unused]),
    change: value.change,
    reward: Object.freeze([...value.reward]),
  });
};

export const parseCip30WalletResponse = (
  requestValue: Cip30WalletRequest,
  value: unknown
): Cip30WalletResponse => {
  const request = parseCip30WalletRequest(requestValue);
  if (
    ownData(value, ['status', 'reason']) &&
    value.status === 'rejected' &&
    ['account-change', 'unavailable', 'internal'].includes(
      value.reason as string
    )
  )
    return Object.freeze({
      status: 'rejected',
      reason: value.reason as 'account-change' | 'unavailable' | 'internal',
    });
  if (
    !ownData(value, ['status', 'operation', 'value']) ||
    value.status !== 'fulfilled' ||
    value.operation !== request.operation
  )
    throw new Error('Invalid CIP-30 wallet response');
  if (request.operation === 'capabilities')
    return Object.freeze({
      status: 'fulfilled',
      operation: 'capabilities',
      value: parseCapabilities(value.value, request),
    });
  if (request.operation === 'addresses')
    return Object.freeze({
      status: 'fulfilled',
      operation: 'addresses',
      value: parseAddresses(value.value, request),
    });
  if (!plainData(value.value)) throw new Error('Invalid CIP-30 wallet context');
  return Object.freeze({
    status: 'fulfilled',
    operation: 'context',
    value: value.value,
  });
};
