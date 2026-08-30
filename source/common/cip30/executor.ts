import type { Cip8BackendResponse } from '../cardano/cip8';

export type Cip30WalletNetwork = Readonly<{
  networkId: 0 | 1;
  networkMagic: number;
  genesisHash: string;
}>;

export type Cip30WalletOperation =
  | 'capabilities'
  | 'context'
  | 'transaction-context'
  | 'collateral-history'
  | 'addresses'
  | 'cip95-key-state'
  | 'sign-transactions'
  | 'submit-transaction'
  | 'sign-data';
type Cip30WalletRequestIdentity = Readonly<{
  walletId: string;
  network: Cip30WalletNetwork;
  sourceRevision: string;
}>;
export type Cip30WalletRequest = Cip30WalletRequestIdentity &
  (
    | Readonly<{
        operation: 'capabilities' | 'context' | 'addresses' | 'cip95-key-state';
      }>
    | Readonly<{
        operation: 'collateral-history';
        preferredInputs: readonly Cip30WalletOutpoint[];
      }>
    | Readonly<{
        operation: 'transaction-context';
        transactions: readonly string[];
      }>
    | Readonly<{
        operation: 'sign-transactions';
        context: unknown;
        transactions: readonly Readonly<{
          cbor: string;
          partialSign: boolean;
        }>[];
        passphrase?: string;
      }>
    | Readonly<{
        operation: 'submit-transaction';
        transaction: string;
      }>
    | Readonly<{
        operation: 'sign-data';
        address: string;
        payload: string;
        passphrase: string;
      }>
  );

export type Cip30WalletOutpoint = Readonly<{
  transactionId: string;
  index: number;
}>;

export type Cip30WalletCollateralHistory = Readonly<{
  transactions: readonly Readonly<{
    transactionId: string;
    status: 'pending' | 'in_ledger' | 'expired';
    scriptValidity: 'valid' | 'invalid' | null;
    normalInputs: readonly Cip30WalletOutpoint[];
    collateralInputs: readonly Cip30WalletOutpoint[];
  }>[];
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

export type Cip30WalletCip95KeyState = Readonly<{
  drep_public_key: string;
  registered_stake_public_keys: readonly string[];
  unregistered_stake_public_keys: readonly string[];
}>;

export type Cip30WalletWitnessResponse = Readonly<{
  revision: 1;
  witnesses: readonly Readonly<{
    transaction_index: number;
    body_hash: string;
    witness_set_cbor: string;
  }>[];
}>;
export type Cip30WalletSubmissionResponse = Readonly<{
  revision: 1;
  transaction_id: string;
  status:
    | 'authorized'
    | 'broadcasting'
    | 'submitted'
    | 'rejected'
    | 'outcome_unknown'
    | 'in_ledger'
    | 'expired';
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
      operation: 'collateral-history';
      value: Cip30WalletCollateralHistory;
    }>
  | Readonly<{
      status: 'fulfilled';
      operation: 'addresses';
      value: Cip30WalletAddresses;
    }>
  | Readonly<{
      status: 'fulfilled';
      operation: 'cip95-key-state';
      value: Cip30WalletCip95KeyState;
    }>
  | Readonly<{
      status: 'fulfilled';
      operation: 'transaction-context';
      value: unknown;
    }>
  | Readonly<{
      status: 'fulfilled';
      operation: 'sign-transactions';
      value: Cip30WalletWitnessResponse;
    }>
  | Readonly<{
      status: 'fulfilled';
      operation: 'submit-transaction';
      value: Cip30WalletSubmissionResponse;
    }>
  | Readonly<{
      status: 'fulfilled';
      operation: 'sign-data';
      value: Cip8BackendResponse;
    }>
  | Readonly<{
      status: 'rejected';
      reason:
        | 'account-change'
        | 'unavailable'
        | 'internal'
        | 'address-not-pk'
        | 'proof-generation'
        | 'tx-proof-generation'
        | 'deprecated-certificate'
        | 'tx-send-failure';
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

const parseOutpoint = (value: unknown): Cip30WalletOutpoint => {
  if (
    !ownData(value, ['transactionId', 'index']) ||
    !hex(value.transactionId, 32) ||
    !uint32(value.index)
  )
    throw new Error('Invalid CIP-30 wallet outpoint');
  return Object.freeze({
    transactionId: value.transactionId,
    index: value.index,
  });
};

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

const transactionCbor = (value: unknown): value is string =>
  typeof value === 'string' &&
  value.length > 0 &&
  value.length % 2 === 0 &&
  value.length <= 65_536 * 2 &&
  lowerHex.test(value);

export const parseCip30WalletRequest = (value: unknown): Cip30WalletRequest => {
  if (!value || typeof value !== 'object' || Array.isArray(value))
    throw new Error('Invalid CIP-30 wallet request');
  const descriptor = Object.getOwnPropertyDescriptor(value, 'operation');
  const operation = descriptor?.value as Cip30WalletOperation | undefined;
  let keys = ['operation', 'walletId', 'network', 'sourceRevision'];
  if (operation === 'sign-data')
    keys = [...keys, 'address', 'payload', 'passphrase'];
  else if (operation === 'transaction-context')
    keys = [...keys, 'transactions'];
  else if (operation === 'collateral-history')
    keys = [...keys, 'preferredInputs'];
  else if (operation === 'sign-transactions') {
    keys = [...keys, 'context', 'transactions'];
    if (Object.prototype.hasOwnProperty.call(value, 'passphrase'))
      keys = [...keys, 'passphrase'];
  } else if (operation === 'submit-transaction')
    keys = [...keys, 'transaction'];
  if (
    !ownData(value, keys) ||
    ![
      'capabilities',
      'context',
      'transaction-context',
      'collateral-history',
      'addresses',
      'cip95-key-state',
      'sign-transactions',
      'submit-transaction',
      'sign-data',
    ].includes(operation || '') ||
    !text(value.walletId) ||
    !hex(value.sourceRevision, 20)
  )
    throw new Error('Invalid CIP-30 wallet request');
  const identity = {
    operation: operation as Cip30WalletOperation,
    walletId: value.walletId,
    network: parseNetwork(value.network),
    sourceRevision: value.sourceRevision,
  };
  if (operation === 'collateral-history') {
    if (
      !Array.isArray(value.preferredInputs) ||
      value.preferredInputs.length < 1 ||
      value.preferredInputs.length > 3
    )
      throw new Error('Invalid CIP-30 wallet request');
    return Object.freeze({
      ...identity,
      operation,
      preferredInputs: Object.freeze(value.preferredInputs.map(parseOutpoint)),
    });
  }
  if (operation === 'transaction-context') {
    if (
      !Array.isArray(value.transactions) ||
      value.transactions.length < 1 ||
      value.transactions.length > 50 ||
      !value.transactions.every(transactionCbor)
    )
      throw new Error('Invalid CIP-30 wallet request');
    return Object.freeze({
      ...identity,
      operation,
      transactions: Object.freeze([...value.transactions]),
    });
  }
  if (operation === 'sign-transactions') {
    const hasPassphrase = Object.prototype.hasOwnProperty.call(
      value,
      'passphrase'
    );
    const passphrase =
      hasPassphrase && text(value.passphrase) ? value.passphrase : undefined;
    if (
      !plainData(value.context) ||
      !Array.isArray(value.transactions) ||
      value.transactions.length < 1 ||
      value.transactions.length > 50 ||
      !value.transactions.every(
        (transaction) =>
          ownData(transaction, ['cbor', 'partialSign']) &&
          transactionCbor(transaction.cbor) &&
          typeof transaction.partialSign === 'boolean'
      ) ||
      (hasPassphrase && !passphrase)
    )
      throw new Error('Invalid CIP-30 wallet request');
    return Object.freeze({
      ...identity,
      operation,
      context: JSON.parse(JSON.stringify(value.context)),
      transactions: Object.freeze(
        value.transactions.map((transaction) =>
          Object.freeze({
            cbor: (transaction as { cbor: string }).cbor,
            partialSign: (transaction as { partialSign: boolean }).partialSign,
          })
        )
      ),
      ...(passphrase ? { passphrase } : {}),
    });
  }
  if (operation === 'submit-transaction') {
    if (!transactionCbor(value.transaction))
      throw new Error('Invalid CIP-30 wallet request');
    return Object.freeze({
      ...identity,
      operation,
      transaction: value.transaction,
    });
  }
  if (operation === 'sign-data') {
    if (
      !text(value.address) ||
      !lowerHex.test(value.address) ||
      typeof value.payload !== 'string' ||
      (value.payload.length > 0 && !lowerHex.test(value.payload)) ||
      value.payload.length % 2 !== 0 ||
      !text(value.passphrase)
    )
      throw new Error('Invalid CIP-30 wallet request');
    return Object.freeze({
      ...identity,
      operation,
      address: value.address,
      payload: value.payload,
      passphrase: value.passphrase,
    });
  }
  return Object.freeze(identity) as Cip30WalletRequest;
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

const parseCip95KeyState = (value: unknown): Cip30WalletCip95KeyState => {
  if (
    !ownData(value, [
      'drep_public_key',
      'registered_stake_public_keys',
      'unregistered_stake_public_keys',
    ]) ||
    !hex(value.drep_public_key, 32) ||
    !Array.isArray(value.registered_stake_public_keys) ||
    !value.registered_stake_public_keys.every((key) => hex(key, 32)) ||
    !Array.isArray(value.unregistered_stake_public_keys) ||
    !value.unregistered_stake_public_keys.every((key) => hex(key, 32))
  )
    throw new Error('Invalid CIP-95 key state');
  return Object.freeze({
    drep_public_key: value.drep_public_key,
    registered_stake_public_keys: Object.freeze([
      ...value.registered_stake_public_keys,
    ]),
    unregistered_stake_public_keys: Object.freeze([
      ...value.unregistered_stake_public_keys,
    ]),
  });
};

const parseDataSignature = (value: unknown): Cip8BackendResponse => {
  if (
    !ownData(value, [
      'revision',
      'credential_kind',
      'credential',
      'cose_sign1',
      'cose_key',
    ]) ||
    value.revision !== 1 ||
    !['payment', 'stake', 'drep'].includes(value.credential_kind as string) ||
    typeof value.credential !== 'string' ||
    !/^[0-9a-f]{56}$/u.test(value.credential) ||
    !text(value.cose_sign1) ||
    !lowerHex.test(value.cose_sign1) ||
    value.cose_sign1.length % 2 !== 0 ||
    !text(value.cose_key) ||
    !lowerHex.test(value.cose_key) ||
    value.cose_key.length % 2 !== 0
  )
    throw new Error('Invalid CIP-30 data signature');
  return Object.freeze(value as Cip8BackendResponse);
};

const parseWitnessResponse = (
  value: unknown,
  expectedCount: number
): Cip30WalletWitnessResponse => {
  if (
    !ownData(value, ['revision', 'witnesses']) ||
    value.revision !== 1 ||
    !Array.isArray(value.witnesses) ||
    value.witnesses.length !== expectedCount ||
    !value.witnesses.every(
      (witness, index) =>
        ownData(witness, [
          'transaction_index',
          'body_hash',
          'witness_set_cbor',
        ]) &&
        witness.transaction_index === index &&
        hex(witness.body_hash, 32) &&
        transactionCbor(witness.witness_set_cbor)
    )
  )
    throw new Error('Invalid CIP-30 transaction witnesses');
  return Object.freeze({
    revision: 1,
    witnesses: Object.freeze(
      value.witnesses.map((witness) =>
        Object.freeze({
          transaction_index: (witness as { transaction_index: number })
            .transaction_index,
          body_hash: (witness as { body_hash: string }).body_hash,
          witness_set_cbor: (witness as { witness_set_cbor: string })
            .witness_set_cbor,
        })
      )
    ),
  });
};

const parseCollateralHistory = (
  value: unknown
): Cip30WalletCollateralHistory => {
  if (!ownData(value, ['transactions']) || !Array.isArray(value.transactions))
    throw new Error('Invalid CIP-30 collateral history');
  const transactions = value.transactions.map((candidate) => {
    if (
      !ownData(candidate, [
        'transactionId',
        'status',
        'scriptValidity',
        'normalInputs',
        'collateralInputs',
      ]) ||
      !hex(candidate.transactionId, 32) ||
      !Array.isArray(candidate.normalInputs) ||
      !Array.isArray(candidate.collateralInputs)
    )
      throw new Error('Invalid CIP-30 collateral history');
    let status: 'pending' | 'in_ledger' | 'expired';
    if (
      candidate.status === 'pending' ||
      candidate.status === 'in_ledger' ||
      candidate.status === 'expired'
    )
      status = candidate.status;
    else throw new Error('Invalid CIP-30 collateral history');
    let scriptValidity: 'valid' | 'invalid' | null;
    if (candidate.scriptValidity === null) scriptValidity = null;
    else if (candidate.scriptValidity === 'valid') scriptValidity = 'valid';
    else if (candidate.scriptValidity === 'invalid') scriptValidity = 'invalid';
    else throw new Error('Invalid CIP-30 collateral history');
    return Object.freeze({
      transactionId: candidate.transactionId,
      status,
      scriptValidity,
      normalInputs: Object.freeze(candidate.normalInputs.map(parseOutpoint)),
      collateralInputs: Object.freeze(
        candidate.collateralInputs.map(parseOutpoint)
      ),
    });
  });
  return Object.freeze({ transactions: Object.freeze(transactions) });
};

const parseSubmissionResponse = (
  value: unknown
): Cip30WalletSubmissionResponse => {
  if (
    !ownData(value, ['revision', 'transaction_id', 'status']) ||
    value.revision !== 1 ||
    !hex(value.transaction_id, 32) ||
    ![
      'authorized',
      'broadcasting',
      'submitted',
      'rejected',
      'outcome_unknown',
      'in_ledger',
      'expired',
    ].includes(value.status as string)
  )
    throw new Error('Invalid CIP-30 transaction submission');
  return Object.freeze(value as Cip30WalletSubmissionResponse);
};

export const parseCip30WalletResponse = (
  requestValue: Cip30WalletRequest,
  value: unknown
): Cip30WalletResponse => {
  const request = parseCip30WalletRequest(requestValue);
  if (
    ownData(value, ['status', 'reason']) &&
    value.status === 'rejected' &&
    [
      'account-change',
      'unavailable',
      'internal',
      'address-not-pk',
      'proof-generation',
      'tx-proof-generation',
      'deprecated-certificate',
      'tx-send-failure',
    ].includes(value.reason as string)
  )
    return Object.freeze({
      status: 'rejected',
      reason: value.reason as
        | 'account-change'
        | 'unavailable'
        | 'internal'
        | 'address-not-pk'
        | 'proof-generation'
        | 'tx-proof-generation'
        | 'deprecated-certificate'
        | 'tx-send-failure',
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
  if (request.operation === 'cip95-key-state')
    return Object.freeze({
      status: 'fulfilled',
      operation: 'cip95-key-state',
      value: parseCip95KeyState(value.value),
    });
  if (request.operation === 'collateral-history')
    return Object.freeze({
      status: 'fulfilled',
      operation: 'collateral-history',
      value: parseCollateralHistory(value.value),
    });
  if (request.operation === 'sign-transactions')
    return Object.freeze({
      status: 'fulfilled',
      operation: 'sign-transactions',
      value: parseWitnessResponse(value.value, request.transactions.length),
    });
  if (request.operation === 'submit-transaction')
    return Object.freeze({
      status: 'fulfilled',
      operation: 'submit-transaction',
      value: parseSubmissionResponse(value.value),
    });
  if (request.operation === 'sign-data')
    return Object.freeze({
      status: 'fulfilled',
      operation: 'sign-data',
      value: parseDataSignature(value.value),
    });
  if (!plainData(value.value)) throw new Error('Invalid CIP-30 wallet context');
  if (
    request.operation !== 'context' &&
    request.operation !== 'transaction-context'
  )
    throw new Error('Invalid CIP-30 wallet response');
  return Object.freeze({
    status: 'fulfilled',
    operation: request.operation,
    value: value.value,
  });
};
