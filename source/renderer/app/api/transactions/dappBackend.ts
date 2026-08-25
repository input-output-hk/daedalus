import type { RequestConfig } from '../common/types';
import { request } from '../utils/request';

export type DappNetwork = {
  network_id: 0 | 1;
  network_magic: number;
  genesis_hash: string;
};

export type DappCapabilities = {
  api_version: 1;
  backend_build: { version: string; source_revision: string };
  network: DappNetwork & { current_era: 'conway' };
  capabilities: Array<{
    name:
      | 'transaction-context'
      | 'reviewed-context-signing'
      | 'cip8-cip95'
      | 'wallet-scoped-submission';
    revision: 1;
    available_eras: ['conway'];
  }>;
};

export type DappTransactionContextRequest = {
  revision: 1;
  network: DappNetwork;
  transactions: string[];
};

export type DappTransactionContext = {
  revision: 1;
  wallet_id: string;
  network: DappNetwork;
  chain_point:
    | { kind: 'genesis' }
    | { kind: 'block'; slot: string; block_hash: string };
  wallet_generation: string;
  pending_generation: string;
  era: 'conway';
  protocol_version: { major: number; minor: number };
  protocol_parameters_cbor: string;
  volatile_delta: {
    point: DappTransactionContext['chain_point'];
    node_transaction_inputs: string[];
  };
  outputs: Array<{
    outpoint: { transaction_id: string; index: number };
    transaction_input_cbor: string;
    source_transaction_output_cbor: string;
    canonical_transaction_output_cbor: string;
    transaction_unspent_output_cbor: string;
    provenance: Array<'earlier' | 'pending' | 'node'>;
    roles: Array<'normal' | 'collateral' | 'reference' | 'wallet_snapshot'>;
    wallet_member: boolean;
    pending_state: 'none' | 'outcome_unknown';
  }>;
  pending_overlay: {
    transactions: Array<{
      transaction_id: string;
      state: 'outcome_unknown';
      transaction_cbor: string;
      normal_inputs: Array<{ transaction_id: string; index: number }>;
      collateral_inputs: Array<{ transaction_id: string; index: number }>;
      expiry_slot: string | null;
    }>;
    spent_wallet_inputs: Array<{ transaction_id: string; index: number }>;
    produced_wallet_outputs: [];
  };
  ownership: Array<{
    credential_kind: 'payment' | 'stake' | 'drep' | 'policy';
    credential: string;
    ownership: 'unowned' | 'owned_key' | 'script';
    derivation_path: number[];
    proof_kinds: Array<
      | 'normal_input'
      | 'collateral'
      | 'withdrawal'
      | 'certificate'
      | 'required_signer'
      | 'native_script'
      | 'policy'
    >;
  }>;
  required_wallet_proofs: Array<{
    transaction_index: number;
    proof_kind:
      | 'normal_input'
      | 'collateral'
      | 'withdrawal'
      | 'certificate'
      | 'required_signer'
      | 'native_script'
      | 'policy';
    credential_kind: 'payment' | 'stake' | 'drep' | 'policy';
    credential: string;
    required: boolean;
  }>;
  batch_overlay: {
    dependencies: Array<{
      transaction_index: number;
      input_role: 'normal' | 'collateral' | 'reference';
      outpoint: { transaction_id: string; index: number };
      source: 'earlier' | 'pending';
      source_transaction_index: number | null;
    }>;
    conflicts: Array<{
      transaction_index: number;
      input_role: 'normal' | 'collateral';
      outpoint: { transaction_id: string; index: number };
      earlier_transaction_index: number;
    }>;
  };
  records: string[];
  context_digest: string;
  context_token: string;
};

export type DappWitnessRequest = {
  revision: 1;
  context: DappTransactionContext;
  transactions: Array<{ cbor: string; partial_sign: boolean }>;
  passphrase: string;
};

export type DappWitnessResponse = {
  revision: 1;
  witnesses: Array<{
    transaction_index: number;
    body_hash: string;
    witness_set_cbor: string;
  }>;
};

export type DappDataSignatureRequest = {
  revision: 1;
  network: DappNetwork;
  address: string;
  payload: string;
  passphrase: string;
};

export type DappDataSignatureResponse = {
  revision: 1;
  credential_kind: 'payment' | 'stake' | 'drep';
  credential: string;
  cose_sign1: string;
  cose_key: string;
};

export type DappCip95KeyState = {
  drep_public_key: string;
  registered_stake_public_keys: string[];
  unregistered_stake_public_keys: string[];
};

export type DappSubmissionRequest = {
  revision: 1;
  network: DappNetwork;
  transaction: string;
};

export type DappSubmissionResponse = {
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
};

const HEX = /^(?:[0-9a-f]{2})+$/;
const HEX_28 = /^[0-9a-f]{56}$/;
const HEX_32 = /^[0-9a-f]{64}$/;
const HEX_40 = /^[0-9a-f]{40}$/;
const WORD64 = /^(?:0|[1-9][0-9]{0,18}|1[0-7][0-9]{18}|18[0-3][0-9]{17}|184[0-3][0-9]{16}|1844[0-5][0-9]{15}|18446[0-6][0-9]{14}|184467[0-3][0-9]{13}|1844674[0-3][0-9]{12}|184467440[0-6][0-9]{10}|1844674407[0-2][0-9]{9}|18446744073[0-6][0-9]{8}|1844674407370[0-8][0-9]{6}|18446744073709[0-4][0-9]{5}|184467440737095[0-4][0-9]{4}|18446744073709550[0-9]{3}|18446744073709551[0-5][0-9]{2}|1844674407370955160[0-9]|1844674407370955161[0-5])$/;
const REQUIRED_CAPABILITIES = [
  'transaction-context',
  'reviewed-context-signing',
  'cip8-cip95',
  'wallet-scoped-submission',
];
const DAPP_ERRORS = {
  dapp_invalid_request: 'Invalid backend request',
  dapp_context_conflict: 'Backend context conflict',
  dapp_identity_conflict: 'Submission identity conflict',
  dapp_account_changed: 'Wallet or network changed',
  dapp_context_unavailable: 'Wallet context unavailable',
  dapp_internal_error: 'Backend operation failed',
  dapp_tx_proof_generation: 'Transaction proof unavailable',
  dapp_deprecated_certificate: 'Deprecated certificate',
  dapp_data_proof_generation: 'Data proof unavailable',
  dapp_data_address_not_pk: 'Address is not a public-key credential',
  dapp_submission_failed: 'Transaction submission failed',
  dapp_submission_unavailable: 'Transaction submission unavailable',
};
type DappErrorCode = keyof typeof DAPP_ERRORS;
const CONTEXT_ERRORS: DappErrorCode[] = [
  'dapp_invalid_request',
  'dapp_context_conflict',
  'dapp_account_changed',
  'dapp_context_unavailable',
  'dapp_internal_error',
];
const WITNESS_ERRORS: DappErrorCode[] = [
  'dapp_invalid_request',
  'dapp_context_conflict',
  'dapp_tx_proof_generation',
  'dapp_deprecated_certificate',
  'dapp_account_changed',
  'dapp_context_unavailable',
  'dapp_internal_error',
];
const DATA_SIGNATURE_ERRORS: DappErrorCode[] = [
  'dapp_invalid_request',
  'dapp_data_address_not_pk',
  'dapp_data_proof_generation',
  'dapp_account_changed',
  'dapp_context_unavailable',
  'dapp_internal_error',
];
const KEY_STATE_ERRORS: DappErrorCode[] = [
  'dapp_account_changed',
  'dapp_context_unavailable',
];
const SUBMISSION_ERRORS: DappErrorCode[] = [
  'dapp_invalid_request',
  'dapp_identity_conflict',
  'dapp_account_changed',
  'dapp_submission_failed',
  'dapp_submission_unavailable',
];

// eslint-disable-next-line no-unused-vars
type Validator = (_value: unknown, _path: string) => void;

const fail = (path: string): never => {
  throw new TypeError(`Invalid dApp backend response at ${path}`);
};
const object = (
  value: unknown,
  path: string,
  fields: Record<string, Validator>
): Record<string, unknown> => {
  if (value === null || typeof value !== 'object' || Array.isArray(value))
    fail(path);
  const record = value as Record<string, unknown>;
  const keys = Object.keys(record);
  if (
    keys.length !== Object.keys(fields).length ||
    keys.some((key) => !(key in fields))
  )
    fail(path);
  Object.entries(fields).forEach(([key, validate]) =>
    validate(record[key], `${path}.${key}`)
  );
  return record;
};
const array = (item: Validator, minimum = 0, maximum = Infinity): Validator => (
  value,
  path
) => {
  if (!Array.isArray(value) || value.length < minimum || value.length > maximum)
    fail(path);
  (value as unknown[]).forEach((entry, index) =>
    item(entry, `${path}[${index}]`)
  );
};
const string = (pattern?: RegExp): Validator => (value, path) => {
  if (typeof value !== 'string' || (pattern && !pattern.test(value)))
    fail(path);
};
const number = (maximum = Number.MAX_SAFE_INTEGER): Validator => (
  value,
  path
) => {
  if (!Number.isSafeInteger(value) || (value as number) < 0 || value > maximum)
    fail(path);
};
const boolean: Validator = (value, path) => {
  if (typeof value !== 'boolean') fail(path);
};
const literal = (...values: unknown[]): Validator => (value, path) => {
  if (!values.includes(value)) fail(path);
};
const nullable = (validate: Validator): Validator => (value, path) => {
  if (value !== null) validate(value, path);
};

const network: Validator = (value, path) => {
  object(value, path, {
    network_id: literal(0, 1),
    network_magic: number(4294967295),
    genesis_hash: string(HEX_32),
  });
};
const outpoint: Validator = (value, path) => {
  object(value, path, {
    transaction_id: string(HEX_32),
    index: number(4294967295),
  });
};
const chainPoint: Validator = (value, path) => {
  if ((value as Record<string, unknown>)?.kind === 'genesis') {
    object(value, path, { kind: literal('genesis') });
  } else {
    object(value, path, {
      kind: literal('block'),
      slot: string(WORD64),
      block_hash: string(HEX_32),
    });
  }
};
const proofKind = literal(
  'normal_input',
  'collateral',
  'withdrawal',
  'certificate',
  'required_signer',
  'native_script',
  'policy'
);
const credentialKind = literal('payment', 'stake', 'drep', 'policy');

export const validateDappCapabilities = (
  value: unknown,
  expected: { sourceRevision: string; network: DappNetwork }
): DappCapabilities => {
  const record = object(value, 'capabilities', {
    api_version: literal(1),
    backend_build: (entry, path) => {
      object(entry, path, {
        version: string(),
        source_revision: string(HEX_40),
      });
    },
    network: (entry, path) => {
      object(entry, path, {
        network_id: literal(expected.network.network_id),
        network_magic: literal(expected.network.network_magic),
        genesis_hash: literal(expected.network.genesis_hash),
        current_era: literal('conway'),
      });
    },
    capabilities: array(
      (entry, path) => {
        object(entry, path, {
          name: literal(...REQUIRED_CAPABILITIES),
          revision: literal(1),
          available_eras: (eras, erasPath) => {
            if (
              !Array.isArray(eras) ||
              eras.length !== 1 ||
              eras[0] !== 'conway'
            )
              fail(erasPath);
          },
        });
      },
      4,
      4
    ),
  });
  const capabilities = record.capabilities as Array<Record<string, unknown>>;
  if (
    (record.backend_build as Record<string, unknown>).source_revision !==
      expected.sourceRevision ||
    new Set(capabilities.map(({ name }) => name)).size !== 4
  )
    fail('capabilities');
  return value as DappCapabilities;
};

export const validateDappTransactionContext = (
  value: unknown
): DappTransactionContext => {
  object(value, 'context', {
    revision: literal(1),
    wallet_id: string(HEX_40),
    network,
    chain_point: chainPoint,
    wallet_generation: string(WORD64),
    pending_generation: string(WORD64),
    era: literal('conway'),
    protocol_version: (entry, path) => {
      object(entry, path, {
        major: number(4294967295),
        minor: number(4294967295),
      });
    },
    protocol_parameters_cbor: string(HEX),
    volatile_delta: (entry, path) => {
      object(entry, path, {
        point: chainPoint,
        node_transaction_inputs: array(string(HEX)),
      });
    },
    outputs: array((entry, path) => {
      object(entry, path, {
        outpoint,
        transaction_input_cbor: string(HEX),
        source_transaction_output_cbor: string(HEX),
        canonical_transaction_output_cbor: string(HEX),
        transaction_unspent_output_cbor: string(HEX),
        provenance: array(literal('earlier', 'pending', 'node'), 1),
        roles: array(
          literal('normal', 'collateral', 'reference', 'wallet_snapshot'),
          1
        ),
        wallet_member: boolean,
        pending_state: literal('none', 'outcome_unknown'),
      });
    }),
    pending_overlay: (entry, path) => {
      object(entry, path, {
        transactions: array((transaction, transactionPath) => {
          object(transaction, transactionPath, {
            transaction_id: string(HEX_32),
            state: literal('outcome_unknown'),
            transaction_cbor: string(HEX),
            normal_inputs: array(outpoint),
            collateral_inputs: array(outpoint),
            expiry_slot: nullable(string(WORD64)),
          });
        }),
        spent_wallet_inputs: array(outpoint),
        produced_wallet_outputs: array(outpoint, 0, 0),
      });
    },
    ownership: array((entry, path) => {
      object(entry, path, {
        credential_kind: credentialKind,
        credential: string(HEX_28),
        ownership: literal('unowned', 'owned_key', 'script'),
        derivation_path: array(number(4294967295)),
        proof_kinds: array(proofKind, 0, 7),
      });
    }),
    required_wallet_proofs: array((entry, path) => {
      object(entry, path, {
        transaction_index: number(49),
        proof_kind: proofKind,
        credential_kind: credentialKind,
        credential: string(HEX_28),
        required: boolean,
      });
    }),
    batch_overlay: (entry, path) => {
      object(entry, path, {
        dependencies: array((dependency, dependencyPath) => {
          object(dependency, dependencyPath, {
            transaction_index: number(49),
            input_role: literal('normal', 'collateral', 'reference'),
            outpoint,
            source: literal('earlier', 'pending'),
            source_transaction_index: nullable(number(49)),
          });
        }),
        conflicts: array((conflict, conflictPath) => {
          object(conflict, conflictPath, {
            transaction_index: number(49),
            input_role: literal('normal', 'collateral'),
            outpoint,
            earlier_transaction_index: number(49),
          });
        }),
      });
    },
    records: array(string(HEX)),
    context_digest: string(HEX_32),
    context_token: string(HEX),
  });
  return value as DappTransactionContext;
};

export const validateDappWitnessResponse = (
  value: unknown
): DappWitnessResponse => {
  object(value, 'witnesses', {
    revision: literal(1),
    witnesses: array(
      (entry, path) => {
        object(entry, path, {
          transaction_index: number(49),
          body_hash: string(HEX_32),
          witness_set_cbor: string(HEX),
        });
      },
      1,
      50
    ),
  });
  return value as DappWitnessResponse;
};

export const validateDappDataSignatureResponse = (
  value: unknown
): DappDataSignatureResponse => {
  object(value, 'dataSignature', {
    revision: literal(1),
    credential_kind: literal('payment', 'stake', 'drep'),
    credential: string(HEX_28),
    cose_sign1: string(HEX),
    cose_key: string(HEX),
  });
  return value as DappDataSignatureResponse;
};

export const validateDappCip95KeyState = (
  value: unknown
): DappCip95KeyState => {
  object(value, 'cip95KeyState', {
    drep_public_key: string(HEX_32),
    registered_stake_public_keys: array(string(HEX_32)),
    unregistered_stake_public_keys: array(string(HEX_32)),
  });
  return value as DappCip95KeyState;
};

export const validateDappSubmissionResponse = (
  value: unknown
): DappSubmissionResponse => {
  object(value, 'submission', {
    revision: literal(1),
    transaction_id: string(HEX_32),
    status: literal(
      'authorized',
      'broadcasting',
      'submitted',
      'rejected',
      'outcome_unknown',
      'in_ledger',
      'expired'
    ),
  });
  return value as DappSubmissionResponse;
};

export const validateDappError = (
  value: unknown,
  allowed: DappErrorCode[] = Object.keys(DAPP_ERRORS) as DappErrorCode[]
): never => {
  const record = object(value, 'error', {
    code: literal(...allowed),
    message: string(),
  });
  const code = record.code as DappErrorCode;
  if (DAPP_ERRORS[code] !== record.message) fail('error.message');
  throw value;
};

const checked = async <Response>(
  promise: Promise<unknown>,
  // eslint-disable-next-line no-unused-vars
  validate: (_value: unknown) => Response,
  allowedErrors: DappErrorCode[] = []
): Promise<Response> => {
  try {
    return validate(await promise);
  } catch (error) {
    if (error instanceof Error) throw error;
    return validateDappError(error, allowedErrors);
  }
};

const post = <Response>(
  config: RequestConfig,
  walletId: string,
  path: string,
  body: unknown,
  // eslint-disable-next-line no-unused-vars
  validate: (_value: unknown) => Response,
  allowedErrors: DappErrorCode[]
): Promise<Response> =>
  checked(
    request(
      { method: 'POST', path: `/v2/wallets/${walletId}/${path}`, ...config },
      {},
      body
    ),
    validate,
    allowedErrors
  );

export const getDappCapabilities = (
  config: RequestConfig,
  expected: { sourceRevision: string; network: DappNetwork }
): Promise<DappCapabilities> =>
  checked(
    request({ method: 'GET', path: '/v2/dapp-capabilities', ...config }),
    (value) => validateDappCapabilities(value, expected)
  );

export const getDappTransactionContext = (
  config: RequestConfig,
  walletId: string,
  body: DappTransactionContextRequest
): Promise<DappTransactionContext> =>
  post(
    config,
    walletId,
    'transaction-context',
    body,
    validateDappTransactionContext,
    CONTEXT_ERRORS
  );

export const signDappTransactions = (
  config: RequestConfig,
  walletId: string,
  body: DappWitnessRequest
): Promise<DappWitnessResponse> =>
  post(
    config,
    walletId,
    'transaction-witnesses',
    body,
    validateDappWitnessResponse,
    WITNESS_ERRORS
  );

export const signDappData = (
  config: RequestConfig,
  walletId: string,
  body: DappDataSignatureRequest
): Promise<DappDataSignatureResponse> =>
  post(
    config,
    walletId,
    'data-signatures',
    body,
    validateDappDataSignatureResponse,
    DATA_SIGNATURE_ERRORS
  );

export const getDappCip95KeyState = (
  config: RequestConfig,
  walletId: string
): Promise<DappCip95KeyState> =>
  checked(
    request({
      method: 'GET',
      path: `/v2/wallets/${walletId}/cip95-key-state`,
      ...config,
    }),
    validateDappCip95KeyState,
    KEY_STATE_ERRORS
  );

export const submitDappTransaction = (
  config: RequestConfig,
  walletId: string,
  body: DappSubmissionRequest
): Promise<DappSubmissionResponse> =>
  post(
    config,
    walletId,
    'transaction-submission',
    body,
    validateDappSubmissionResponse,
    SUBMISSION_ERRORS
  );
