const nodeHttps = require('https');

global.https = nodeHttps;
global.environment = { ...global.environment, isSelfnode: false };

const { getOctetStreamBody } = require('../utils/request');
const {
  validateDappCapabilities,
  validateDappCip95KeyState,
  validateDappDataSignatureResponse,
  validateDappError,
  validateDappSubmissionResponse,
  validateDappTransactionContext,
  validateDappWitnessResponse,
} = require('./dappBackend');

const network = {
  network_id: 1 as const,
  network_magic: 764824073,
  genesis_hash: 'a'.repeat(64),
};

const context = {
  revision: 1,
  wallet_id: 'b'.repeat(40),
  network,
  chain_point: { kind: 'block', slot: '1', block_hash: 'c'.repeat(64) },
  wallet_generation: '1',
  pending_generation: '2',
  era: 'conway',
  protocol_version: { major: 10, minor: 0 },
  protocol_parameters_cbor: 'a0',
  volatile_delta: {
    point: { kind: 'block', slot: '1', block_hash: 'c'.repeat(64) },
    node_transaction_inputs: ['80'],
  },
  outputs: [],
  pending_overlay: {
    transactions: [],
    spent_wallet_inputs: [],
    produced_wallet_outputs: [],
  },
  ownership: [],
  required_wallet_proofs: [],
  batch_overlay: { dependencies: [], conflicts: [] },
  records: ['80'],
  context_digest: 'd'.repeat(64),
  context_token: '00',
};

const capabilities = {
  api_version: 1,
  backend_build: { version: '2026.8.0', source_revision: 'e'.repeat(40) },
  network: { ...network, current_era: 'conway' },
  capabilities: [
    'transaction-context',
    'reviewed-context-signing',
    'cip8-cip95',
    'wallet-scoped-submission',
  ].map((name) => ({ name, revision: 1, available_eras: ['conway'] })),
};

describe('dApp backend client contracts', () => {
  it('uses the exact octet-stream bytes and byte length', () => {
    const bytes = Buffer.from([0, 1, 127, 128, 255]);
    const result = getOctetStreamBody(bytes);

    expect(result.body).toBe(bytes);
    expect(result.contentLength).toBe(5);
    expect(() => getOctetStreamBody('00017f80ff')).toThrow(
      'Octet-stream request body must be bytes'
    );
  });

  it('requires the complete revision-1 capability identity', () => {
    expect(
      validateDappCapabilities(capabilities, {
        sourceRevision: 'e'.repeat(40),
        network,
      })
    ).toBe(capabilities);

    expect(() =>
      validateDappCapabilities(
        {
          ...capabilities,
          capabilities: capabilities.capabilities.map((capability, index) =>
            index === 0 ? { ...capability, revision: 0 } : capability
          ),
        },
        { sourceRevision: 'e'.repeat(40), network }
      )
    ).toThrow('Invalid dApp backend response');
    expect(() =>
      validateDappCapabilities(capabilities, {
        sourceRevision: 'f'.repeat(40),
        network,
      })
    ).toThrow('Invalid dApp backend response');
  });

  it('validates every dApp success response before use', () => {
    expect(validateDappTransactionContext(context)).toBe(context);
    expect(
      validateDappWitnessResponse({
        revision: 1,
        witnesses: [
          {
            transaction_index: 0,
            body_hash: '1'.repeat(64),
            witness_set_cbor: 'a0',
          },
        ],
      }).witnesses
    ).toHaveLength(1);
    expect(
      validateDappDataSignatureResponse({
        revision: 1,
        credential_kind: 'payment',
        credential: '2'.repeat(56),
        cose_sign1: '80',
        cose_key: 'a0',
      }).credential_kind
    ).toBe('payment');
    expect(
      validateDappCip95KeyState({
        drep_public_key: '3'.repeat(64),
        registered_stake_public_keys: ['4'.repeat(64)],
        unregistered_stake_public_keys: [],
      }).registered_stake_public_keys
    ).toHaveLength(1);
    expect(
      validateDappSubmissionResponse({
        revision: 1,
        transaction_id: '5'.repeat(64),
        status: 'submitted',
      }).status
    ).toBe('submitted');
  });

  it('rejects extra response fields and noncanonical backend errors', () => {
    expect(() =>
      validateDappSubmissionResponse({
        revision: 1,
        transaction_id: '5'.repeat(64),
        status: 'submitted',
        transaction: 'sensitive',
      })
    ).toThrow('Invalid dApp backend response');

    const canonical = {
      code: 'dapp_context_unavailable',
      message: 'Wallet context unavailable',
    };
    let thrown;
    try {
      validateDappError(canonical);
    } catch (error) {
      thrown = error;
    }
    expect(thrown).toBe(canonical);
    expect(() =>
      validateDappError({ ...canonical, message: 'token=secret' })
    ).toThrow('Invalid dApp backend response');
    expect(() =>
      validateDappError(canonical, ['dapp_account_changed'])
    ).toThrow('Invalid dApp backend response');
  });
});
