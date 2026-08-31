import { generateKeyPairSync, sign } from 'crypto';
import cbor from 'cbor';

import { decodeConwayTransaction } from '../../../common/cardano/transaction';
import { parseConwayTransactionEnvelope } from '../../../common/cardano/transactionEnvelope';
import {
  encodeVKeyWitnessSet,
  extractVKeyWitnesses,
} from '../../../common/cardano/witnessSet';
import type {
  Cip30WalletRequest,
  Cip30WalletResponse,
} from '../../../common/cip30/executor';
import type {
  Cip103PreflightBatch,
  Cip103PreflightItem,
} from '../../../common/types/cip103.types';
import {
  Cip103SoftwareSigningError,
  Cip103WalletSigningRequest,
  signCip103WalletBatch,
} from './cip103';

const network = {
  networkId: 0 as const,
  networkMagic: 2,
  genesisHash: 'aa'.repeat(32),
};

const item = (index: number, fee: number): Cip103PreflightItem => {
  const envelope = parseConwayTransactionEnvelope(
    cbor.encodeCanonical([
      new Map<number, unknown>([
        [0, []],
        [1, []],
        [2, fee],
      ]),
      new Map(),
      true,
      null,
    ])
  );
  return Object.freeze({
    index,
    cbor: envelope.cbor.toString('hex'),
    fullCborDigest: '00'.repeat(32),
    bodyHash: envelope.transactionId,
    partialSign: false,
    envelope,
    transaction: decodeConwayTransaction(envelope),
  });
};

const batch: Cip103PreflightBatch = Object.freeze({
  state: 'preflighted',
  operation: 'sign',
  items: Object.freeze([item(0, 0), item(1, 1)]),
});
const review = Object.freeze({
  mode: 'sign' as const,
  approvable: true,
  items: Object.freeze(
    batch.items.map(({ index, bodyHash, fullCborDigest }) =>
      Object.freeze({
        index,
        transaction: Object.freeze({
          transactionId: bodyHash,
          fullCborDigest,
          fullCbor: batch.items[index].cbor,
        }),
      })
    )
  ),
});
const signingContext = Object.freeze({
  revision: 1,
  context_digest: 'bb'.repeat(32),
});

const signingRequest = (
  requiredKeyHashes?: readonly (readonly string[])[]
): Cip103WalletSigningRequest => ({
  walletId: 'wallet',
  walletKind: 'shelley-software',
  network,
  sourceRevision: 'revision',
  batch,
  review,
  signingContext,
  passphrase: 'secret',
  requiredKeyHashes,
});

const executor = (response: Cip30WalletResponse) =>
  jest.fn(
    async (_request: Cip30WalletRequest): Promise<Cip30WalletResponse> =>
      response
  );

const fulfilled = (
  witnesses: ReadonlyArray<{
    transaction_index: number;
    body_hash: string;
    witness_set_cbor: string;
  }>
): Cip30WalletResponse => ({
  status: 'fulfilled',
  operation: 'sign-transactions',
  value: { revision: 1, witnesses },
});

const signedWitness = (index: number) => {
  const pair = generateKeyPairSync('ed25519');
  const publicKey = pair.publicKey
    .export({ format: 'der', type: 'spki' })
    .subarray(-32);
  const witnessSet = encodeVKeyWitnessSet([
    {
      publicKey,
      signature: sign(
        null,
        Buffer.from(batch.items[index].bodyHash, 'hex'),
        pair.privateKey
      ),
    },
  ]);
  const [witness] = extractVKeyWitnesses(witnessSet);
  return {
    keyHash: witness.keyHash.toString('hex'),
    cbor: witnessSet.toString('hex'),
  };
};

const emptyWitnesses = () =>
  batch.items.map(({ bodyHash }, transaction_index) => ({
    transaction_index,
    body_hash: bodyHash,
    witness_set_cbor: 'a0',
  }));

describe('CIP-103 software batch signing', () => {
  it('uses one password/context call and releases only the fully verified ordered set', async () => {
    const second = signedWitness(1);
    const witnesses = emptyWitnesses();
    witnesses[1] = {
      transaction_index: 1,
      body_hash: batch.items[1].bodyHash,
      witness_set_cbor: second.cbor,
    };
    const executeWallet = executor(fulfilled(witnesses));

    const result = await signCip103WalletBatch(
      executeWallet,
      signingRequest([[], [second.keyHash]])
    );

    expect(executeWallet).toHaveBeenCalledTimes(1);
    expect(executeWallet).toHaveBeenCalledWith({
      operation: 'sign-transactions',
      walletId: 'wallet',
      network,
      sourceRevision: 'revision',
      context: signingContext,
      transactions: batch.items.map(({ cbor }) => ({
        cbor,
        partialSign: false,
      })),
      passphrase: 'secret',
    });
    expect(result).toEqual(['a0', second.cbor]);
    expect(Object.isFrozen(result)).toBe(true);
  });

  it.each([
    ['missing witness', emptyWitnesses().slice(0, 1)],
    [
      'extra witness',
      [
        ...emptyWitnesses(),
        {
          transaction_index: 2,
          body_hash: batch.items[1].bodyHash,
          witness_set_cbor: 'a0',
        },
      ],
    ],
    [
      'misaligned witness',
      [{ ...emptyWitnesses()[0], transaction_index: 1 }, emptyWitnesses()[1]],
    ],
    [
      'body-hash mismatch',
      [
        { ...emptyWitnesses()[0], body_hash: 'ff'.repeat(32) },
        emptyWitnesses()[1],
      ],
    ],
    [
      'malformed witness',
      [emptyWitnesses()[0], { ...emptyWitnesses()[1], witness_set_cbor: '80' }],
    ],
  ])('releases no result for a %s', async (_label, witnesses) => {
    let released: readonly string[] | undefined;
    const error = await signCip103WalletBatch(
      executor(fulfilled(witnesses)),
      signingRequest()
    ).catch((failure) => {
      if (failure instanceof Cip103SoftwareSigningError) return failure;
      throw failure;
    });

    expect(released).toBeUndefined();
    expect(error).toMatchObject({ failure: 'internal' });
  });

  it('discards an earlier staged witness when a later CIP-95 proof is missing', async () => {
    const required = signedWitness(1).keyHash;
    let released: readonly string[] | undefined;
    const error = await signCip103WalletBatch(
      executor(fulfilled(emptyWitnesses())),
      signingRequest([[], [required]])
    )
      .then((result) => {
        released = result;
        return undefined;
      })
      .catch((failure) => failure);

    expect(released).toBeUndefined();
    expect(error).toMatchObject({
      failure: 'internal',
      transactionIndex: 1,
    });
  });

  it.each(['tx-proof-generation', 'deprecated-certificate'] as const)(
    'preserves the backend %s failure without releasing witnesses',
    async (reason) => {
      const error = await signCip103WalletBatch(
        executor({ status: 'rejected', reason }),
        signingRequest()
      ).catch((failure) => failure);

      expect(error).toMatchObject({ failure: reason });
    }
  );

  it('rejects invalid local signing inputs before backend work', async () => {
    const executeWallet = executor(fulfilled(emptyWitnesses()));

    await expect(
      signCip103WalletBatch(executeWallet, {
        ...signingRequest(),
        passphrase: '',
      })
    ).rejects.toMatchObject({ failure: 'internal' });
    await expect(
      signCip103WalletBatch(
        executeWallet,
        signingRequest([[], ['not-a-key-hash']])
      )
    ).rejects.toMatchObject({ failure: 'internal' });
    await expect(
      signCip103WalletBatch(executeWallet, {
        ...signingRequest(),
        review: { ...review, approvable: false },
      })
    ).rejects.toMatchObject({ failure: 'internal' });
    await expect(
      signCip103WalletBatch(executeWallet, {
        ...signingRequest(),
        review: { ...review, mode: 'submit' },
      })
    ).rejects.toMatchObject({ failure: 'internal' });
    expect(executeWallet).not.toHaveBeenCalled();
  });
});
