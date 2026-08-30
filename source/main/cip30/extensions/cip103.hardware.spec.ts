import { generateKeyPairSync, sign } from 'crypto';
import cbor from 'cbor';

import { decodeConwayTransaction } from '../../../common/cardano/transaction';
import { parseConwayTransactionEnvelope } from '../../../common/cardano/transactionEnvelope';
import {
  encodeVKeyWitnessSet,
  extractVKeyWitnesses,
} from '../../../common/cardano/witnessSet';
import type {
  HardwareExactTransaction,
  HardwareTransactionPreparation,
} from '../../../common/types/hardware-wallets.types';
import type {
  Cip103PreflightBatch,
  Cip103PreflightItem,
} from '../../../common/types/cip103.types';
import {
  Cip103HardwareSigningError,
  Cip103ExecutionReview,
  signCip103HardwareBatch,
} from './cip103';

const item = (index: number, partialSign: boolean): Cip103PreflightItem => {
  const envelope = parseConwayTransactionEnvelope(
    cbor.encodeCanonical([
      new Map<number, unknown>([
        [0, []],
        [1, []],
        [2, index],
      ]),
      new Map(),
      true,
      null,
    ])
  );
  return Object.freeze({
    index,
    cbor: envelope.cbor.toString('hex'),
    fullCborDigest: `${index + 1}`.repeat(64),
    bodyHash: envelope.transactionId,
    partialSign,
    envelope,
    transaction: decodeConwayTransaction(envelope),
  });
};

const batch: Cip103PreflightBatch = Object.freeze({
  state: 'preflighted',
  operation: 'sign',
  items: Object.freeze([item(0, false), item(1, true), item(2, false)]),
});
const review: Cip103ExecutionReview = Object.freeze({
  mode: 'sign',
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

const signed = batch.items.map(({ bodyHash }) => {
  const pair = generateKeyPairSync('ed25519');
  const publicKey = pair.publicKey
    .export({ format: 'der', type: 'spki' })
    .subarray(-32);
  const cborHex = encodeVKeyWitnessSet([
    {
      publicKey,
      signature: sign(null, Buffer.from(bodyHash, 'hex'), pair.privateKey),
    },
  ]).toString('hex');
  const [witness] = extractVKeyWitnesses(Buffer.from(cborHex, 'hex'));
  return Object.freeze({
    cbor: cborHex,
    keyHash: witness.keyHash.toString('hex'),
  });
});

const exact = (index: number, withDeviceKey = true): HardwareExactTransaction =>
  Object.freeze({
    transaction: batch.items[index].transaction,
    bodyHash: batch.items[index].bodyHash,
    contextDigest: 'aa'.repeat(32),
    network: Object.freeze({
      networkId: 0,
      networkMagic: 2,
      genesisHash: 'bb'.repeat(32),
    }),
    partialSign: batch.items[index].partialSign ?? false,
    signers: Object.freeze([]),
    ownedInputs: Object.freeze([]),
    ownedOutputs: Object.freeze([]),
    witnesses: Object.freeze({
      requiredKeyHashes: Object.freeze(
        batch.items[index].partialSign || !withDeviceKey
          ? []
          : [signed[index].keyHash]
      ),
      preExistingKeyHashes: Object.freeze([]),
      requestedDeviceKeyHashes: Object.freeze(
        withDeviceKey ? [signed[index].keyHash] : []
      ),
      missingKeyHashes: Object.freeze([]),
      unexpectedKeyHashes: Object.freeze([]),
    }),
    capability: Object.freeze({
      matrixRevision: 'matrix',
      artifactId: 'artifact',
      rowId: 'row',
      vendor: 'ledger',
      staticallyRepresentable: true,
      staticGatesPassed: true,
      physicalCertified: true,
      productEnabled: true,
      familyDispositions: Object.freeze({}),
    }),
  });

const ready = (index: number): HardwareTransactionPreparation =>
  Object.freeze({
    status: 'ready',
    deviceInteraction: true,
    exact: exact(index),
  });
const empty = (index: number): HardwareTransactionPreparation =>
  Object.freeze({
    status: 'empty',
    deviceInteraction: false,
    witnessSetCbor: 'a0',
    exact: exact(index, false),
  });
const rejected = (index: number): HardwareTransactionPreparation =>
  Object.freeze({
    status: 'rejected',
    deviceInteraction: false,
    reasons: Object.freeze(['not-representable']),
    exact: exact(index),
  });

const request = (
  preparations: readonly HardwareTransactionPreparation[],
  controller = new AbortController()
) => {
  const prepare = jest.fn((index: number) => preparations[index]);
  const signTransaction = jest.fn(
    async (_transaction: HardwareExactTransaction, index: number) =>
      signed[index].cbor
  );
  const cancelDevice = jest.fn(async () => undefined);
  return {
    controller,
    prepare,
    signTransaction,
    cancelDevice,
    value: {
      batch,
      review,
      signal: controller.signal,
      prepare,
      signTransaction,
      cancelDevice,
    },
  };
};

describe('CIP-103 hardware batch signing', () => {
  it('confirms ready items in caller order and skips the canonical empty partial item', async () => {
    const fixture = request([ready(0), empty(1), ready(2)]);

    const result = await signCip103HardwareBatch(fixture.value);

    expect(fixture.prepare.mock.calls.map(([index]) => index)).toEqual([
      0,
      1,
      2,
    ]);
    expect(
      fixture.signTransaction.mock.calls.map(([, index]) => index)
    ).toEqual([0, 2]);
    expect(result).toEqual([signed[0].cbor, 'a0', signed[2].cbor]);
    expect(Object.isFrozen(result)).toBe(true);
    expect(fixture.cancelDevice).not.toHaveBeenCalled();
  });

  it.each([0, 1, 2])(
    'releases nothing when device confirmation rejects at index %i',
    async (rejectedIndex) => {
      const fixture = request([ready(0), ready(1), ready(2)]);
      fixture.signTransaction.mockImplementation(
        async (_transaction: HardwareExactTransaction, index: number) => {
          if (index === rejectedIndex)
            throw Object.assign(new Error('declined'), {
              code: 'TxSignError.UserDeclined',
            });
          return signed[index].cbor;
        }
      );
      let released: readonly string[] | undefined;

      const error = await signCip103HardwareBatch(fixture.value)
        .then((result) => {
          released = result;
          return undefined;
        })
        .catch((failure) => failure);

      expect(released).toBeUndefined();
      expect(error).toMatchObject({
        failure: 'user-declined',
        transactionIndex: rejectedIndex,
      });
      expect(
        fixture.signTransaction.mock.calls.map(([, index]) => index)
      ).toEqual(
        Array.from({ length: rejectedIndex + 1 }, (_value, index) => index)
      );
    }
  );

  it('cancels once and suppresses a late device result and all later items', async () => {
    const fixture = request([ready(0), ready(1), ready(2)]);
    fixture.signTransaction.mockImplementation(
      async (_transaction: HardwareExactTransaction, index: number) => {
        if (index === 1) fixture.controller.abort();
        return signed[index].cbor;
      }
    );
    let released: readonly string[] | undefined;

    const error = await signCip103HardwareBatch(fixture.value)
      .then((result) => {
        released = result;
        return undefined;
      })
      .catch((failure) => failure);

    expect(released).toBeUndefined();
    expect(error).toMatchObject({ failure: 'cancelled', transactionIndex: 1 });
    expect(
      fixture.signTransaction.mock.calls.map(([, index]) => index)
    ).toEqual([0, 1]);
    expect(fixture.cancelDevice).toHaveBeenCalledTimes(1);
  });

  it('cancels before preparation results can reach a device', async () => {
    const controller = new AbortController();
    controller.abort();
    const fixture = request([ready(0), ready(1), ready(2)], controller);

    await expect(signCip103HardwareBatch(fixture.value)).rejects.toMatchObject({
      failure: 'cancelled',
      transactionIndex: 0,
    });
    expect(fixture.prepare).not.toHaveBeenCalled();
    expect(fixture.signTransaction).not.toHaveBeenCalled();
    expect(fixture.cancelDevice).toHaveBeenCalledTimes(1);
  });

  it('preflights every item before the first device confirmation', async () => {
    const fixture = request([ready(0), ready(1), rejected(2)]);

    const error = await signCip103HardwareBatch(fixture.value).catch(
      (failure) => failure
    );

    expect(error).toBeInstanceOf(Cip103HardwareSigningError);
    expect(error).toMatchObject({
      failure: 'proof-generation',
      transactionIndex: 2,
    });
    expect(fixture.signTransaction).not.toHaveBeenCalled();
  });
});
