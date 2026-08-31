import { generateKeyPairSync, sign } from 'crypto';
import cbor from 'cbor';
import { Given, Then, When } from 'cucumber';
import { expect } from 'chai';

import { preflightCip103Sign } from '../../../../../source/common/cip30/cip103Batch';
import {
  encodeVKeyWitnessSet,
  extractVKeyWitnesses,
} from '../../../../../source/common/cardano/witnessSet';
import type {
  Cip30WalletRequest,
  Cip30WalletResponse,
} from '../../../../../source/common/cip30/executor';
import type { Cip103PreflightBatch } from '../../../../../source/common/types/cip103.types';
import {
  Cip103WalletSigningRequest,
  signCip103WalletBatch,
} from '../../../../../source/main/cip30/extensions/cip103';

const network = {
  networkId: 0 as const,
  networkMagic: 2,
  genesisHash: 'aa'.repeat(32),
};

const transaction = (fee: number): string =>
  cbor
    .encodeCanonical([
      new Map<number, unknown>([
        [0, []],
        [1, []],
        [2, fee],
      ]),
      new Map(),
      true,
      null,
    ])
    .toString('hex');

const review = (batch: Cip103PreflightBatch) => ({
  mode: 'sign' as const,
  approvable: true,
  items: batch.items.map(
    ({ index, bodyHash, fullCborDigest, cbor: fullCbor }) => ({
      index,
      transaction: { transactionId: bodyHash, fullCborDigest, fullCbor },
    })
  ),
});

Given(/^a two-item CIP-103 batch for a "([^"]*)" wallet$/, async function (
  wallet: string
) {
  let walletKind: Cip103WalletSigningRequest['walletKind'];
  if (wallet === 'software') walletKind = 'shelley-software';
  else if (wallet === 'Ledger') walletKind = 'ledger';
  else walletKind = 'trezor';
  const batch = preflightCip103Sign(
    [
      { cbor: transaction(1), partialSign: false },
      { cbor: transaction(2), partialSign: true },
    ],
    network.networkId
  );
  const signed = batch.items.map(({ bodyHash }) => {
    const keys = generateKeyPairSync('ed25519');
    const publicKey = keys.publicKey
      .export({ format: 'der', type: 'spki' })
      .subarray(-32);
    const witnessSet = encodeVKeyWitnessSet([
      {
        publicKey,
        signature: sign(null, Buffer.from(bodyHash, 'hex'), keys.privateKey),
      },
    ]).toString('hex');
    const [witness] = extractVKeyWitnesses(Buffer.from(witnessSet, 'hex'));
    return { witnessSet, keyHash: witness.keyHash.toString('hex') };
  });
  this.context.cip103 = { batch, signed, walletKind };
});

When(/^the user approves the CIP-103 batch$/, async function () {
  const { batch, signed, walletKind } = this.context.cip103;
  const calls: Cip30WalletRequest[] = [];
  const executeWallet = async (
    request: Cip30WalletRequest
  ): Promise<Cip30WalletResponse> => {
    calls.push(request);
    return {
      status: 'fulfilled',
      operation: 'sign-transactions',
      value: {
        revision: 1,
        witnesses: batch.items.map(
          ({ bodyHash }, transactionIndex: number) => ({
            transaction_index: transactionIndex,
            body_hash: bodyHash,
            witness_set_cbor: signed[transactionIndex].witnessSet,
          })
        ),
      },
    };
  };
  const result = await signCip103WalletBatch(executeWallet, {
    walletId: 'wallet',
    walletKind,
    network,
    sourceRevision: 'revision',
    batch,
    review: review(batch),
    signingContext: { revision: 1 },
    ...(walletKind === 'shelley-software' ? { passphrase: 'secret' } : {}),
    requiredKeyHashes: signed.map(({ keyHash }) => [keyHash]),
  });
  this.context.cip103 = { ...this.context.cip103, calls, result };
});

Then(
  /^the connector releases both verified witnesses in caller order$/,
  async function () {
    const { calls, result, signed } = this.context.cip103;
    expect(result).to.deep.equal(signed.map(({ witnessSet }) => witnessSet));
    expect(calls).to.have.length(1);
    expect(calls[0].transactions.map(({ cbor }) => cbor)).to.deep.equal(
      this.context.cip103.batch.items.map(({ cbor }) => cbor)
    );
  }
);

Then(/^the hardware batch requires no software passphrase$/, async function () {
  expect(this.context.cip103.calls[0]).not.to.have.property('passphrase');
});
