import { blake2b } from 'blakejs';
import { utils } from '@cardano-foundation/ledgerjs-hw-app-cardano';
import type { CoinSelectionOutput } from '../api/transactions/types';
import { CachedDeriveXpubFactory } from './hardwareWalletUtils';
import { decodeConwayOutput } from '../../../common/cardano/transaction';
import type {
  ContextOwnership,
  ContextRequiredProof,
  TransactionContextSnapshot,
} from '../../../common/cardano/transactionContext';
import {
  encodeVKeyWitnessSet,
  extractEnvelopeVKeyWitnesses,
} from '../../../common/cardano/witnessSet';
import type {
  HardwareExactTransaction,
  HardwareOwnedInput,
  HardwareOwnedAddress,
  HardwareSigner,
  HardwareTransactionCapability,
  HardwareTransactionPreparation,
} from '../../../common/types/hardware-wallets.types';

const outpoint = (transactionId: string, index: bigint | number): string =>
  `${transactionId}:${index}`;

const paymentCredential = (
  address: string,
  networkId: 0 | 1
): string | null => {
  const bytes = Buffer.from(address, 'hex');
  if (bytes.length < 29 || (bytes[0] & 0x0f) !== networkId) return null;
  return [0, 2, 4, 6].includes(bytes[0] >> 4)
    ? bytes.subarray(1, 29).toString('hex')
    : null;
};

const ownedKey = (
  ownership: readonly ContextOwnership[],
  credentialKind: ContextRequiredProof['credentialKind'],
  credential: string
): ContextOwnership | undefined =>
  ownership.find(
    (candidate) =>
      candidate.credentialKind === credentialKind &&
      candidate.credential === credential &&
      candidate.ownership === 'owned_key'
  );

const freezeSigner = (
  proof: ContextRequiredProof,
  ownership: ContextOwnership
): HardwareSigner =>
  Object.freeze({
    credentialKind: proof.credentialKind,
    keyHash: proof.credential,
    path: Object.freeze([...ownership.derivationPath]),
    proofKinds: Object.freeze([...ownership.proofKinds]),
  });

const outputBinding = (
  address: string,
  ownership: readonly ContextOwnership[],
  networkId: 0 | 1
): HardwareOwnedAddress | null => {
  const bytes = Buffer.from(address, 'hex');
  if (bytes.length < 29 || (bytes[0] & 0x0f) !== networkId) return null;
  const type = bytes[0] >> 4;
  const payment = [0, 2, 4, 6].includes(type)
    ? ownedKey(ownership, 'payment', bytes.subarray(1, 29).toString('hex'))
    : undefined;
  let stake: ContextOwnership | undefined;
  if ([0, 1, 2, 3].includes(type) && bytes.length >= 57)
    stake = ownedKey(
      ownership,
      'stake',
      bytes.subarray(29, 57).toString('hex')
    );
  else if (type === 14)
    stake = ownedKey(ownership, 'stake', bytes.subarray(1, 29).toString('hex'));
  if (!payment && !stake) return null;
  return Object.freeze({
    address,
    ...(payment
      ? { paymentPath: Object.freeze([...payment.derivationPath]) }
      : {}),
    ...(stake ? { stakePath: Object.freeze([...stake.derivationPath]) } : {}),
  });
};

export const prepareHardwareTransaction = (
  snapshot: TransactionContextSnapshot,
  transactionIndex: number,
  partialSign: boolean,
  capability: HardwareTransactionCapability
): HardwareTransactionPreparation => {
  const transaction = snapshot.transactionsSemantic[transactionIndex];
  if (
    !transaction ||
    snapshot.transactions[transactionIndex] !==
      transaction.envelope.cbor.toString('hex')
  )
    throw new Error('Invalid hardware transaction context');

  const resolved = new Map(
    snapshot.outputs.map((output) => [
      outpoint(output.outpoint.transactionId, output.outpoint.index),
      output,
    ])
  );
  const reasons: string[] = [];
  const proofs = snapshot.requiredProofs.filter(
    (proof) => proof.transactionIndex === transactionIndex
  );
  const signers: HardwareSigner[] = [];
  const signerByHash = new Map<string, HardwareSigner>();
  proofs.forEach((proof) => {
    if (
      proof.proofKind === 'normal_input' ||
      proof.proofKind === 'collateral'
    ) {
      const role = proof.proofKind === 'normal_input' ? 'normal' : 'collateral';
      const bound = transaction.inputs[role].some((input) => {
        const output = resolved.get(outpoint(input.transactionId, input.index));
        return (
          output !== undefined &&
          paymentCredential(
            decodeConwayOutput(Buffer.from(output.sourceCbor, 'hex')).address,
            snapshot.network.networkId
          ) === proof.credential
        );
      });
      if (!bound)
        reasons.push(`unbound-${proof.proofKind}:${proof.credential}`);
    }
    const ownership = ownedKey(
      snapshot.ownership,
      proof.credentialKind,
      proof.credential
    );
    if (!ownership) return;
    if (!ownership.proofKinds.includes(proof.proofKind)) {
      reasons.push(`untrusted-path:${proof.credential}`);
      return;
    }
    const signer = freezeSigner(proof, ownership);
    const existing = signerByHash.get(signer.keyHash);
    if (
      existing &&
      (existing.credentialKind !== signer.credentialKind ||
        existing.path.join('/') !== signer.path.join('/'))
    ) {
      reasons.push(`ambiguous-path:${signer.keyHash}`);
      return;
    }
    if (!existing) {
      signerByHash.set(signer.keyHash, signer);
      signers.push(signer);
    }
  });

  const preExistingKeyHashes = extractEnvelopeVKeyWitnesses(
    transaction.envelope
  ).map(({ keyHash }) => keyHash.toString('hex'));
  const requiredKeyHashes = proofs
    .filter(({ required }) => required)
    .map(({ credential }) => credential)
    .filter((value, index, values) => values.indexOf(value) === index);
  const requestedDeviceKeyHashes = signers
    .map(({ keyHash }) => keyHash)
    .filter((keyHash) => !preExistingKeyHashes.includes(keyHash));
  const missingKeyHashes = requiredKeyHashes.filter(
    (keyHash) =>
      !preExistingKeyHashes.includes(keyHash) &&
      !requestedDeviceKeyHashes.includes(keyHash)
  );
  const ownedOutputs = transaction.outputs
    .map(({ address }) =>
      outputBinding(address, snapshot.ownership, snapshot.network.networkId)
    )
    .filter((value): value is HardwareOwnedAddress => value !== null);
  const ownedInputs: HardwareOwnedInput[] = [];
  (['normal', 'collateral'] as const).forEach((role) => {
    transaction.inputs[role].forEach((input) => {
      const output = resolved.get(outpoint(input.transactionId, input.index));
      if (!output) return;
      const credential = paymentCredential(
        decodeConwayOutput(Buffer.from(output.sourceCbor, 'hex')).address,
        snapshot.network.networkId
      );
      const ownership =
        credential && ownedKey(snapshot.ownership, 'payment', credential);
      const proofKind = role === 'normal' ? 'normal_input' : 'collateral';
      if (!ownership?.proofKinds.includes(proofKind)) return;
      ownedInputs.push(
        Object.freeze({
          transactionId: input.transactionId,
          index: input.index,
          path: Object.freeze([...ownership.derivationPath]),
          role,
        })
      );
    });
  });
  const exact: HardwareExactTransaction = Object.freeze({
    transaction,
    bodyHash: transaction.transactionId,
    contextDigest: snapshot.contextDigest,
    network: snapshot.network,
    partialSign,
    signers: Object.freeze(signers),
    ownedInputs: Object.freeze(ownedInputs),
    ownedOutputs: Object.freeze(ownedOutputs),
    witnesses: Object.freeze({
      requiredKeyHashes: Object.freeze(requiredKeyHashes),
      preExistingKeyHashes: Object.freeze(preExistingKeyHashes),
      requestedDeviceKeyHashes: Object.freeze(requestedDeviceKeyHashes),
      missingKeyHashes: Object.freeze(missingKeyHashes),
      unexpectedKeyHashes: Object.freeze([]),
    }),
    capability,
  });

  if (reasons.length)
    return Object.freeze({
      status: 'rejected',
      deviceInteraction: false,
      reasons: Object.freeze(reasons),
      exact,
    });
  if (partialSign && requestedDeviceKeyHashes.length === 0)
    return Object.freeze({
      status: 'empty',
      deviceInteraction: false,
      witnessSetCbor: encodeVKeyWitnessSet([]).toString('hex'),
      exact,
    });
  if (!partialSign && requestedDeviceKeyHashes.length === 0) {
    if (missingKeyHashes.length)
      return Object.freeze({
        status: 'rejected',
        deviceInteraction: false,
        reasons: Object.freeze(['proof-generation']),
        exact,
      });
    return Object.freeze({
      status: 'empty',
      deviceInteraction: false,
      witnessSetCbor: encodeVKeyWitnessSet([]).toString('hex'),
      exact,
    });
  }

  const capabilityReasons = [
    ...(!capability.staticallyRepresentable ? ['not-representable'] : []),
    ...(!capability.staticGatesPassed ? ['static-gate'] : []),
    ...(!capability.physicalCertified ? ['physical-certification'] : []),
    ...(!capability.productEnabled ? ['product-disabled'] : []),
    ...Object.entries(capability.familyDispositions)
      .filter(([, disposition]) => disposition !== 'representable')
      .map(([family, disposition]) => `${family}:${disposition}`),
  ];
  if (capabilityReasons.length)
    return Object.freeze({
      status: 'rejected',
      deviceInteraction: false,
      reasons: Object.freeze(capabilityReasons),
      exact,
    });
  return Object.freeze({ status: 'ready', deviceInteraction: true, exact });
};

// Construction marks change; ownership alone must not hide a self-payment.
export const bindPaymentChange = async (
  exact: HardwareExactTransaction,
  outputs: readonly CoinSelectionOutput[],
  accountXpub: string
): Promise<HardwareExactTransaction> => {
  if (!/^[0-9a-f]{128}$/iu.test(accountXpub))
    throw new Error('Invalid change account public key');
  const derive = CachedDeriveXpubFactory(async () =>
    Buffer.from(accountXpub, 'hex')
  );
  const ownedOutputs: HardwareOwnedAddress[] = [];
  const remaining = new Set(exact.transaction.outputs.map((_, index) => index));
  for (const output of outputs) {
    if (
      !Number.isSafeInteger(output.amount.quantity) ||
      output.amount.quantity < 0
    )
      throw new Error('Invalid payment output amount');
    const assets = output.assets || [];
    if (
      assets.some(
        ({ quantity }) => !Number.isSafeInteger(quantity) || quantity < 0
      )
    )
      throw new Error('Invalid payment output asset quantity');
    const address = Buffer.from(
      utils.bech32_decodeAddress(output.address)
    ).toString('hex');
    const index = [...remaining].find((candidate) => {
      const actual = exact.transaction.outputs[candidate];
      return (
        actual.address === address &&
        actual.value.coin === BigInt(output.amount.quantity) &&
        actual.value.assets.length === assets.length &&
        actual.value.assets.every((asset) =>
          assets.some(
            (expected) =>
              expected.policyId === asset.policyId &&
              expected.assetName === asset.assetName &&
              BigInt(expected.quantity) === asset.quantity
          )
        )
      );
    });
    if (index === undefined)
      throw new Error('Payment output does not match exact transaction');
    remaining.delete(index);
    if (!output.derivationPath) continue;
    const path = output.derivationPath;
    if (
      path.length !== 5 ||
      path[0] !== '1852H' ||
      path[1] !== '1815H' ||
      path[2] !== '0H' ||
      path[3] !== '1' ||
      !/^(0|[1-9][0-9]*)$/u.test(path[4]) ||
      !Number.isSafeInteger(Number(path[4])) ||
      Number(path[4]) >= 0x80000000
    )
      throw new Error('Invalid payment change path');
    const paymentPath = [
      0x8000073c,
      0x80000717,
      0x80000000,
      1,
      Number(path[4]),
    ];
    const stakePath = [0x8000073c, 0x80000717, 0x80000000, 2, 0];
    const paymentKey = await derive(paymentPath, accountXpub);
    const stakeKey = await derive(stakePath, accountXpub);
    const derived = Buffer.concat([
      Buffer.from([exact.network.networkId]),
      Buffer.from(blake2b(paymentKey.subarray(0, 32), undefined, 28)),
      Buffer.from(blake2b(stakeKey.subarray(0, 32), undefined, 28)),
    ]).toString('hex');
    if (derived !== address)
      throw new Error('Change address does not belong to paired wallet');
    ownedOutputs.push(
      Object.freeze({
        address,
        outputIndex: index,
        paymentPath: Object.freeze(paymentPath),
        stakePath: Object.freeze(stakePath),
      })
    );
  }
  if (remaining.size) throw new Error('Unaccounted exact payment output');
  return Object.freeze({ ...exact, ownedOutputs: Object.freeze(ownedOutputs) });
};
