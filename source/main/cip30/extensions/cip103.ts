import type {
  Cip30WalletNetwork,
  Cip30WalletRequest,
  Cip30WalletResponse,
} from '../../../common/cip30/executor';
import { diffVKeyWitnesses } from '../../../common/cardano/witnessSet';
import type {
  Cip103PreflightBatch,
  Cip103SignResult,
} from '../../../common/types/cip103.types';
import type {
  HardwareExactTransaction,
  HardwareTransactionPreparation,
} from '../../../common/types/hardware-wallets.types';

type ExecuteWallet = (
  _request: Cip30WalletRequest
) => Promise<Cip30WalletResponse>;

export type Cip103SoftwareSigningFailure =
  | 'account-change'
  | 'unavailable'
  | 'internal'
  | 'tx-proof-generation'
  | 'deprecated-certificate';

export class Cip103SoftwareSigningError extends Error {
  public constructor(
    public readonly failure: Cip103SoftwareSigningFailure,
    public readonly transactionIndex?: number
  ) {
    super(failure);
    this.name = 'Cip103SoftwareSigningError';
  }
}

export type Cip103SigningReview = Readonly<{
  mode: 'sign' | 'submit';
  approvable: boolean;
  items: readonly Readonly<{
    index: number;
    transaction: Readonly<{
      transactionId: string;
      fullCborDigest: string;
    }>;
  }>[];
}>;

export type Cip103SoftwareSigningRequest = Readonly<{
  walletId: string;
  network: Cip30WalletNetwork;
  sourceRevision: string;
  batch: Cip103PreflightBatch;
  review: Cip103SigningReview;
  signingContext: unknown;
  passphrase: string;
  requiredKeyHashes?: readonly (readonly string[])[];
}>;

const internal = (transactionIndex?: number): never => {
  throw new Cip103SoftwareSigningError('internal', transactionIndex);
};

const validKeyHash = (value: string): boolean => /^[0-9a-f]{56}$/u.test(value);
const reviewedBatch = (
  batch: Cip103PreflightBatch,
  review: Cip103SigningReview
): boolean =>
  batch.operation === 'sign' &&
  batch.items.length >= 1 &&
  batch.items.length <= 50 &&
  review.mode === 'sign' &&
  review.approvable &&
  review.items.length === batch.items.length &&
  review.items.every(
    (item, index) =>
      item.index === index &&
      item.transaction.transactionId === batch.items[index]?.bodyHash &&
      item.transaction.fullCborDigest === batch.items[index]?.fullCborDigest
  );

export const signCip103SoftwareBatch = async (
  executeWallet: ExecuteWallet,
  request: Cip103SoftwareSigningRequest
): Promise<Cip103SignResult> => {
  const { batch, passphrase, requiredKeyHashes, review } = request;
  if (
    !reviewedBatch(batch, review) ||
    !passphrase ||
    (requiredKeyHashes && requiredKeyHashes.length !== batch.items.length) ||
    requiredKeyHashes?.some((hashes) =>
      hashes.some((hash) => !validKeyHash(hash))
    )
  )
    internal();

  let response: Cip30WalletResponse;
  try {
    response = await executeWallet({
      operation: 'sign-transactions',
      walletId: request.walletId,
      network: request.network,
      sourceRevision: request.sourceRevision,
      context: request.signingContext,
      transactions: Object.freeze(
        batch.items.map(({ cbor, partialSign }) =>
          Object.freeze({ cbor, partialSign: partialSign ?? false })
        )
      ),
      passphrase,
    });
  } catch {
    internal();
  }

  if (response.status !== 'fulfilled') {
    if (
      response.reason === 'account-change' ||
      response.reason === 'unavailable' ||
      response.reason === 'tx-proof-generation' ||
      response.reason === 'deprecated-certificate'
    )
      throw new Cip103SoftwareSigningError(response.reason);
    throw new Cip103SoftwareSigningError('internal');
  }
  if (response.operation !== 'sign-transactions')
    throw new Cip103SoftwareSigningError('internal');
  if (
    response.value.revision !== 1 ||
    response.value.witnesses.length !== batch.items.length
  )
    throw new Cip103SoftwareSigningError('internal');

  const staged: string[] = [];
  for (const [index, item] of batch.items.entries()) {
    const witness = response.value.witnesses[index];
    if (!witness || witness.transaction_index !== index) internal(index);
    try {
      staged.push(
        diffVKeyWitnesses(
          item.envelope,
          witness.body_hash,
          Buffer.from(witness.witness_set_cbor, 'hex'),
          requiredKeyHashes?.[index] || []
        ).toString('hex')
      );
    } catch {
      internal(index);
    }
  }

  return Object.freeze(staged);
};

export type Cip103HardwareSigningFailure =
  | 'cancelled'
  | 'user-declined'
  | 'proof-generation'
  | 'internal';

export class Cip103HardwareSigningError extends Error {
  public constructor(
    public readonly failure: Cip103HardwareSigningFailure,
    public readonly transactionIndex: number
  ) {
    super(failure);
    this.name = 'Cip103HardwareSigningError';
  }
}

export type Cip103HardwareSigningRequest = Readonly<{
  batch: Cip103PreflightBatch;
  review: Cip103SigningReview;
  signal: AbortSignal;
  prepare: (_index: number) => HardwareTransactionPreparation;
  signTransaction: (
    _exact: HardwareExactTransaction,
    _index: number
  ) => Promise<string>;
  cancelDevice: () => Promise<void> | void;
}>;

const hardwareFailure = (
  failure: Cip103HardwareSigningFailure,
  transactionIndex: number
): never => {
  throw new Cip103HardwareSigningError(failure, transactionIndex);
};

const exactPreparation = (
  preparation: HardwareTransactionPreparation,
  item: Cip103PreflightBatch['items'][number]
): boolean =>
  preparation.exact.bodyHash === item.bodyHash &&
  preparation.exact.transaction.transactionId === item.bodyHash &&
  preparation.exact.transaction.envelope.cbor.toString('hex') === item.cbor &&
  preparation.exact.partialSign === (item.partialSign ?? false);

export const signCip103HardwareBatch = async (
  request: Cip103HardwareSigningRequest
): Promise<Cip103SignResult> => {
  const { batch, review, signal } = request;
  if (!reviewedBatch(batch, review)) hardwareFailure('internal', 0);
  let cancelStarted = false;
  const cancel = async (transactionIndex: number): Promise<never> => {
    if (!cancelStarted) {
      cancelStarted = true;
      try {
        await request.cancelDevice();
      } catch {
        // Result suppression is authoritative even when transport cleanup fails.
      }
    }
    return hardwareFailure('cancelled', transactionIndex);
  };
  if (signal.aborted) await cancel(0);

  let preparations: HardwareTransactionPreparation[];
  try {
    preparations = batch.items.map((_item, index) => request.prepare(index));
  } catch {
    hardwareFailure('internal', 0);
  }
  if (signal.aborted) await cancel(0);
  const contextDigest = preparations[0]?.exact.contextDigest;
  for (const [index, preparation] of preparations.entries()) {
    if (
      !exactPreparation(preparation, batch.items[index]) ||
      preparation.exact.contextDigest !== contextDigest
    )
      hardwareFailure('internal', index);
    if (preparation.status === 'rejected')
      hardwareFailure('proof-generation', index);
    if (preparation.status === 'empty' && preparation.witnessSetCbor !== 'a0')
      hardwareFailure('internal', index);
  }

  const staged: string[] = [];
  for (const [index, preparation] of preparations.entries()) {
    if (signal.aborted) await cancel(index);
    let witnessSetCbor: string;
    if (preparation.status === 'empty') {
      witnessSetCbor = preparation.witnessSetCbor;
    } else if (preparation.status === 'ready') {
      try {
        witnessSetCbor = await request.signTransaction(
          preparation.exact,
          index
        );
      } catch (error) {
        if (signal.aborted) await cancel(index);
        const code =
          error && typeof error === 'object' && 'code' in error
            ? error.code
            : undefined;
        if (code === 'TxSignError.UserDeclined')
          hardwareFailure('user-declined', index);
        if (code === 'TxSignError.ProofGeneration')
          hardwareFailure('proof-generation', index);
        hardwareFailure('internal', index);
      }
      if (signal.aborted) await cancel(index);
    } else {
      hardwareFailure('proof-generation', index);
    }

    try {
      staged.push(
        diffVKeyWitnesses(
          batch.items[index].envelope,
          batch.items[index].bodyHash,
          Buffer.from(witnessSetCbor, 'hex'),
          preparation.exact.partialSign
            ? preparation.exact.witnesses.requestedDeviceKeyHashes
            : preparation.exact.witnesses.requiredKeyHashes
        ).toString('hex')
      );
    } catch {
      hardwareFailure('internal', index);
    }
  }
  if (signal.aborted) await cancel(preparations.length - 1);
  return Object.freeze(staged);
};
