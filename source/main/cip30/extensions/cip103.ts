import type {
  Cip30WalletNetwork,
  Cip30WalletRequest,
  Cip30WalletResponse,
  Cip30WalletSubmissionResponse,
} from '../../../common/cip30/executor';
import { diffVKeyWitnesses } from '../../../common/cardano/witnessSet';
import type {
  Cip103Operation,
  Cip103PreflightBatch,
  Cip103SignResult,
  Cip103SubmitResult,
} from '../../../common/types/cip103.types';
import type {
  HardwareExactTransaction,
  HardwareTransactionPreparation,
} from '../../../common/types/hardware-wallets.types';
import type {
  Cip103SubmitError,
  TxSendError,
} from '../../../common/cip30/errors';

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

export type Cip103ExecutionReview = Readonly<{
  mode: 'sign' | 'submit';
  approvable: boolean;
  items: readonly Readonly<{
    index: number;
    transaction: Readonly<{
      transactionId: string;
      fullCborDigest: string;
      fullCbor: string;
    }>;
  }>[];
}>;

export type Cip103WalletSigningRequest = Readonly<{
  walletId: string;
  walletKind: 'shelley-software' | 'ledger' | 'trezor';
  network: Cip30WalletNetwork;
  sourceRevision: string;
  batch: Cip103PreflightBatch;
  review: Cip103ExecutionReview;
  signingContext: unknown;
  passphrase?: string;
  requiredKeyHashes?: readonly (readonly string[])[];
  allowedKeyHashes?: readonly (readonly string[])[];
}>;

const internal = (transactionIndex?: number): never => {
  throw new Cip103SoftwareSigningError('internal', transactionIndex);
};

const validKeyHash = (value: string): boolean => /^[0-9a-f]{56}$/u.test(value);
const reviewedBatch = (
  batch: Cip103PreflightBatch,
  review: Cip103ExecutionReview,
  operation: Cip103Operation
): boolean =>
  batch.operation === operation &&
  batch.items.length >= 1 &&
  batch.items.length <= 50 &&
  review.mode === operation &&
  review.approvable &&
  review.items.length === batch.items.length &&
  review.items.every(
    (item, index) =>
      item.index === index &&
      item.transaction.transactionId === batch.items[index]?.bodyHash &&
      item.transaction.fullCborDigest === batch.items[index]?.fullCborDigest &&
      item.transaction.fullCbor === batch.items[index]?.cbor
  );

export const signCip103WalletBatch = async (
  executeWallet: ExecuteWallet,
  request: Cip103WalletSigningRequest
): Promise<Cip103SignResult> => {
  const {
    allowedKeyHashes,
    batch,
    passphrase,
    requiredKeyHashes,
    review,
    walletKind,
  } = request;
  if (
    !reviewedBatch(batch, review, 'sign') ||
    (walletKind === 'shelley-software' && !passphrase) ||
    (requiredKeyHashes && requiredKeyHashes.length !== batch.items.length) ||
    (allowedKeyHashes && allowedKeyHashes.length !== batch.items.length) ||
    [...(requiredKeyHashes || []), ...(allowedKeyHashes || [])].some((hashes) =>
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
      ...(walletKind === 'shelley-software' ? { passphrase } : {}),
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
          requiredKeyHashes?.[index] || [],
          allowedKeyHashes?.[index]
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
  review: Cip103ExecutionReview;
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
  if (!reviewedBatch(batch, review, 'sign')) hardwareFailure('internal', 0);
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

export type Cip103BatchSubmissionRequest = Readonly<{
  batch: Cip103PreflightBatch;
  review: Cip103ExecutionReview;
  submitTransaction: (
    _cbor: string,
    _index: number
  ) => Promise<Cip30WalletSubmissionResponse>;
}>;

const failedSubmission = (): TxSendError =>
  Object.freeze({ code: 2, info: 'Transaction submission failed' });

const submissionError = (error: unknown): TxSendError => {
  if (
    error &&
    typeof error === 'object' &&
    'code' in error &&
    'info' in error
  ) {
    const { code, info } = error;
    if (
      (code === 1 || code === 2) &&
      typeof info === 'string' &&
      info.length > 0
    )
      return Object.freeze({ code, info });
  }
  return failedSubmission();
};

export const submitCip103Batch = async (
  request: Cip103BatchSubmissionRequest
): Promise<Cip103SubmitResult> => {
  const { batch, review } = request;
  if (!reviewedBatch(batch, review, 'submit')) {
    const rejection: Cip103SubmitError = batch.items.map(failedSubmission);
    // Public CIP-103 rejects with the aligned array itself, never an Error.
    // eslint-disable-next-line no-throw-literal
    throw rejection;
  }

  const results: Array<string | TxSendError> = [];
  const hashes: string[] = [];
  // Guest lifecycle is deliberately absent: authorization owns this ordered loop.
  for (const [index, item] of batch.items.entries()) {
    try {
      const response = await request.submitTransaction(item.cbor, index);
      if (
        response.revision !== 1 ||
        response.transaction_id !== item.bodyHash ||
        response.status === 'rejected' ||
        response.status === 'expired'
      ) {
        results.push(failedSubmission());
      } else {
        results.push(item.bodyHash);
        hashes.push(item.bodyHash);
      }
    } catch (error) {
      results.push(submissionError(error));
    }
  }

  if (results.some((result) => typeof result !== 'string')) {
    // Public CIP-103 rejects with the aligned array itself, never an Error.
    // eslint-disable-next-line no-throw-literal
    throw results;
  }
  return Object.freeze(hashes);
};
