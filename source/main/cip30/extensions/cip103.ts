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

export type Cip103SoftwareSigningReview = Readonly<{
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
  review: Cip103SoftwareSigningReview;
  signingContext: unknown;
  passphrase: string;
  requiredKeyHashes?: readonly (readonly string[])[];
}>;

const internal = (transactionIndex?: number): never => {
  throw new Cip103SoftwareSigningError('internal', transactionIndex);
};

const validKeyHash = (value: string): boolean => /^[0-9a-f]{56}$/u.test(value);

export const signCip103SoftwareBatch = async (
  executeWallet: ExecuteWallet,
  request: Cip103SoftwareSigningRequest
): Promise<Cip103SignResult> => {
  const { batch, passphrase, requiredKeyHashes, review } = request;
  if (
    batch.operation !== 'sign' ||
    batch.items.length < 1 ||
    batch.items.length > 50 ||
    review.mode !== 'sign' ||
    !review.approvable ||
    review.items.length !== batch.items.length ||
    review.items.some(
      (item, index) =>
        item.index !== index ||
        item.transaction.transactionId !== batch.items[index]?.bodyHash ||
        item.transaction.fullCborDigest !== batch.items[index]?.fullCborDigest
    ) ||
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
