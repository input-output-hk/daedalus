import type { SemanticTransaction } from '../cardano/transaction';
import type { ExactTransactionEnvelope } from '../cardano/transactionEnvelope';
import type {
  Cip103SubmitError as PublicCip103SubmitError,
  TxSignError,
} from '../cip30/errors';

export type TransactionSignatureRequest = Readonly<{
  cbor: string;
  partialSign?: boolean;
}>;

export type Cip103Operation = 'sign' | 'submit';

export type Cip103PreflightItem = Readonly<{
  index: number;
  cbor: string;
  fullCborDigest: string;
  bodyHash: string;
  partialSign?: boolean;
  envelope: ExactTransactionEnvelope;
  transaction: SemanticTransaction;
}>;

export type Cip103PreflightBatch = Readonly<{
  state: 'preflighted';
  operation: Cip103Operation;
  items: readonly Cip103PreflightItem[];
}>;

export type Cip103SignResult = readonly string[];
export type Cip103SignError = TxSignError;
export type Cip103SubmitResult = readonly string[];
export type Cip103SubmitError = Readonly<PublicCip103SubmitError>;

export const formatCip103FailureInfo = (index: number): string =>
  `Transaction at index ${index} failed`;
