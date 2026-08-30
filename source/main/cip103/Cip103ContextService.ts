import type { TransactionContextSnapshot } from '../../common/cardano/transactionContext';
import {
  Cip103OverlayError,
  Cip103Resolution,
  resolveCip103TransactionOverlay,
} from '../../common/cardano/transactionOverlay';
import {
  Cip103BatchReview,
  createCip103BatchReview,
} from '../../common/cip30/cip103Review';
import type { Cip103PreflightBatch } from '../../common/types/cip103.types';
import {
  DappContextBinding,
  DappContextFailure,
  DappTransactionContextService,
  DappTransactionContextServiceError,
} from '../cardano/DappTransactionContextService';

export type Cip103ContextFailure = DappContextFailure | 'resolution_failed';

export class Cip103ContextError extends Error {
  public constructor(
    public readonly failure: Cip103ContextFailure,
    public readonly transactionIndex?: number
  ) {
    super(failure);
    this.name = 'Cip103ContextError';
  }
}

export type Cip103ResolvedBatch = Readonly<{
  state: 'context-resolved';
  operation: 'sign' | 'submit';
  snapshot: TransactionContextSnapshot;
  resolution: Cip103Resolution;
  review: Cip103BatchReview;
}>;

type ContextCapture = Pick<DappTransactionContextService, 'capture'>;

export class Cip103ContextService {
  public constructor(private readonly context: ContextCapture) {}

  public async capture(
    binding: DappContextBinding,
    batch: Cip103PreflightBatch
  ): Promise<Cip103ResolvedBatch> {
    const transactions = Object.freeze(batch.items.map(({ cbor }) => cbor));
    try {
      const snapshot = await this.context.capture(binding, transactions);
      if (
        snapshot.transactions.length !== transactions.length ||
        snapshot.transactions.some(
          (transaction, index) => transaction !== transactions[index]
        )
      )
        throw new Cip103ContextError('internal_error');
      const resolution = resolveCip103TransactionOverlay(batch.items, snapshot);
      const review = createCip103BatchReview(
        batch,
        resolution,
        snapshot.transactionsSemantic
      );
      return Object.freeze({
        state: 'context-resolved',
        operation: batch.operation,
        snapshot,
        resolution,
        review,
      });
    } catch (error) {
      if (error instanceof Cip103OverlayError)
        throw new Cip103ContextError(
          'resolution_failed',
          error.transactionIndex
        );
      if (error instanceof Cip103ContextError) throw error;
      if (error instanceof DappTransactionContextServiceError)
        throw new Cip103ContextError(error.failure);
      throw new Cip103ContextError('internal_error');
    }
  }
}
