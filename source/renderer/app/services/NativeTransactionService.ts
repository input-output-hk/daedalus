import type {
  NativeApprovalResult,
  NativePreparedApproval,
} from '../../../common/transactions/nativePlan';
import NativeTransactionApprovalService from './NativeTransactionApprovalService';

export type NativeTransactionOperation = Readonly<{
  walletId: string;
  ownerSignal: AbortSignal;
  payment?: Readonly<{ address: string; amount: string }>;
  prepare: () => Promise<NativePreparedApproval>;
  execute: (
    prepared: NativePreparedApproval,
    passphrase: string | undefined,
    signal: AbortSignal,
    markSubmitting: () => Promise<void>
  ) => Promise<NativeApprovalResult>;
}>;

type WalletSendLock = <T>(
  walletId: string,
  work: () => Promise<T>
) => Promise<T>;

export default class NativeTransactionService {
  private disposed = false;

  constructor(
    private readonly withWalletSendLock: WalletSendLock,
    private readonly approval = new NativeTransactionApprovalService(),
    private readonly onExecute?: (
      requestId: string,
      payment: NativeTransactionOperation['payment']
    ) => void
  ) {}

  async run(
    operation: NativeTransactionOperation
  ): Promise<NativeApprovalResult> {
    const cancelled = () =>
      Object.freeze({ status: 'rejected' as const, errorCode: 'cancelled' });
    if (this.disposed || operation.ownerSignal.aborted) return cancelled();
    return this.withWalletSendLock(operation.walletId, async () => {
      if (this.disposed || operation.ownerSignal.aborted) return cancelled();
      const prepared = await operation.prepare();
      if (
        this.disposed ||
        operation.ownerSignal.aborted ||
        prepared.walletId !== operation.walletId
      )
        return cancelled();
      return this.approval.request(
        prepared,
        (passphrase, signal, markSubmitting, requestId) => {
          if (signal.aborted) return Promise.resolve(cancelled());
          this.onExecute?.(requestId, operation.payment);
          return operation.execute(
            prepared,
            passphrase,
            signal,
            markSubmitting
          );
        },
        operation.ownerSignal
      );
    });
  }

  dispose(): void {
    this.disposed = true;
    this.approval.dispose();
  }
}
