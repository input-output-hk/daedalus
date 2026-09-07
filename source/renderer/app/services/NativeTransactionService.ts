import type {
  NativeApprovalResult,
  NativePreparedApproval,
} from '../../../common/transactions/nativePlan';
import NativeTransactionApprovalService from './NativeTransactionApprovalService';

export type NativeTransactionOperation = Readonly<{
  walletId: string;
  ownerSignal: AbortSignal;
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
  constructor(
    private readonly withWalletSendLock: WalletSendLock,
    private readonly approval = new NativeTransactionApprovalService()
  ) {}

  async run(
    operation: NativeTransactionOperation
  ): Promise<NativeApprovalResult> {
    const prepared = await this.withWalletSendLock(
      operation.walletId,
      operation.prepare
    );
    if (
      prepared.walletId !== operation.walletId ||
      operation.ownerSignal.aborted
    )
      return Object.freeze({ status: 'rejected', errorCode: 'cancelled' });
    return this.approval.request(
      prepared,
      (passphrase, signal, markSubmitting) =>
        this.withWalletSendLock(operation.walletId, () =>
          operation.execute(prepared, passphrase, signal, markSubmitting)
        ),
      operation.ownerSignal
    );
  }

  dispose(): void {
    this.approval.dispose();
  }
}
