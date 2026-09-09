import { action, computed, observable } from 'mobx';
import type {
  WalletApprovalPresentation,
  WalletApprovalProgressPhase,
  WalletApprovalRenderMainRequest,
  WalletApprovalRenderRendererResponse,
  WalletApprovalResult,
} from '../../../common/ipc/api';
import { bindWalletApprovalRenderer } from '../ipc/walletApproval';
import { cancelNativeTransactionApproval } from '../ipc/nativeTransactionApproval';
import NativeTransactionService from '../services/NativeTransactionService';
import type { NativeTransactionOperation } from '../services/NativeTransactionService';
import type { TransactionReceiptDetails } from '../components/transactions/TransactionApprovalDialog.types';
import type { TransactionReviewDisplay } from '../../../common/transactions/reviewDisplay';
import { logger } from '../utils/logging';
import Store from './lib/Store';

export default class WalletApprovalStore extends Store {
  @observable current: WalletApprovalPresentation | null = null;
  @observable.ref result: WalletApprovalResult | undefined;
  @observable deciding = false;
  @observable phase: 'ready' | WalletApprovalProgressPhase = 'ready';
  @observable activeItemIndex: number | undefined;
  @observable submissionAuthorized = false;
  @observable cancelling = false;
  nativeTransactions: NativeTransactionService;

  private focusedElement: HTMLElement | null = null;
  private resolveDecision?: (
    decision: WalletApprovalRenderRendererResponse
  ) => void;
  private resolveTerminal?: () => void;
  private terminalPromise?: Promise<WalletApprovalRenderRendererResponse>;
  private unbind?: () => void;
  private payment: NativeTransactionOperation['payment'];

  setup(): void {
    this.nativeTransactions = new NativeTransactionService(
      this.stores.transactions.withWalletSendLock,
      undefined,
      this.capturePayment
    );
    this.unbind = bindWalletApprovalRenderer(this.receive);
  }

  teardown(): void {
    this.nativeTransactions?.dispose();
    this.unbind?.();
    this.unbind = undefined;
    this.clear();
    super.teardown();
  }

  @action.bound
  receive(
    message: WalletApprovalRenderMainRequest
  ): Promise<WalletApprovalRenderRendererResponse> {
    switch (message.type) {
      case 'present':
        if (this.current) this.clear();
        this.focusedElement =
          document.activeElement instanceof HTMLElement
            ? document.activeElement
            : null;
        this.current = message.request;
        this.deciding = false;
        this.phase = 'ready';
        this.activeItemIndex = undefined;
        this.submissionAuthorized = false;
        this.cancelling = false;
        return new Promise((resolve) => {
          this.resolveDecision = resolve;
        });
      case 'progress':
        if (
          this.current?.requestId === message.requestId &&
          this.deciding &&
          this.validItemIndex(message.itemIndex)
        ) {
          this.phase = message.phase;
          this.activeItemIndex = message.itemIndex;
          this.submissionAuthorized = message.submissionAuthorized;
        }
        return Promise.resolve(undefined);
      case 'terminal':
        if (this.current?.requestId !== message.requestId)
          return Promise.resolve(undefined);
        if (this.result)
          return this.terminalPromise ?? Promise.resolve(undefined);
        if (
          this.current.kind === 'connection' ||
          this.current.kind === 'key-disclosure' ||
          this.current.kind === 'data-sign' ||
          message.result === undefined
        ) {
          this.clear();
          return Promise.resolve(undefined);
        }
        this.result = message.result;
        this.deciding = false;
        this.phase = 'ready';
        this.activeItemIndex = undefined;
        this.trackResult();
        this.terminalPromise = new Promise((resolve) => {
          this.resolveTerminal = resolve;
        });
        return this.terminalPromise;
      default:
        return Promise.resolve(undefined);
    }
  }

  @action.bound
  approve(passphrase?: string): void {
    const transaction =
      this.current && 'authorization' in this.current
        ? this.current
        : undefined;
    this.decide(
      true,
      (this.current?.kind === 'key-disclosure' &&
        this.current.requiresPassphrase === true) ||
        this.current?.kind === 'data-sign' ||
        transaction?.authorization.kind === 'software'
        ? passphrase
        : undefined
    );
  }

  @action.bound
  reject(): void {
    this.decide(false);
  }
  @action.bound
  dismissResult(): void {
    if (!this.result || !this.current) return;
    const { walletId } = this.current;
    if (this.isSubmissionRequest)
      this.stores.transactions.dismissReceipt(walletId, this.receiptIds);
    this.clear();
    this.actions.router.goToRoute.trigger({
      route: this.stores.wallets.getWalletRoute(walletId, 'transactions'),
    });
  }

  @action.bound
  viewTransaction(transactionId: string): void {
    if (
      !this.current ||
      !this.isSubmissionRequest ||
      !this.receiptIds.includes(transactionId)
    )
      return;
    const { walletId } = this.current;
    this.dismissResult();
    this.stores.transactions.openTransaction(walletId, transactionId);
  }

  private capturePayment = (
    requestId: string,
    payment: NativeTransactionOperation['payment']
  ): void => {
    if (this.current?.requestId === requestId) this.payment = payment;
  };
  private get isSubmissionRequest(): boolean {
    const request = this.current;
    if (!request) return false;
    return (
      request.kind === 'native-transaction' ||
      request.kind === 'transaction-submit' ||
      request.kind === 'batch-submit'
    );
  }

  private reviewedTransactions(): readonly {
    transactionId?: string;
    display: TransactionReviewDisplay;
  }[] {
    const request = this.current;
    if (!request) return [];
    if (request.kind === 'native-transaction')
      return request.items.map((item) =>
        item.kind === 'exact-cbor' ? item.review : { display: item.display }
      );

    if (
      request.kind === 'transaction-sign' ||
      request.kind === 'transaction-submit'
    )
      return [request.review];
    if (request.kind === 'batch-sign' || request.kind === 'batch-submit')
      return request.review.items.map((item) => item.transaction);
    return [];
  }

  @computed
  get receiptIds(): readonly string[] {
    if (!this.result) return [];
    if ('transactionIds' in this.result) return this.result.transactionIds;
    if (
      this.result.errorCode !== 'expired' &&
      this.result.errorCode !== 'rejected'
    )
      return [];
    return this.reviewedTransactions()
      .map(({ transactionId }) => transactionId)
      .filter((id): id is string => id !== undefined);
  }

  @computed
  get receiptDetails(): readonly TransactionReceiptDetails[] {
    if (!this.current || !this.result || !this.isSubmissionRequest) return [];
    const result = this.result;
    const walletId = this.current.walletId;
    return this.receiptIds.map((id) => {
      const transaction = this.stores.transactions.getTransaction(walletId, id);
      if (transaction)
        return {
          id,
          state: transaction.state,
          amount: transaction.amount,
          fee: transaction.fee,
          confirmations: transaction.confirmations,
          transferAmount: transaction.transferAmount,
          isSelfTransfer: transaction.isSelfTransfer,
          amountIsKnown: transaction.amountIsKnown,
        };
      if (result.status === 'submission-unknown')
        return { id, state: 'submission-unknown' as const };
      if (result.status === 'rejected')
        return {
          id,
          state:
            result.errorCode === 'expired'
              ? ('expired' as const)
              : ('failed' as const),
        };
      return { id, state: 'pending' as const };
    });
  }

  private trackResult(): void {
    if (!this.current || !this.result || !this.isSubmissionRequest) return;
    const walletId = this.current.walletId;
    const result = this.result;
    const reviewed = this.reviewedTransactions();
    let state: 'pending' | 'submission-unknown' | 'expired' | 'failed' =
      'pending';
    if (result.status === 'submission-unknown') state = 'submission-unknown';
    else if (result.status === 'rejected')
      state = result.errorCode === 'expired' ? 'expired' : 'failed';
    const pending = this.receiptIds.map((transactionId, index) => {
      const item =
        reviewed.find((value) => value.transactionId === transactionId) ||
        (this.current?.kind === 'native-transaction' &&
        reviewed[index]?.transactionId === undefined
          ? reviewed[index]
          : undefined);
      return this.stores.transactions.trackSubmission({
        walletId,
        transactionId,
        state,
        display: item?.display,
        payment: this.payment,
      });
    });
    Promise.all(pending).catch((error) => {
      logger.warn('Transaction receipt history could not be persisted', {
        error,
      });
    });
  }
  @action.bound
  async cancel(): Promise<void> {
    const current = this.current;
    if (
      current?.kind !== 'native-transaction' ||
      !this.deciding ||
      this.submissionAuthorized ||
      this.cancelling
    )
      return;
    this.cancelling = true;
    const response = await cancelNativeTransactionApproval(
      current.attemptId,
      current.requestId
    );
    if (response.status === 'submission-authorized')
      this.submissionAuthorized = true;
  }

  private decide(approved: boolean, passphrase?: string): void {
    if (!this.current || this.deciding || !this.resolveDecision) return;
    this.deciding = true;
    const resolve = this.resolveDecision;
    this.resolveDecision = undefined;
    resolve({
      requestId: this.current.requestId,
      approved,
      ...(approved && passphrase ? { passphrase } : {}),
    });
  }

  private validItemIndex(index?: number): boolean {
    if (index === undefined) return true;
    if (!this.current) return false;
    if (
      this.current.kind === 'batch-sign' ||
      this.current.kind === 'batch-submit'
    )
      return index < this.current.review.items.length;
    return index === 0;
  }

  @action
  private clear(): void {
    if (this.current && this.resolveDecision)
      this.resolveDecision({
        requestId: this.current.requestId,
        approved: false,
      });
    this.resolveDecision = undefined;
    this.result = undefined;
    this.payment = undefined;
    const resolveTerminal = this.resolveTerminal;
    this.resolveTerminal = undefined;
    this.terminalPromise = undefined;
    resolveTerminal?.();
    this.current = null;
    this.deciding = false;
    this.phase = 'ready';
    this.activeItemIndex = undefined;
    this.submissionAuthorized = false;
    this.cancelling = false;
    const target = this.focusedElement;
    this.focusedElement = null;
    if (target?.isConnected) target.focus();
  }
}
