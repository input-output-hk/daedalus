import { action, observable } from 'mobx';
import type {
  WalletApprovalPresentation,
  WalletApprovalProgressPhase,
  WalletApprovalRenderMainRequest,
  WalletApprovalRenderRendererResponse,
} from '../../../common/ipc/api';
import { bindWalletApprovalRenderer } from '../ipc/walletApproval';
import { cancelNativeTransactionApproval } from '../ipc/nativeTransactionApproval';
import NativeTransactionService from '../services/NativeTransactionService';
import Store from './lib/Store';

export default class WalletApprovalStore extends Store {
  @observable current: WalletApprovalPresentation | null = null;
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
  private unbind?: () => void;

  setup(): void {
    this.nativeTransactions = new NativeTransactionService(
      this.stores.transactions.withWalletSendLock
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
        if (this.current) return Promise.resolve(undefined);
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
        if (this.current?.requestId === message.requestId) this.clear();
        return Promise.resolve(undefined);
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
