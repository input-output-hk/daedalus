import { action, observable } from 'mobx';
import type {
  DappConsentPresentation,
  DappConsentRenderMainRequest,
  DappConsentRenderRendererResponse,
} from '../../../common/ipc/api';
import { bindDappConsentRenderer } from '../ipc/dappConsent';
import Store from './lib/Store';

export default class Cip30ConsentStore extends Store {
  @observable current: DappConsentPresentation | null = null;
  @observable deciding = false;

  private focusedElement: HTMLElement | null = null;
  private resolveDecision?: (
    decision: DappConsentRenderRendererResponse
  ) => void;
  private unbind?: () => void;

  setup(): void {
    this.unbind = bindDappConsentRenderer(this.receive);
  }

  teardown(): void {
    this.unbind?.();
    this.unbind = undefined;
    this.clear();
    super.teardown();
  }

  @action.bound
  receive(
    message: DappConsentRenderMainRequest
  ): Promise<DappConsentRenderRendererResponse> {
    if (message.type === 'present') {
      if (this.current) return Promise.resolve(undefined);
      this.focusedElement =
        document.activeElement instanceof HTMLElement
          ? document.activeElement
          : null;
      this.current = message.request;
      this.deciding = false;
      return new Promise((resolve) => {
        this.resolveDecision = resolve;
      });
    }
    if (this.current?.requestId === message.requestId) this.clear();
    return Promise.resolve(undefined);
  }

  @action.bound
  approve(passphrase?: string): void {
    this.decide(
      true,
      this.current?.kind === 'data-sign' ||
        this.current?.kind === 'transaction-sign' ||
        this.current?.kind === 'batch-sign'
        ? passphrase
        : undefined
    );
  }

  @action.bound
  reject(): void {
    this.decide(false);
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
    const target = this.focusedElement;
    this.focusedElement = null;
    if (target?.isConnected) target.focus();
  }
}
