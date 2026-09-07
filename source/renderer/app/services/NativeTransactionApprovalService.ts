import { v4 as uuidv4 } from 'uuid';

import type {
  NativeApprovalResult,
  NativePreparedApproval,
} from '../../../common/transactions/nativePlan';
import { nativeApprovalBindingDigest } from '../../../common/transactions/nativePlan';
import {
  bindNativeTransactionExecutor,
  cancelNativeTransactionApproval,
  commitNativeTransactionSubmission,
  reportWalletApprovalProgress,
  requestNativeTransactionApproval,
} from '../ipc/nativeTransactionApproval';

type Execute = (
  passphrase: string | undefined,
  signal: AbortSignal,
  markSubmitting: () => Promise<void>
) => Promise<NativeApprovalResult>;
type Attempt = {
  prepared: NativePreparedApproval;
  execute: Execute;
  abort: AbortController;
};
const attempts = new Map<string, Attempt>();

const executeNativeTransaction = async (
  request: Parameters<Parameters<typeof bindNativeTransactionExecutor>[0]>[0]
): Promise<NativeApprovalResult> => {
  const attempt = attempts.get(request.attemptId);
  if (!attempt)
    return Object.freeze({ status: 'rejected', errorCode: 'cancelled' });
  if (request.type === 'cancel') {
    attempt.abort.abort();
    return Object.freeze({ status: 'rejected', errorCode: 'cancelled' });
  }
  if (
    request.bindingDigest !== nativeApprovalBindingDigest(attempt.prepared) ||
    attempt.abort.signal.aborted
  )
    return Object.freeze({
      status: 'rejected',
      errorCode: 'transaction_plan_changed',
    });
  await reportWalletApprovalProgress(
    request.requestId,
    attempt.prepared.authorization === 'software'
      ? 'signing'
      : 'waiting-for-device'
  );
  try {
    return await attempt.execute(
      request.passphrase,
      attempt.abort.signal,
      async () => {
        const committed = await commitNativeTransactionSubmission(
          request.attemptId,
          request.requestId
        );
        if (committed.status !== 'submission-authorized')
          throw new Error('Native transaction submission was cancelled');
        await reportWalletApprovalProgress(request.requestId, 'submitting', 0);
      }
    );
  } catch (error) {
    return Object.freeze({
      status: 'rejected',
      errorCode:
        error &&
        typeof error === 'object' &&
        'code' in error &&
        error.code === 'wrong_encryption_passphrase'
          ? 'wrong_encryption_passphrase'
          : 'failed',
    });
  }
};
let unbindExecutor: (() => void) | undefined;
const registerExecutor = (): void => {
  if (unbindExecutor) return;
  unbindExecutor = bindNativeTransactionExecutor(executeNativeTransaction);
};

export default class NativeTransactionApprovalService {
  async request(
    prepared: NativePreparedApproval,
    execute: Execute,
    ownerSignal: AbortSignal
  ): Promise<NativeApprovalResult> {
    registerExecutor();
    while (!ownerSignal.aborted) {
      const result = await this.requestAttempt(prepared, execute, ownerSignal);
      if (
        result.status !== 'rejected' ||
        result.errorCode !== 'wrong_encryption_passphrase'
      )
        return result;
    }
    return Object.freeze({ status: 'rejected', errorCode: 'cancelled' });
  }

  private async requestAttempt(
    prepared: NativePreparedApproval,
    execute: Execute,
    ownerSignal: AbortSignal
  ): Promise<NativeApprovalResult> {
    const attemptId = uuidv4();
    const abort = new AbortController();
    const attempt = { prepared, execute, abort };
    attempts.set(attemptId, attempt);
    const cancel = () => {
      abort.abort();
      cancelNativeTransactionApproval(attemptId).catch(() => undefined);
    };
    ownerSignal.addEventListener('abort', cancel, { once: true });
    if (ownerSignal.aborted) cancel();
    try {
      const response = await requestNativeTransactionApproval(
        attemptId,
        prepared
      );
      if (
        response.bindingDigest !== nativeApprovalBindingDigest(prepared) ||
        !response.result
      )
        return Object.freeze({ status: 'rejected', errorCode: 'failed' });
      return response.result;
    } finally {
      ownerSignal.removeEventListener('abort', cancel);
      abort.abort();
      attempts.delete(attemptId);
    }
  }

  dispose(): void {
    attempts.forEach(({ abort }) => abort.abort());
    attempts.clear();
    unbindExecutor?.();
    unbindExecutor = undefined;
  }
}
