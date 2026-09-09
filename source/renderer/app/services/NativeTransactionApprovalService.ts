import { v4 as uuidv4 } from 'uuid';

import type {
  NativeApprovalResult,
  NativePreparedApproval,
} from '../../../common/transactions/nativePlan';
import { nativeApprovalBindingDigest } from '../../../common/transactions/nativePlan';
import { parseConwayTransactionEnvelope } from '../../../common/cardano/transactionEnvelope';
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
  markSubmitting: () => Promise<void>,
  requestId: string
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
  let submitting = false;
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
        submitting = true;
        await reportWalletApprovalProgress(request.requestId, 'submitting', 0);
      },
      request.requestId
    );
  } catch (error) {
    if (submitting) {
      const transactionIds: string[] = [];
      for (const item of attempt.prepared.items)
        if (item.kind === 'exact-cbor')
          transactionIds.push(
            parseConwayTransactionEnvelope(Buffer.from(item.cbor, 'hex'))
              .transactionId
          );
      return Object.freeze({
        status: 'submission-unknown',
        transactionIds: Object.freeze(transactionIds),
      });
    }
    let code: unknown;
    if (error && typeof error === 'object' && 'code' in error)
      code = error.code;
    else if (error instanceof Error) code = error.message;
    return Object.freeze({
      status: 'rejected',
      errorCode:
        code === 'wrong_encryption_passphrase' ||
        code === 'TxSignError.UserDeclined' ||
        code === 'TxSignError.ProofGeneration'
          ? code
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
