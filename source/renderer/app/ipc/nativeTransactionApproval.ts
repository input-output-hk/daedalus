import {
  NATIVE_TRANSACTION_EXECUTE_CHANNEL,
  WALLET_TRANSACTION_APPROVAL_CHANNEL,
} from '../../../common/ipc/api';
import type {
  NativeTransactionExecuteMainRequest,
  NativeTransactionExecuteRendererResponse,
  WalletApprovalProgressPhase,
  WalletTransactionApprovalMainResponse,
  WalletTransactionApprovalRendererRequest,
} from '../../../common/ipc/api';
import type {
  NativeApprovalResult,
  NativePreparedApproval,
} from '../../../common/transactions/nativePlan';
import { RendererIpcChannel } from './lib/RendererIpcChannel';

const channel = new RendererIpcChannel<
  WalletTransactionApprovalMainResponse,
  WalletTransactionApprovalRendererRequest
>(WALLET_TRANSACTION_APPROVAL_CHANNEL);
const executor = new RendererIpcChannel<
  NativeTransactionExecuteMainRequest,
  NativeTransactionExecuteRendererResponse
>(NATIVE_TRANSACTION_EXECUTE_CHANNEL);

export const reportWalletApprovalProgress = (
  requestId: string,
  phase: WalletApprovalProgressPhase,
  itemIndex?: number
): Promise<WalletTransactionApprovalMainResponse> =>
  channel.request({
    type: 'progress',
    requestId,
    phase,
    ...(itemIndex === undefined ? {} : { itemIndex }),
  });

export const requestNativeTransactionApproval = (
  attemptId: string,
  prepared: NativePreparedApproval
): Promise<WalletTransactionApprovalMainResponse> =>
  channel.request({
    type: 'request-native',
    attemptId,
    walletId: prepared.walletId,
    network: prepared.network,
    prepared,
  });

export const cancelNativeTransactionApproval = (
  attemptId: string,
  requestId?: string
): Promise<WalletTransactionApprovalMainResponse> =>
  channel.request({
    type: 'cancel-native',
    attemptId,
    ...(requestId === undefined ? {} : { requestId }),
  });

export const commitNativeTransactionSubmission = (
  attemptId: string,
  requestId: string
): Promise<WalletTransactionApprovalMainResponse> =>
  channel.request({
    type: 'commit-native-submission',
    attemptId,
    requestId,
  });

let execute:
  | ((
      request: NativeTransactionExecuteMainRequest
    ) => Promise<NativeApprovalResult>)
  | undefined;
let registered = false;
export const bindNativeTransactionExecutor = (
  next: (
    request: NativeTransactionExecuteMainRequest
  ) => Promise<NativeApprovalResult>
): (() => void) => {
  execute = next;
  if (!registered) {
    registered = true;
    executor.onRequest(async (request) => {
      if (!execute)
        return Object.freeze({ status: 'rejected', errorCode: 'cancelled' });
      return execute(request);
    });
  }
  return () => {
    if (execute === next) execute = undefined;
  };
};
