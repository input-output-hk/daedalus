import type { RequestConfig } from '../../common/types';
import type {
  CreateExternalTransactionResponse,
  CreateExternalTransactionRequest,
} from '../types';
import { request } from '../../utils/request';

export const createExternalTransaction = (
  config: RequestConfig,
  { signedTransactionBlob }: CreateExternalTransactionRequest
): Promise<CreateExternalTransactionResponse> =>
  request(
    {
      method: 'POST',
      path: '/v2/proxy/transactions',
      ...config,
    },
    {},
    signedTransactionBlob,
    {
      isOctetStreamRequest: true,
    }
  );
