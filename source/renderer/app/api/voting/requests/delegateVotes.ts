import { request } from '../../utils/request';
import { RequestConfig } from '../../common/types';
import { Transaction } from '../../transactions/types';
import { DelegateVotesParams } from '../types';

export const delegateVotes = (
  config: RequestConfig,
  { dRepId, passphrase, walletId }: DelegateVotesParams
): Promise<Transaction> =>
  request(
    {
      ...config,
      method: 'PUT',
      path: `/v2/dreps/${dRepId}/wallets/${walletId}`,
    },
    {},
    { passphrase }
  );
