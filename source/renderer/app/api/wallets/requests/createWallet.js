// @flow
import type { RequestConfig } from '../../common/types';
import type { AdaWallet } from '../types';
import { request } from '../../utils/request';

export type WalletInitData = {
  operation: 'create' | 'restore',
  backupPhrase: [string],
  name: string,
  spendingPassword?: string,
};

export const createWallet = (
  config: RequestConfig,
  { walletInitData }: { walletInitData: WalletInitData }
): Promise<AdaWallet> =>
  request(
    {
      method: 'POST',
      path: '/api/v1/wallets',
      ...config,
    },
    {},
    walletInitData
  );
