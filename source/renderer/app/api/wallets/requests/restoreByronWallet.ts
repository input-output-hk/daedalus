import type { RequestConfig } from '../../common/types';
import type { LegacyAdaWallet, LegacyWalletInitData } from '../types';
import type { WalletByronKind } from '../../../types/walletRestoreTypes';
import { request } from '../../utils/request';

export const restoreByronWallet = (
  config: RequestConfig,
  {
    walletInitData,
  }: {
    walletInitData: LegacyWalletInitData;
  },
  type: WalletByronKind
): Promise<LegacyAdaWallet> =>
  request(
    {
      method: 'POST',
      path: '/v2/byron-wallets',
      ...config,
    },
    {},
    { ...walletInitData, style: type }
  );
