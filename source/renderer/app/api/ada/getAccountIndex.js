import { remote } from 'electron';
import { getAdaWalletAccountsV1 } from './getAdaWalletAccountsV1';

const ca = remote.getGlobal('ca');

export default async (walletId: string) => {
  const accounts = await getAdaWalletAccountsV1({ ca, walletId });

  if (!accounts || !accounts.length) {
    throw new Error('Active account is required.');
  }

  return accounts[0].index;
};
