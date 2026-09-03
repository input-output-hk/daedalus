import { bech32 } from 'bech32';

export const encodeCip104AccountPub = (accountKey: string): string => {
  const decoded = bech32.decode(accountKey, 1000);
  const bytes = Buffer.from(bech32.fromWords(decoded.words));
  if (decoded.prefix !== 'acct_xvk' || bytes.length !== 64)
    throw new Error('Invalid CIP-104 account public key');
  return `5840${bytes.toString('hex')}`;
};
