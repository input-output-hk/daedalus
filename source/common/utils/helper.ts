import { HARDENED } from '@cardano-foundation/ledgerjs-hw-app-cardano';

export const toJS = (object: any | null | undefined): any =>
  typeof object === 'object' ? JSON.parse(JSON.stringify(object)) : object;

export const parseBIP32Index = (str: string): number => {
  let base = 0;
  if (str.endsWith("'")) {
    str = str.slice(0, -1);
    base = HARDENED;
  }
  const i = parseInt(str, 10);
  if (i < 0 || i > HARDENED) throw new Error('Invalid path');
  return base + i;
};

export const bip32StrToPath = (str: string): number[] => {
  return str.split('/').map((i: string): number => {
    return parseBIP32Index(i);
  });
};
