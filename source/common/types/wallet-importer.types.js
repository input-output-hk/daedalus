// @flow
export type ExtractedWallet = {
  index: number,
  encryptedSecretKey: string,
  passwordHash: string,
  password: ?string,
  balance: ?string,
  imported: boolean,
  id: ?string,
};

export type ExtractedWallets = Array<ExtractedWallet>;
