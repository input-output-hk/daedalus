// @flow
export type ExtractedWallet = {
  index: number,
  raw: Array<Buffer>,
  passwordHash: string,
  password: ?string,
  balance: ?string,
  imported: boolean,
  id: ?string,
};

export type ExtractedWallets = Array<ExtractedWallet>;
