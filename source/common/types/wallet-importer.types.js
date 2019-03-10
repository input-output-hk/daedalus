// @flow
export type ExtractedWallet = {
  index: number,
  raw: Array<Buffer>,
  passwordHash: string,
  password: ?string,
  balance: ?string,
};

export type ExtractedWallets = Array<ExtractedWallet>;
