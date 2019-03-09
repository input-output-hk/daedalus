// @flow
export type ExtractedWallet = {
  raw: Array<Buffer>,
  passwordHash: string,
  password: ?string,
  balance: ?number,
};

export type ExtractedWallets = Array<ExtractedWallet>;
