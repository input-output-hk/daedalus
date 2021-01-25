// @flow
export type CreateVotingRegistrationRequest = {
  walletId: string,
  address: string,
  amount: number,
  passphrase: string,
  votingKey: string,
  stakeKey: string,
  signature: string,
};

export type CreateWalletSignatureRequest = {
  walletId: string,
  role: string,
  index: string,
  passphrase: string,
  votingKey: string,
  stakeKey: string,
};

export type SignatureParams = {
  walletId: string,
  role: string,
  index: string,
  data: {
    passphrase: string,
  },
};
