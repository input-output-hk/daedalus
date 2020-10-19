// @flow
export type CreateVotingRegistrationRequest = {
  walletId: string,
  address: string,
  amount: number,
  passphrase: string,
  votingKey: string,
};
