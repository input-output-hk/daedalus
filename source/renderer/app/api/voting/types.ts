export type CreateVotingRegistrationRequest = {
  walletId: string;
  address: string;
  addressHex: string;
  amount: number;
  passphrase: string;
  votingKey: string;
  stakeKey: string;
  signature: string;
  absoluteSlotNumber: number;
};
export type CreateWalletSignatureRequest = {
  walletId: string;
  role: string;
  index: string;
  passphrase: string;
  votingKey: string;
  stakeKey: string;
  addressHex: string;
  absoluteSlotNumber: number;
};
export type SignatureParams = {
  walletId: string;
  role: string;
  index: string;
  data: {
    passphrase: string;
  };
};
export type GetCatalystFundResponse = {
  id: number;
  fund_end_time: string;
  fund_name: string;
  fund_start_time: string;
  next_fund_start_time: string;
  next_registration_snapshot_time: string;
  registration_snapshot_time: string;
  chain_vote_plans: Array<{
    chain_committee_end_time: string;
  }>;
};
export type CatalystFund = {
  fundNumber: number;
  nextFundNumber: number;
  fundEndTime: Date;
  fundStartTime: Date;
  fundResults: Date;
  nextFundStartTime: Date;
  nextRegistrationSnapshotTime: Date;
  registrationSnapshotTime: Date;
};
