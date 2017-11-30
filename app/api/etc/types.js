// @flow
import BigNumber from 'bignumber.js';

export type EtcAccountPassphrase = string;
export type EtcWalletId = string;
export type EtcWalletBalance = string;
export type EtcBlockNumber = number;
export type EtcGas = string;
export type EtcGasPrice = BigNumber;
export type EtcTxHash = string;

export type EtcRecoveryPassphrase = Array<string>;

export type EtcAccounts = Array<EtcWalletId>;

export type EtcBlock = {
  timestamp: string
};

export type EtcSyncProgress = ?{
  startingBlock: EtcBlock,
  currentBlock: EtcBlock,
  highestBlock: EtcBlock
};

export type EtcTransaction = {
  hash: EtcTxHash,
  nonce: string,
  blockHash: string,
  blockNumber: EtcBlockNumber,
  transactionIndex: string,
  from: EtcWalletId,
  to: EtcWalletId,
  value: string,
  gasPrice: EtcGasPrice,
  gas: EtcGas,
  input: string,
  pending: boolean,
};

export type EtcTransactions = {
  received: Array<EtcTransaction>,
  sent: Array<EtcTransaction>,
};
