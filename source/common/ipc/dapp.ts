export const DAPP_CIP30_GATEWAY_CHANNEL = 'dapp-cip30-gateway';

export const DAPP_CIP30_METHODS = [
  'provider.isEnabled',
  'provider.enable',
  'api.getExtensions',
  'api.getNetworkId',
  'api.getUtxos',
  'api.getCollateral',
  'api.getBalance',
  'api.getUsedAddresses',
  'api.getUnusedAddresses',
  'api.getChangeAddress',
  'api.getRewardAddresses',
  'api.signTx',
  'api.signData',
  'api.submitTx',
  'api.cip95.getPubDRepKey',
  'api.cip95.getRegisteredPubStakeKeys',
  'api.cip95.getUnregisteredPubStakeKeys',
  'api.cip95.signData',
  'api.cip103.signTxs',
  'api.cip103.submitTxs',
  'api.cip104.getAccountPub',
  'api.cip142.getNetworkMagic',
] as const;

export type DappCip30Method = typeof DAPP_CIP30_METHODS[number];

export type DappCip30GatewayRequest = {
  method: DappCip30Method;
  args: unknown[];
};

export type ApiError = {
  code: -1 | -2 | -3 | -4;
  info: string;
};

export type PaginateError = {
  maxSize: number;
};

export type TxSignError = {
  code: 1 | 2 | 3;
  info: string;
};

export type DataSignError = {
  code: 1 | 2 | 3;
  info: string;
};

export type TxSendError = {
  code: 1 | 2;
  info: string;
};

export type Cip103SubmitError = Array<string | TxSendError>;

export type DappCip30Rejection =
  | { type: 'api-error'; value: ApiError }
  | { type: 'paginate-error'; value: PaginateError }
  | { type: 'tx-sign-error'; value: TxSignError }
  | { type: 'data-sign-error'; value: DataSignError }
  | { type: 'tx-send-error'; value: TxSendError }
  | { type: 'cip103-submit-error'; value: Cip103SubmitError };

export type DappCip30ResultEnvelope<T = unknown> =
  | { status: 'fulfilled'; value: T }
  | { status: 'rejected'; rejection: DappCip30Rejection };

export type Extension = { cip: number };
export type EnableOptions = { extensions?: Extension[] };
export type Paginate = { page: number; limit: number };
export type DataSignature = { signature: string; key: string };
export type TransactionSignatureRequest = {
  cbor: string;
  partialSign?: boolean;
};

export interface Cip95Api {
  getPubDRepKey(): Promise<string>;
  getRegisteredPubStakeKeys(): Promise<string[]>;
  getUnregisteredPubStakeKeys(): Promise<string[]>;
  signData(addr: string, payload: string): Promise<DataSignature>;
}

export interface Cip103Api {
  signTxs(txs: TransactionSignatureRequest[]): Promise<string[]>;
  submitTxs(txs: string[]): Promise<string[]>;
}

export interface Cip104Api {
  getAccountPub(): Promise<string>;
}

export interface Cip142Api {
  getNetworkMagic(): Promise<number>;
}

export interface DaedalusApi {
  getExtensions(): Promise<Extension[]>;
  getNetworkId(): Promise<number>;
  getUtxos(amount?: string, paginate?: Paginate): Promise<string[] | null>;
  getCollateral(params: { amount: string }): Promise<string[] | null>;
  getBalance(): Promise<string>;
  getUsedAddresses(paginate?: Paginate): Promise<string[]>;
  getUnusedAddresses(): Promise<string[]>;
  getChangeAddress(): Promise<string>;
  getRewardAddresses(): Promise<string[]>;
  signTx(tx: string, partialSign?: boolean): Promise<string>;
  signData(addr: string, payload: string): Promise<DataSignature>;
  submitTx(tx: string): Promise<string>;
  cip95?: Cip95Api;
  cip103?: Cip103Api;
  cip104?: Cip104Api;
  cip142?: Cip142Api;
}

export interface DaedalusProvider {
  apiVersion: '1';
  name: string;
  icon: string;
  supportedExtensions: Extension[];
  isEnabled(): Promise<boolean>;
  enable(options?: EnableOptions): Promise<DaedalusApi>;
}
