import type { TransactionSignatureRequest } from '../types/cip103.types';
import type { DappCip30Rejection } from './errors';

export type { TransactionSignatureRequest } from '../types/cip103.types';

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
export type Extension = { cip: number };
export type EnableOptions = { extensions?: Extension[] };
export type Paginate = { page: number; limit: number };
export type DataSignature = { signature: string; key: string };

export interface DappCip30MethodMap {
  'provider.isEnabled': { args: []; result: boolean };
  'provider.enable': { args: [options?: EnableOptions]; result: object };
  'api.getExtensions': { args: []; result: Extension[] };
  'api.getNetworkId': { args: []; result: number };
  'api.getUtxos': {
    args: [amount?: string, paginate?: Paginate];
    result: string[] | null;
  };
  'api.getCollateral': {
    args: [params: { amount: string }];
    result: string[] | null;
  };
  'api.getBalance': { args: []; result: string };
  'api.getUsedAddresses': { args: [paginate?: Paginate]; result: string[] };
  'api.getUnusedAddresses': { args: []; result: string[] };
  'api.getChangeAddress': { args: []; result: string };
  'api.getRewardAddresses': { args: []; result: string[] };
  'api.signTx': { args: [tx: string, partialSign?: boolean]; result: string };
  'api.signData': {
    args: [addr: string, payload: string];
    result: DataSignature;
  };
  'api.submitTx': { args: [tx: string]; result: string };
  'api.cip95.getPubDRepKey': { args: []; result: string };
  'api.cip95.getRegisteredPubStakeKeys': { args: []; result: string[] };
  'api.cip95.getUnregisteredPubStakeKeys': { args: []; result: string[] };
  'api.cip95.signData': {
    args: [addr: string, payload: string];
    result: DataSignature;
  };
  'api.cip103.signTxs': {
    args: [txs: TransactionSignatureRequest[]];
    result: string[];
  };
  'api.cip103.submitTxs': { args: [txs: string[]]; result: string[] };
  'api.cip104.getAccountPub': { args: []; result: string };
  'api.cip142.getNetworkMagic': { args: []; result: number };
}

export type DappCip30GatewayRequest<
  M extends DappCip30Method = DappCip30Method
> = M extends DappCip30Method
  ? { method: M; args: DappCip30MethodMap[M]['args'] }
  : never;

export type DappCip30ResultEnvelope<T = unknown> =
  | { status: 'fulfilled'; value: T }
  | { status: 'rejected'; rejection: DappCip30Rejection };

export type DappApprovalDecision = {
  requestId: string;
  approved: boolean;
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
  /** @deprecated Side-effect-free compatibility API; prefer CIP-40 collateral return. */
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
