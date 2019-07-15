// @flow
import type { CardanoNodeState } from './cardano-node.types';
import type { SystemInfo } from '../../renderer/app/types/systemInfoTypes';
import type { CoreSystemInfo } from '../../renderer/app/types/coreSystemInfoTypes';

export type FormatMessageContextParams = {
  appName: string,
  electronProcess: string,
  level: string,
  network: string,
};

export type ConstructMessageBodyParams = {
  at: string,
  env: string,
  ns?: Array<?string>,
  data?: ?Object,
  app?: Array<?string>,
  msg: string,
  pid: number | string,
  sev: string,
  thread: number | string,
};

export type MessageBody = {
  at: string,
  env: string,
  ns: Array<?string>,
  data: Object,
  app: Array<?string>,
  msg: string,
  pid: number | string,
  sev: string,
  thread: number | string,
};

export type ElectronLoggerMessage = {
  date: Date,
  data: Array<*>,
  level: string,
};

export type LogSystemInfoParams = {
  cardanoVersion: string,
  cpu: Array<Object>,
  current: string,
  daedalusVersion: string,
  isInSafeMode: boolean,
  network: string,
  osName: string,
  platformVersion: string,
  ram: string,
  startTime: string,
};

export type StateSnapshotLogParams = {
  systemInfo: SystemInfo,
  coreInfo: CoreSystemInfo,
  cardanoNodeState: CardanoNodeState | any,
  current: string,
  currentLocale: string,
  isConnected: boolean,
  isDev: boolean,
  isForceCheckingNodeTime: boolean,
  isMainnet: boolean,
  isNodeInSync: boolean,
  isNodeResponding: boolean,
  isNodeSubscribed: boolean,
  isNodeSyncing: boolean,
  isNodeTimeCorrect: boolean,
  isStaging: boolean,
  isSynced: boolean,
  isSystemTimeCorrect: boolean,
  isSystemTimeIgnored: boolean,
  isTestnet: boolean,
  latestLocalBlockTimestamp: number,
  latestNetworkBlockTimestamp: number,
  localBlockHeight: number,
  localTimeDifference: ?number,
  networkBlockHeight: number,
  startTime: string,
};
