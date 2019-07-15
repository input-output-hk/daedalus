// @flow
import type { CardanoNodeState } from './cardano-node.types';

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
  availableDiskSpace: string,
  cardanoAPIPort: number,
  cardanoNetwork: string,
  cardanoNodeState: CardanoNodeState | any,
  cardanoProcessID: number,
  cardanoVersion: string,
  cpu: string,
  current: string,
  currentLocale: string,
  daedalusVersion: string,
  daedalusMainProcessID: string,
  daedalusProcessID: string,
  daedalusStateDirectoryPath: string,
  isConnected: boolean,
  isDev: boolean,
  isForceCheckingNodeTime: boolean,
  isInSafeMode: boolean,
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
  platform: string,
  platformVersion: string,
  ram: string,
  startTime: string,
};
