// @flow
import type { CardanoNodeState } from './cardano-node.types';
import type { SystemInfo } from '../../renderer/app/types/systemInfoTypes';
import type { CoreSystemInfo } from '../../renderer/app/types/coreSystemInfoTypes';
import type { WalletMigrationStatus } from '../../renderer/app/stores/WalletMigrationStore';
import LocalizableError from '../../renderer/app/i18n/LocalizableError';

export type Logger = {
  debug: (string, ?Object) => void,
  info: (string, ?Object) => void,
  error: (string, ?Object) => void,
  warn: (string, ?Object) => void,
};

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
  daedalusVersion: string,
  isBlankScreenFixActive: boolean,
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
  currentLocale: string,
  isConnected: boolean,
  isDev: boolean,
  isMainnet: boolean,
  isNodeInSync: boolean,
  isNodeResponding: boolean,
  isNodeSyncing: boolean,
  isStaging: boolean,
  isSynced: boolean,
  isTestnet: boolean,
  currentTime: string,
  syncPercentage: string,
  localTip: ?Object,
  networkTip: ?Object,
};

export type WalletMigrationReportData = {
  exportedWalletsData: Array<{
    name: string,
    hasPassword: boolean,
  }>,
  exportedWalletsCount: number,
  exportErrors: string,
  restoredWalletsData: Array<{
    id: string,
    name: string,
    hasPassword: boolean,
  }>,
  restoredWalletsCount: number,
  restorationErrors: Array<{
    error: LocalizableError,
    wallet: { name: string, hasPassword: boolean },
  }>,
  finalMigrationStatus: WalletMigrationStatus,
};
