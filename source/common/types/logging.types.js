// @flow
import type { DaedalusDiagnosticsProps } from '../../renderer/app/components/status/DaedalusDiagnostics';

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

export type LogStateSnapshotParams = {
  ...$Exact<DaedalusDiagnosticsProps>,
  current: string,
  startTime: string,
};
