// @flow
export type FormatMessageContextParams = {
  appName: string,
  electronProcess: string,
  level: string,
  network: string,
};

export type ConstructMessageBodyParams = {
  at?: string,
  env?: string,
  ns?: Array<?string>,
  data?: Object,
  app?: Array<?string>,
  msg?: string,
  pid?: number | string,
  sev?: string,
  thread?: number | string
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
  thread: number | string
};
