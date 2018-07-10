// @flow
export type LogFiles = {
  files: Array<string>,
  path: string,
} | {};

export type CompressedLogs = {
  files: Array<string>,
  path: string,
  originalFile: string,
} | {};

export type CompressedLogsFile = {
  fileName?: string,
  destination?: string,
  downloadInProgress?: boolean,
};
