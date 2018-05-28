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

export type CompressedFileDownload = {
  inProgress?: boolean,
  destination?: string,
};
