export type LogFiles = {
  files: Array<string>;
  path: string;
};
export type CompressedLogStatus = {
  fileName?: string;
  destination?: string;
  isDownloading?: boolean;
};
