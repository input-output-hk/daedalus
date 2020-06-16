// @flow

export type AllowedDownloadDirectories =
  | 'stateDirectory'
  | 'downloads'
  | 'desktop';

// https://www.npmjs.com/package/node-downloader-helper
export type DownloadRequest = {
  /**
   *
   * The ID is optional and can be any string without dots (.)
   * If not provided, the ID will be the fileName with dots replaced by dashes
   *
   */
  id?: string,
  fileUrl: string,
  destinationDirectoryName?: AllowedDownloadDirectories,
  options?: ?DownloadRequestOptions,
  resumeDownload?: {
    temporaryFilename: string,
    originalFilename: string,
  },
};

export type DownloadRequestOptions = {
  method?: 'GET' | 'POST', // Request Method Verb
  headers?: Object, // Custom HTTP Header ex: Authorization, User-Agent
  fileName?: string | Function | { name: string, ext: string }, // Custom filename when saved
  retry?: Object | boolean, // { maxRetries: number, delay: number in ms } or false to disable (default)
  forceResume?: boolean, // If the server does not return the "accept-ranges" header, can be force if it does support it
  removeOnStop?: boolean, // remove the file when is stopped (default:true)
  removeOnFail?: boolean, // remove the file when fail (default:true)
  override?: boolean, // if true it will override the file, otherwise will append '(number)' to the end of file
  httpRequestOptions?: Object, // Override the http request options
  httpsRequestOptions?: Object, // Override the https request options, ex: to add SSL Certs
};

// https://www.npmjs.com/package/node-downloader-helper
export type DownloadState =
  | 'IDLE'
  | 'SKIPPED'
  | 'STARTED'
  | 'DOWNLOADING'
  | 'RETRY'
  | 'PAUSED'
  | 'RESUMED'
  | 'STOPPED'
  | 'FINISHED'
  | 'FAILED';

export type DownloadEventType =
  | 'start'
  | 'download'
  | 'progress'
  | 'end'
  | 'timeout'
  | 'error';

export type DownloadResponse = {
  eventType: DownloadEventType,
  data: DownloadData,
  progress: DownloadProgress,
};

export type DownloadData = {
  downloadId: string,
  fileUrl: string,
  originalFilename: string,
  temporaryFilename: string,
  destinationDirectoryName: AllowedDownloadDirectories,
  destinationPath: string,
  options: DownloadRequestOptions,
  persistLocalData?: boolean,
};

export type DownloadProgress = {
  state: DownloadState,
  remainingSize: number,
  serverFileSize: number,
  diskFileSize: number,
  downloadSize: number,
  progress: number,
  speed: number,
  incomplete: boolean,
  isResumed: boolean,
  error?: string,
};

export type DownloadProgressUpdate = {
  state?: DownloadState,
  remainingSize?: number,
  serverFileSize?: number,
  diskFileSize?: number,
  downloadSize?: number,
  progress?: number,
  speed?: number,
  incomplete?: boolean,
  isResumed?: boolean,
  error?: string,
};

/**
 *
 * Each event has a different response
 * which is formatted and so the Main IPC
 * response has always the DownloadProgress shape
 *
 */

// https://www.npmjs.com/package/node-downloader-helper
export type DownloadInfoInit = {
  totalSize: number, // total file size got from the server
  fileName: string, // assigned name
  filePath: string, // download path
  isResumed: boolean, // if the download is a resume,
  downloadedSize: number, // the downloaded amount (only if is resumed otherwise always 0)
};
export type DownloadInfoProgress = {
  name: string, // file name
  total: number, // total size that needs to be downloaded in bytes
  downloaded: number, // downloaded size in bytes
  progress: number, // progress porcentage 0-100%
  speed: number, // download speed in bytes
};
export type DownloadInfoEnd = {
  fileName: string,
  filePath: string,
  totalSize: number, // total file size got from the server
  incomplete: boolean, // true/false if the download endend but still incomplete
  onDiskSize: number, // total size of file on the disk
  downloadedSize: number, // the total size downloaded
};
export type DownloadInfoError = {
  message: string, // Error message
  status: string, // Http status response if available
  body: string, // Http body response if available
};

export type DownloadLocalDataRequest = {
  fileName?: string,
  id?: string,
};

export type DownloadLocalDataResponse = {
  data: DownloadData,
  progress: DownloadProgress,
};

export type DownloadsLocalDataRequest = {
  state: DownloadState,
};

export type DownloadsLocalDataResponse = {
  [key: string]: DownloadsLocalDataResponse,
};

export type ResumeDownloadRequest = {
  fileName?: string,
  id?: string,
};

export type ResumeDownloadResponse = DownloadResponse | void;
