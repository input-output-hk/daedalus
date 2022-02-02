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
  id?: string;
  fileUrl: string;
  destinationDirectoryName?: AllowedDownloadDirectories;
  options?: DownloadRequestOptions | null | undefined;
  resumeDownload?: {
    temporaryFilename: string;
    originalFilename: string;
  };
};
export type DownloadRequestOptions = {
  method?: 'GET' | 'POST';
  // Request Method Verb
  headers?: Record<string, any>;
  // Custom HTTP Header ex: Authorization, User-Agent
  fileName?:
    | string
    | ((...args: Array<any>) => any)
    | {
        name: string;
        ext: string;
      };
  // Custom filename when saved
  retry?: Record<string, any> | boolean;
  // { maxRetries: number, delay: number in ms } or false to disable (default)
  forceResume?: boolean;
  // If the server does not return the "accept-ranges" header, can be force if it does support it
  removeOnStop?: boolean;
  // remove the file when is stopped (default:true)
  removeOnFail?: boolean;
  // remove the file when fail (default:true)
  override?: boolean;
  // if true it will override the file, otherwise will append '(number)' to the end of file
  httpRequestOptions?: Record<string, any>;
  // Override the http request options
  httpsRequestOptions?: Record<string, any>;
  // Override the https request options, ex: to add SSL Certs
  progressIsThrottled?: boolean;
  // by default, the progress is sent every second. if `false` it will be sent every milisecond
  persistLocalData?: boolean; // by default, the localdata information is deleted after the end of the download
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
  | 'stop'
  | 'pause'
  | 'error';
export type DownloadResponse = {
  eventType: DownloadEventType;
  info: DownloadInfo;
  data: DownloadData;
  error?: string;
};
export type DownloadInfo = {
  downloadId: string;
  fileUrl: string;
  originalFilename: string;
  temporaryFilename: string;
  destinationDirectoryName: AllowedDownloadDirectories;
  destinationPath: string;
  options: DownloadRequestOptions;
};
export type DownloadData = {
  state: DownloadState;
  remainingSize: number;
  serverFileSize: number;
  diskFileSize: number;
  downloadSize: number;
  progress: number;
  speed: number;
  incomplete: boolean;
  isResumed: boolean;
  error?: string;
};
export type DownloadDataUpdate = {
  state?: DownloadState;
  remainingSize?: number;
  serverFileSize?: number;
  diskFileSize?: number;
  downloadSize?: number;
  progress?: number;
  speed?: number;
  incomplete?: boolean;
  isResumed?: boolean;
  error?: string;
};

/**
 *
 * Each event has a different response
 * which is formatted and so the Main IPC
 * response has always the DownloadData shape
 *
 */
// https://www.npmjs.com/package/node-downloader-helper
export type DownloadInfoInit = {
  totalSize: number;
  // total file size got from the server
  fileName: string;
  // assigned name
  filePath: string;
  // download path
  isResumed: boolean;
  // if the download is a resume,
  downloadedSize: number; // the downloaded amount (only if is resumed otherwise always 0)
};
export type DownloadInfoProgress = {
  name: string;
  // file name
  total: number;
  // total size that needs to be downloaded in bytes
  downloaded: number;
  // downloaded size in bytes
  progress: number;
  // progress porcentage 0-100%
  speed: number; // download speed in bytes
};
export type DownloadInfoEnd = {
  fileName: string;
  filePath: string;
  totalSize: number;
  // total file size got from the server
  incomplete: boolean;
  // true/false if the download endend but still incomplete
  onDiskSize: number;
  // total size of file on the disk
  downloadedSize: number; // the total size downloaded
};
export type DownloadInfoError = {
  message: string;
  // Error message
  status?: string;
  // Http status response if available
  body?: string; // Http body response if available
};
export type DownloadLocalDataRequest = {
  fileName?: string;
  id?: string;
};
export type DownloadLocalDataResponse = {
  info?: DownloadInfo;
  data?: DownloadData;
};
export type DownloadsLocalDataRequest = {
  state: DownloadState;
};
export type DownloadsLocalDataResponse = Record<
  string,
  DownloadLocalDataResponse
>;
export type ResumeDownloadRequest = DownloadLocalDataRequest;
export type ResumeDownloadResponse = DownloadResponse | void;
export type ClearDownloadLocalDataRequest = DownloadLocalDataRequest;
export type ClearDownloadLocalDataResponse = void;
export type DeleteDownloadedFileRequest = {
  id?: string;
};
export type DeleteDownloadedFileResponse = void;
export type CheckFileExistsRequest = {
  id?: string;
};
