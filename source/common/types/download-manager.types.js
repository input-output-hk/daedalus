// @flow

export type PathOption = 'download' | 'state';

type FilePatternMatch = {
  filePath?: PathOption,
  fileNamePattern: string | RegExp,
  fileExtentionPattern: string | RegExp,
};

type FileExactMatch = {
  filePath?: PathOption,
  fileName: string,
  fileExtention: string,
};

export type PersistedDownloadStatusRequest = {
  file: FilePatternMatch | FileExactMatch,
};

export type PersistedDownloadStatusResponse = {
  hasPendingDownload: boolean,
  pendingUpdateFileName: ?string,
  downloadProgress?: ?number,
};

export type DownloadStatusRequest = {
  file: FileExactMatch,
};

export type DownloadStatusResponse = {
  isDownloading: boolean,
  downloadProgress?: ?number,
};

export type AllowedDownloadDirectories = 'downloads' | 'desktop';

// https://www.npmjs.com/package/node-downloader-helper
export type DownloadRequest = {
  fileUrl: string,
  destinationDirectoryName?: AllowedDownloadDirectories,
  options?: ?{
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
  },
};

export type DownloadResponse = {
  progressStatusType: DownloadProgressStatuses,
  downloadInfo: DownloadInfo,
};

// https://www.npmjs.com/package/node-downloader-helper

export type DownloadProgressStatuses =
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

// https://www.npmjs.com/package/node-downloader-helper

export type DownloadInfo =
  | DownloadInfoInit
  | DownloadInfoProgress
  | DownloadInfoEnd
  | DownloadInfoError;

export type DownloadInfoInit = {
  totalSize: number, // total file size got from the server
  fileName: string, // assigned name
  filePath: string, // download path
  isResumed: boolean, // if the download is a resume,
  downloadedSize: number, // the downloaded amount (only if is resumed otherwise always 0)
};

export type DownloadInfoProgress = {
  name: string, // file name
  total: string, // total size that needs to be downloaded in bytes
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
