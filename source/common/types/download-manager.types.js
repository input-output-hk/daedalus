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

// https://www.npmjs.com/package/node-downloader-helper
export type DownloadInfo = {
  fileName?: ?string,
  filePath?: ?string,
  downloaded?: ?number,
  progress?: ?number,
  error?: ?boolean,
};

export type DownloadResponse = {
  ...$Exact<DownloadInfo>,
  progressStatusType: DownloadProgressStatuses,
};

export type DownloadProgressStatuses =
  | 'start'
  | 'download'
  | 'end'
  | 'error'
  | 'stateChanged'
  | 'timeout'
  | 'progress';
