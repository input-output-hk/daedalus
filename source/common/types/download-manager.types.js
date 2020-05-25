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

// event skip skipInfo object

// {
//     totalSize:, // total file size got from the server
//     fileName:, // original file name
//     filePath:, // original path name
//     downloadedSize:, // the downloaded amount
// }
// event download downloadInfo object

// {
//     totalSize:, // total file size got from the server
//     fileName:, // assigned name
//     filePath:, // download path
//     isResumed:, // if the download is a resume,
//     downloadedSize:, // the downloaded amount (only if is resumed otherwise always 0)
// }
// event progress or progress.throttled stats object

// {
//     name:, // file name
//     total:, // total size that needs to be downloaded in bytes
//     downloaded:, // downloaded size in bytes
//     progress:, // progress porcentage 0-100%
//     speed: // download speed in bytes
// }
// event end downloadInfo object

// {
//     fileName:,
//     filePath:,
//     totalSize:, // total file size got from the server
//     incomplete:, // true/false if the download endend but still incomplete
//     onDiskSize, // total size of file on the disk
//     downloadedSize:, // the total size downloaded
// }
// event renamed filePaths object

// {
//     path:, // modified path name
//     fileName:, // modified file name
//     prevPath:, // original path name
//     prevFileName:, // original file name
// }
// event error error object

// {
//     message:, // Error message
//     status:, // Http status response if available
//     body:, // Http body response if available
// }

export type DownloadResponse = {
  ...$Exact<DownloadInfo>,
  progressStatusType: DownloadProgressStatuses,
};

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
