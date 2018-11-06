// @flow
export type RequestConfig = {
  port: number,
  ca: Uint8Array,
  cert: Uint8Array,
  key: Uint8Array,
};

export type ResponseBase = {
  status: ResponseStatus,
  meta: Pagination
};

export type ResponseStatus = 'success' | 'fail' | 'error';

export type Pagination = {
  pagination: {
    totalPages: number,
    page: number,
    perPage: number,
    totalEntries: number
  }
};

export type SendBugReportRequest = {
  email: string,
  subject: string,
  problem: string,
  compressedLogsFile: string,
};
