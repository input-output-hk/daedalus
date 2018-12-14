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
  requestFormData: {
    email: string,
    subject: string,
    problem: string,
    compressedLogsFile: string,
  },
  environmentData: {
    network: string,
    version: string,
    os: string,
    apiVersion: string,
    build: string,
    installerVersion: string,
    reportUrl: string,
  }
};
