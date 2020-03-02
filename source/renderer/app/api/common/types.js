// @flow
export type RequestConfig = $Exact<{
  hostname: string,
  port: number,
  ca: Uint8Array,
  cert: Uint8Array,
  key: Uint8Array,
}>;

export type ResponseBase = {
  status: ResponseStatus,
  meta: Pagination,
};

export type ResponseStatus = 'success' | 'fail' | 'error';

export type Pagination = {
  pagination: {
    totalPages: number,
    page: number,
    perPage: number,
    totalEntries: number,
  },
};
