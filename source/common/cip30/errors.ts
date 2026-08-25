export type ApiError = {
  code: -1 | -2 | -3 | -4;
  info: string;
};

export type PaginateError = { maxSize: number };
export type TxSignError = { code: 1 | 2 | 3; info: string };
export type DataSignError = { code: 1 | 2 | 3; info: string };
export type TxSendError = { code: 1 | 2; info: string };
export type Cip103SubmitError = Array<string | TxSendError>;

export type DappCip30Rejection =
  | { type: 'api-error'; value: ApiError }
  | { type: 'paginate-error'; value: PaginateError }
  | { type: 'tx-sign-error'; value: TxSignError }
  | { type: 'data-sign-error'; value: DataSignError }
  | { type: 'tx-send-error'; value: TxSendError }
  | { type: 'cip103-submit-error'; value: Cip103SubmitError };

export const invalidRequest = (): ApiError => ({
  code: -1,
  info: 'Invalid request',
});

export const reconstructPublicRejection = (
  rejection: DappCip30Rejection
):
  | ApiError
  | PaginateError
  | TxSignError
  | DataSignError
  | TxSendError
  | Cip103SubmitError => {
  if (rejection.type === 'paginate-error') {
    return { maxSize: rejection.value.maxSize };
  }
  if (rejection.type === 'cip103-submit-error') {
    return rejection.value.map((value) =>
      typeof value === 'string' ? value : { code: value.code, info: value.info }
    );
  }
  return { code: rejection.value.code, info: rejection.value.info };
};
