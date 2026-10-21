import {
  StatusWordV7,
  StatusWordV8,
} from '@cardano-foundation/ledgerjs-hw-app-cardano/dist/errors/deviceStatusError';

export const isLedgerUserRejection = (error: {
  statusCode?: number;
  code?: string | number;
}) => {
  const statusCode = error.statusCode ?? error.code;
  return (
    statusCode === StatusWordV7.ERR_REJECTED_BY_USER ||
    statusCode === StatusWordV8.SWO_CONDITIONS_NOT_SATISFIED
  );
};
