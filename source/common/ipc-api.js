// @flow
const PARSE_REDEMPTION_CHANNEL = 'parse-redemption-code-from-pdf';
export const PARSE_REDEMPTION_CODE = {
  REQUEST: PARSE_REDEMPTION_CHANNEL,
  SUCCESS: `${PARSE_REDEMPTION_CHANNEL}-success`,
  ERROR: `${PARSE_REDEMPTION_CHANNEL}-error`,
  INVALID_CERTIFICATE_ERROR: 'invalid certificate',
};

const GET_LOGS_CHANNEL = 'get-logs';
export const GET_LOGS = {
  REQUEST: GET_LOGS_CHANNEL,
  SUCCESS: `${GET_LOGS_CHANNEL}-success`,
};

const COMPRESS_LOGS_CHANNEL = 'compress-logs';
export const COMPRESS_LOGS = {
  REQUEST: COMPRESS_LOGS_CHANNEL,
  SUCCESS: `${COMPRESS_LOGS_CHANNEL}-success`,
  ERROR: `${COMPRESS_LOGS_CHANNEL}-error`,
};

const DOWNLOAD_LOGS_CHANNEL = 'download-logs';
export const DOWNLOAD_LOGS = {
  REQUEST: DOWNLOAD_LOGS_CHANNEL,
  SUCCESS: `${DOWNLOAD_LOGS_CHANNEL}-success`,
  ERROR: `${DOWNLOAD_LOGS_CHANNEL}-error`,
};

const GET_GPU_STATUS_CHANNEL = 'get-gpu-status';
export const GET_GPU_STATUS = {
  REQUEST: GET_GPU_STATUS_CHANNEL,
  SUCCESS: `${GET_GPU_STATUS_CHANNEL}-success`,
  ERROR: `${GET_GPU_STATUS_CHANNEL}-error`,
};

