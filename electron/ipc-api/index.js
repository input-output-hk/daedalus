import compressLogsApi from './compress-logs';
import deleteCompressedLogsApi from './delete-compressed-logs';
import getLogsApi from './get-logs';
import parseRedemptionCodeApi from './parse-redemption-code-from-pdf';
import resizeWindowApi from './resize-window';
import killProcess from './kill-process';

export default (params) => {
  compressLogsApi(params);
  deleteCompressedLogsApi(params);
  getLogsApi(params);
  parseRedemptionCodeApi(params);
  resizeWindowApi(params);
  killProcess(params);
};
