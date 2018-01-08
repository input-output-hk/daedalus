import parseRedemptionCodeApi from './parse-redemption-code-from-pdf';
import resizeWindowApi from './resize-window';
import killProcess from './kill-process';

export default (params) => {
  parseRedemptionCodeApi(params);
  resizeWindowApi(params);
  killProcess(params);
};
