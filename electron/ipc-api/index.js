import parseRedemptionCodeApi from './parse-redemption-code-from-pdf';
import resizeWindowApi from './resize-window';

export default (params) => {
  parseRedemptionCodeApi(params);
  resizeWindowApi(params);
};
