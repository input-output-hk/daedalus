import extractRedemptionCodeApi from './extract-redemption-code-from-pdf';
import resizeWindowApi from './resize-window';

export default (params) => {
  extractRedemptionCodeApi(params);
  resizeWindowApi(params);
};
