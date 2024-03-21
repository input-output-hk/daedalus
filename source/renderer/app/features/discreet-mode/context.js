'use strict';
var __createBinding =
  (this && this.__createBinding) ||
  (Object.create
    ? function (o, m, k, k2) {
        if (k2 === undefined) k2 = k;
        var desc = Object.getOwnPropertyDescriptor(m, k);
        if (
          !desc ||
          ('get' in desc ? !m.__esModule : desc.writable || desc.configurable)
        ) {
          desc = {
            enumerable: true,
            get: function () {
              return m[k];
            },
          };
        }
        Object.defineProperty(o, k2, desc);
      }
    : function (o, m, k, k2) {
        if (k2 === undefined) k2 = k;
        o[k2] = m[k];
      });
var __setModuleDefault =
  (this && this.__setModuleDefault) ||
  (Object.create
    ? function (o, v) {
        Object.defineProperty(o, 'default', { enumerable: true, value: v });
      }
    : function (o, v) {
        o['default'] = v;
      });
var __importStar =
  (this && this.__importStar) ||
  function (mod) {
    if (mod && mod.__esModule) return mod;
    var result = {};
    if (mod != null)
      for (var k in mod)
        if (k !== 'default' && Object.prototype.hasOwnProperty.call(mod, k))
          __createBinding(result, mod, k);
    __setModuleDefault(result, mod);
    return result;
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.useDiscreetModeFeature = exports.DiscreetModeFeatureProvider = exports.discreetModeContext = void 0;
const react_1 = __importStar(require('react'));
const fp_1 = require('lodash/fp');
const hooks_1 = require('../../utils/mobx-features/hooks');
const local_storage_1 = require('../local-storage');
const feature_1 = require('./feature');
const api_1 = require('./api');
const analytics_1 = require('../../components/analytics');
exports.discreetModeContext = react_1.default.createContext(null);
function DiscreetModeFeatureProvider({ children }) {
  const localStorageFeature = (0, local_storage_1.useLocalStorageFeature)();
  const analyticsTracker = (0, analytics_1.useAnalytics)();
  const [discreetModeFeature] = (0, react_1.useState)(() => {
    const feature = new feature_1.DiscreetMode(
      new api_1.DiscreetModeApi(localStorageFeature),
      analyticsTracker
    );
    window.daedalus = (0, fp_1.merge)(window.daedalus, {
      features: {
        discreetModeFeature: feature,
      },
    });
    return feature;
  });
  (0, hooks_1.useFeature)(discreetModeFeature);
  return react_1.default.createElement(
    exports.discreetModeContext.Provider,
    { value: discreetModeFeature },
    children
  );
}
exports.DiscreetModeFeatureProvider = DiscreetModeFeatureProvider;
function useDiscreetModeFeature() {
  return (0, hooks_1.getFeatureFromContext)(exports.discreetModeContext);
}
exports.useDiscreetModeFeature = useDiscreetModeFeature;
//# sourceMappingURL=context.js.map
