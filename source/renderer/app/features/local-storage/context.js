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
exports.useLocalStorageFeature = exports.LocalStorageFeatureProvider = exports.localStorageContext = void 0;
const react_1 = __importStar(require('react'));
const fp_1 = require('lodash/fp');
const hooks_1 = require('../../utils/mobx-features/hooks');
exports.localStorageContext = react_1.default.createContext(null);
function LocalStorageFeatureProvider({ children, localStorage }) {
  const [localStorageFeature] = (0, react_1.useState)(() => {
    // @ts-ignore ts-migrate(2339) FIXME: Property 'daedalus' does not exist on type 'Window... Remove this comment to see the full error message
    window.daedalus = (0, fp_1.merge)(window.daedalus, {
      features: {
        localStorage,
      },
    });
    return localStorage;
  });
  return react_1.default.createElement(
    exports.localStorageContext.Provider,
    { value: localStorageFeature },
    children
  );
}
exports.LocalStorageFeatureProvider = LocalStorageFeatureProvider;
function useLocalStorageFeature() {
  return (0, hooks_1.getFeatureFromContext)(exports.localStorageContext);
}
exports.useLocalStorageFeature = useLocalStorageFeature;
//# sourceMappingURL=context.js.map
