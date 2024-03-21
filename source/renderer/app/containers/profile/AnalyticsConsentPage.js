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
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.AnalyticsConsentPage = void 0;
const react_1 = __importStar(require('react'));
const TopBar_1 = __importDefault(require('../../components/layout/TopBar'));
const TopBarLayout_1 = __importDefault(
  require('../../components/layout/TopBarLayout')
);
const AnalyticsConsentForm_1 = __importDefault(
  require('../../components/profile/analytics/AnalyticsConsentForm')
);
const types_1 = require('../../analytics/types');
const useActions_1 = require('../../hooks/useActions');
const useStores_1 = require('../../hooks/useStores');
function AnalyticsConsentPage() {
  const actions = (0, useActions_1.useActions)();
  const { networkStatus, profile, app } = (0, useStores_1.useStores)();
  const handleSubmit = (0, react_1.useCallback)(async (analyticsAccepted) => {
    await actions.profile.acceptAnalytics.trigger(
      analyticsAccepted
        ? types_1.AnalyticsAcceptanceStatus.ACCEPTED
        : types_1.AnalyticsAcceptanceStatus.REJECTED
    );
  }, []);
  const { setAnalyticsAcceptanceRequest } = profile;
  const { isShelleyActivated } = networkStatus;
  const topbar = react_1.default.createElement(TopBar_1.default, {
    isShelleyActivated: isShelleyActivated,
  });
  return react_1.default.createElement(
    TopBarLayout_1.default,
    { topbar: topbar },
    react_1.default.createElement(AnalyticsConsentForm_1.default, {
      loading: setAnalyticsAcceptanceRequest.isExecuting,
      onSubmit: handleSubmit,
      onExternalLinkClick: app.openExternalLink,
    })
  );
}
exports.AnalyticsConsentPage = AnalyticsConsentPage;
exports.default = AnalyticsConsentPage;
//# sourceMappingURL=AnalyticsConsentPage.js.map
