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
var __decorate =
  (this && this.__decorate) ||
  function (decorators, target, key, desc) {
    var c = arguments.length,
      r =
        c < 3
          ? target
          : desc === null
          ? (desc = Object.getOwnPropertyDescriptor(target, key))
          : desc,
      d;
    if (typeof Reflect === 'object' && typeof Reflect.decorate === 'function')
      r = Reflect.decorate(decorators, target, key, desc);
    else
      for (var i = decorators.length - 1; i >= 0; i--)
        if ((d = decorators[i]))
          r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
  };
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
const react_1 = __importStar(require('react'));
const mobx_react_1 = require('mobx-react');
const react_intl_1 = require('react-intl');
const SupportSettings_1 = __importDefault(
  require('../../../components/settings/categories/SupportSettings')
);
const reporting_1 = require('../../../../../common/utils/reporting');
const analytics_1 = require('../../../analytics');
const routes_config_1 = require('../../../routes-config');
const messages = (0, react_intl_1.defineMessages)({
  supportRequestLinkUrl: {
    id: 'settings.support.reportProblem.linkUrl',
    defaultMessage: '!!!https://iohk.zendesk.com/hc/en-us/requests/new/',
    description:
      '"submit a support request" link URL in the "Report a problem" section on the support settings page.',
  },
});
let SupportSettingsPage = class SupportSettingsPage extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  static defaultProps = {
    actions: null,
    stores: null,
  };
  handleChangeAnalyticsSettings = () => {
    this.props.actions.router.goToRoute.trigger({
      route: routes_config_1.ROUTES.PROFILE.ANALYTICS,
    });
  };
  handleSupportRequestClick = async (event) => {
    event.preventDefault();
    event.stopPropagation();
    const { intl } = this.context;
    const supportRequestLinkUrl = intl.formatMessage(
      messages.supportRequestLinkUrl
    );
    const locale = this.props.stores.profile.currentLocale;
    const { environment } = this.props.stores.app;
    const supportUrl = (0, reporting_1.generateSupportRequestLink)(
      supportRequestLinkUrl,
      environment,
      locale
    );
    this.props.stores.app.openExternalLink(supportUrl);
  };
  handleDownloadLogs = () => {
    const { app } = this.props.actions;
    app.downloadLogs.trigger();
    app.setIsDownloadingLogs.trigger(true);
  };
  render() {
    const { stores } = this.props;
    return react_1.default.createElement(SupportSettings_1.default, {
      onExternalLinkClick: stores.app.openExternalLink,
      onSupportRequestClick: this.handleSupportRequestClick,
      onDownloadLogs: this.handleDownloadLogs,
      onChangeAnalyticsSettings: this.handleChangeAnalyticsSettings,
      disableDownloadLogs: this.props.stores.app.isDownloadNotificationVisible,
      analyticsAccepted:
        this.props.stores.profile.analyticsAcceptanceStatus ===
        analytics_1.AnalyticsAcceptanceStatus.ACCEPTED,
    });
  }
};
SupportSettingsPage = __decorate(
  [(0, mobx_react_1.inject)('stores', 'actions'), mobx_react_1.observer],
  SupportSettingsPage
);
exports.default = SupportSettingsPage;
//# sourceMappingURL=SupportSettingsPage.js.map
