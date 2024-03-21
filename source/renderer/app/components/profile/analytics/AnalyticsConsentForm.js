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
const react_1 = __importStar(require('react'));
const react_intl_1 = require('react-intl');
const Button_1 = require('@react-polymorph/components/Button');
const Link_1 = require('@react-polymorph/components/Link');
const ButtonSpinnerSkin_1 = require('@react-polymorph/skins/simple/ButtonSpinnerSkin');
const classnames_1 = __importDefault(require('classnames'));
const AnalyticsConsentForm_scss_1 = __importDefault(
  require('./AnalyticsConsentForm.scss')
);
const AnalyticsConsentForm_messages_1 = require('./AnalyticsConsentForm.messages');
const CollectedDataOverview_1 = require('./CollectedDataOverview');
const analyticsConfig_1 = require('../../../config/analyticsConfig');
function AnalyticsConsentForm({
  intl,
  loading,
  onSubmit,
  onExternalLinkClick,
}) {
  const handleAllow = (0, react_1.useCallback)(() => {
    onSubmit(true);
  }, []);
  const handleSkip = (0, react_1.useCallback)(() => {
    onSubmit(false);
  }, []);
  const privacyPolicyLink = react_1.default.createElement(Link_1.Link, {
    className: AnalyticsConsentForm_scss_1.default.privacyPolicyLink,
    onClick: () => onExternalLinkClick(analyticsConfig_1.PRIVACY_POLICY_LINK),
    label: intl.formatMessage(
      AnalyticsConsentForm_messages_1.messages.privacyPolicyLink
    ),
    hasIconAfter: false,
  });
  return react_1.default.createElement(
    'div',
    { className: AnalyticsConsentForm_scss_1.default.component },
    react_1.default.createElement(
      'div',
      { className: AnalyticsConsentForm_scss_1.default.centeredBox },
      react_1.default.createElement(
        'h2',
        { className: AnalyticsConsentForm_scss_1.default.title },
        intl.formatMessage(AnalyticsConsentForm_messages_1.messages.title)
      ),
      react_1.default.createElement(
        'p',
        { className: AnalyticsConsentForm_scss_1.default.description },
        intl.formatMessage(AnalyticsConsentForm_messages_1.messages.description)
      ),
      react_1.default.createElement(
        CollectedDataOverview_1.CollectedDataOverview,
        null
      ),
      react_1.default.createElement(
        'p',
        {
          className:
            AnalyticsConsentForm_scss_1.default.privacyPolicyDescription,
        },
        react_1.default.createElement(react_intl_1.FormattedMessage, {
          ...AnalyticsConsentForm_messages_1.messages
            .analyticsSectionPrivacyPolicy,
          values: {
            privacyPolicyLink,
          },
        })
      ),
      react_1.default.createElement(
        'div',
        { className: AnalyticsConsentForm_scss_1.default.actions },
        react_1.default.createElement(Button_1.Button, {
          className: (0, classnames_1.default)(
            AnalyticsConsentForm_scss_1.default.disallowButton,
            'flat'
          ),
          label: intl.formatMessage(
            AnalyticsConsentForm_messages_1.messages.disallowButton
          ),
          skin: ButtonSpinnerSkin_1.ButtonSpinnerSkin,
          loading: loading,
          onClick: handleSkip,
        }),
        react_1.default.createElement(Button_1.Button, {
          label: intl.formatMessage(
            AnalyticsConsentForm_messages_1.messages.allowButton
          ),
          skin: ButtonSpinnerSkin_1.ButtonSpinnerSkin,
          loading: loading,
          onClick: handleAllow,
        })
      )
    )
  );
}
exports.default = (0, react_intl_1.injectIntl)(AnalyticsConsentForm);
//# sourceMappingURL=AnalyticsConsentForm.js.map
