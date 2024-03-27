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
const classnames_1 = __importDefault(require('classnames'));
const Link_1 = require('@react-polymorph/components/Link');
const LinkSkin_1 = require('@react-polymorph/skins/simple/LinkSkin');
const SupportSettings_scss_1 = __importDefault(
  require('./SupportSettings.scss')
);
const global_messages_1 = __importDefault(
  require('../../../i18n/global-messages')
);
const SupportSettings_messages_1 = require('./SupportSettings.messages');
const Separator_1 = require('../../widgets/separator/Separator');
let SupportSettings = class SupportSettings extends react_1.Component {
  render() {
    const {
      onExternalLinkClick,
      onSupportRequestClick,
      onDownloadLogs,
      disableDownloadLogs,
      intl,
    } = this.props;
    const faqLinkUrl = intl.formatMessage(global_messages_1.default.faqLinkUrl);
    const faqLink = react_1.default.createElement(
      'span',
      { className: SupportSettings_scss_1.default.faqLink },
      react_1.default.createElement(Link_1.Link, {
        className: SupportSettings_scss_1.default.externalLink,
        onClick: (event) => onExternalLinkClick(faqLinkUrl, event),
        label: intl.formatMessage(SupportSettings_messages_1.messages.faqLink),
        skin: LinkSkin_1.LinkSkin,
      })
    );
    const downloadLogsClasses = (0, classnames_1.default)([
      SupportSettings_scss_1.default.externalLink,
      disableDownloadLogs ? SupportSettings_scss_1.default.disabled : null,
    ]);
    const stepsDownloadLogsLink = react_1.default.createElement(Link_1.Link, {
      className: downloadLogsClasses,
      onClick: onDownloadLogs,
      hasIconAfter: false,
      label: intl.formatMessage(
        SupportSettings_messages_1.messages.stepsDownloadLogsLink
      ),
      skin: LinkSkin_1.LinkSkin,
    });
    const reportProblemLink = react_1.default.createElement(Link_1.Link, {
      className: SupportSettings_scss_1.default.externalLink,
      onClick: onSupportRequestClick,
      label: intl.formatMessage(
        SupportSettings_messages_1.messages.stepsReportProblemLink
      ),
      skin: LinkSkin_1.LinkSkin,
    });
    const changeAnalyticsSettingsLink = react_1.default.createElement(
      Link_1.Link,
      {
        className: SupportSettings_scss_1.default.changeAnalyticsSettingsLink,
        onClick: this.props.onChangeAnalyticsSettings,
        label: intl.formatMessage(
          SupportSettings_messages_1.messages.changeAnalyticsSettingsLink
        ),
        hasIconAfter: false,
      }
    );
    return react_1.default.createElement(
      react_1.default.Fragment,
      null,
      react_1.default.createElement(
        'div',
        { className: SupportSettings_scss_1.default.supportGuide },
        react_1.default.createElement(
          'h1',
          null,
          intl.formatMessage(SupportSettings_messages_1.messages.faqTitle)
        ),
        react_1.default.createElement(
          'p',
          null,
          react_1.default.createElement(react_intl_1.FormattedMessage, {
            ...SupportSettings_messages_1.messages.faqContent,
            values: {
              faqLink,
            },
          })
        ),
        react_1.default.createElement(
          'h1',
          null,
          intl.formatMessage(SupportSettings_messages_1.messages.stepsTitle)
        ),
        react_1.default.createElement(
          'ol',
          null,
          react_1.default.createElement(
            'li',
            null,
            react_1.default.createElement(
              'h2',
              null,
              intl.formatMessage(
                SupportSettings_messages_1.messages.stepsDownloadLogsTitle
              )
            ),
            react_1.default.createElement(
              'p',
              null,
              react_1.default.createElement(react_intl_1.FormattedMessage, {
                ...SupportSettings_messages_1.messages
                  .stepsDownloadLogsDescription,
                values: {
                  stepsDownloadLogsLink,
                },
              })
            )
          ),
          react_1.default.createElement(
            'li',
            null,
            react_1.default.createElement(
              'h2',
              null,
              intl.formatMessage(
                SupportSettings_messages_1.messages.stepsReportProblemTitle
              )
            ),
            react_1.default.createElement(
              'p',
              null,
              react_1.default.createElement(react_intl_1.FormattedMessage, {
                ...SupportSettings_messages_1.messages
                  .stepsReportProblemDescription,
                values: {
                  reportProblemLink,
                },
              })
            )
          )
        )
      ),
      react_1.default.createElement(Separator_1.Separator, null),
      react_1.default.createElement(
        'h2',
        { className: SupportSettings_scss_1.default.analyticsSectionTitle },
        intl.formatMessage(
          SupportSettings_messages_1.messages.analyticsSectionTitle
        )
      ),
      react_1.default.createElement(
        'p',
        {
          className: SupportSettings_scss_1.default.analyticsSectionDescription,
        },
        react_1.default.createElement(react_intl_1.FormattedHTMLMessage, {
          ...(this.props.analyticsAccepted
            ? SupportSettings_messages_1.messages.analyticsAcceptedDescription
            : SupportSettings_messages_1.messages.analyticsDeclinedDescription),
        }),
        react_1.default.createElement(react_intl_1.FormattedMessage, {
          ...SupportSettings_messages_1.messages.changeAnalyticsSettings,
          values: {
            changeAnalyticsSettingsLink,
          },
        })
      )
    );
  }
};
SupportSettings = __decorate([mobx_react_1.observer], SupportSettings);
exports.default = (0, react_intl_1.injectIntl)(SupportSettings);
//# sourceMappingURL=SupportSettings.js.map
