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
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
const react_intl_1 = require('react-intl');
const classnames_1 = __importDefault(require('classnames'));
const Button_1 = require('@react-polymorph/components/Button');
const ButtonSkin_1 = require('@react-polymorph/skins/simple/ButtonSkin');
const Link_1 = require('@react-polymorph/components/Link');
const LinkSkin_1 = require('@react-polymorph/skins/simple/LinkSkin');
const ReportIssue_scss_1 = __importDefault(require('./ReportIssue.scss'));
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/link-ic... Remove this comment to see the full error message
const link_ic_inline_svg_1 = __importDefault(
  require('../../../assets/images/link-ic.inline.svg')
);
const messages = (0, react_intl_1.defineMessages)({
  reportConnectingIssueText: {
    id: 'loading.screen.reportIssue.connecting.text',
    defaultMessage: '!!!Having trouble connecting to network?',
    description: 'Report connecting issue text on the loading screen.',
  },
  reportIssueButtonLabel: {
    id: 'loading.screen.reportIssue.buttonLabel',
    defaultMessage: '!!!Open support ticket',
    description: 'Open support ticket button label on the loading.',
  },
  readArticleButtonLabel: {
    id: 'loading.screen.readArticle.buttonLabel',
    defaultMessage: '!!!Read the article',
    description: 'Read the article button label on the loading.',
  },
  reportIssueDownloadLogsLinkLabel: {
    id: 'loading.screen.reportIssue.downloadLogsLinkLabel',
    defaultMessage: '!!!Download logs',
    description: 'Download logs button label on the loading.',
  },
  reportIssueButtonUrl: {
    id: 'loading.screen.reportIssue.reportIssueButtonUrl',
    defaultMessage: '!!!https://iohk.zendesk.com/hc/en-us/requests/new/',
    description: 'Link to Open Support page',
  },
  connectivityIssueArticleUrl: {
    id: 'loading.screen.readIssueArticle.connectivityIssueArticleUrl',
    defaultMessage:
      '!!!https://iohk.zendesk.com/hc/en-us/articles/360010522913',
    description: 'Link to connectivity issue article page',
  },
});
class ReportIssue extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  render() {
    const { intl } = this.context;
    const {
      onIssueClick,
      onOpenExternalLink,
      onDownloadLogs,
      disableDownloadLogs,
    } = this.props;
    const reportIssueButtonClasses = (0, classnames_1.default)([
      'primary',
      'reportIssueButton',
      ReportIssue_scss_1.default.actionButton,
    ]);
    const readArticleButtonClasses = (0, classnames_1.default)([
      'primary',
      'readArticleButton',
      ReportIssue_scss_1.default.actionButton,
    ]);
    const downloadLogsButtonClasses = (0, classnames_1.default)([
      ReportIssue_scss_1.default.downloadLogsButton,
      disableDownloadLogs ? ReportIssue_scss_1.default.disabled : null,
    ]);
    const readArticleButtonUrl = messages.connectivityIssueArticleUrl;
    return react_1.default.createElement(
      'div',
      { className: ReportIssue_scss_1.default.component },
      react_1.default.createElement(
        'h1',
        { className: ReportIssue_scss_1.default.reportIssueText },
        intl.formatMessage(messages.reportConnectingIssueText)
      ),
      react_1.default.createElement(Button_1.Button, {
        className: readArticleButtonClasses,
        label: react_1.default.createElement(
          'p',
          null,
          react_1.default.createElement(react_svg_inline_1.default, {
            svg: link_ic_inline_svg_1.default,
            className: ReportIssue_scss_1.default.externalLinkIcon,
          }),
          intl.formatMessage(messages.readArticleButtonLabel)
        ),
        onClick: () =>
          onOpenExternalLink(intl.formatMessage(readArticleButtonUrl)),
        skin: ButtonSkin_1.ButtonSkin,
      }),
      react_1.default.createElement(Button_1.Button, {
        className: reportIssueButtonClasses,
        label: react_1.default.createElement(
          'p',
          null,
          react_1.default.createElement(react_svg_inline_1.default, {
            svg: link_ic_inline_svg_1.default,
            className: ReportIssue_scss_1.default.externalLinkIcon,
          }),
          intl.formatMessage(messages.reportIssueButtonLabel)
        ),
        onClick: () =>
          onIssueClick(intl.formatMessage(messages.reportIssueButtonUrl)),
        skin: ButtonSkin_1.ButtonSkin,
      }),
      react_1.default.createElement('br', null),
      react_1.default.createElement(Link_1.Link, {
        className: downloadLogsButtonClasses,
        onClick: !disableDownloadLogs ? onDownloadLogs : null,
        hasIconAfter: false,
        label: intl.formatMessage(messages.reportIssueDownloadLogsLinkLabel),
        skin: LinkSkin_1.LinkSkin,
      })
    );
  }
}
exports.default = ReportIssue;
//# sourceMappingURL=ReportIssue.js.map
