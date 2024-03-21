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
const Button_1 = require('@react-polymorph/components/Button');
const ButtonSkin_1 = require('@react-polymorph/skins/simple/ButtonSkin');
const Link_1 = require('@react-polymorph/components/Link');
const LinkSkin_1 = require('@react-polymorph/skins/simple/LinkSkin');
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/not-res... Remove this comment to see the full error message
const not_responding_inline_svg_1 = __importDefault(
  require('../../../assets/images/not-responding.inline.svg')
);
const NotResponding_scss_1 = __importDefault(require('./NotResponding.scss'));
const messages = (0, react_intl_1.defineMessages)({
  title: {
    id: 'wallet.notResponding.title',
    defaultMessage: '!!!The wallet is not responding.',
    description: 'Title on the NotResponding dialog.',
  },
  description: {
    id: 'wallet.notResponding.description',
    defaultMessage:
      '!!!The {walletName} wallet is not responding. This is caused by a known but rare issue, which is currently being fixed. Please restart the Cardano node by clicking the button below, which should resolve the issue. If the issue persists, or if it happens again, please submit a support request.',
    description: 'Description on the NotResponding dialog.',
  },
  restartNodeButtonLabel: {
    id: 'wallet.notResponding.restartNodeButtonLabel',
    defaultMessage: '!!!Restart Cardano Node',
    description: 'Restart Node Button Label on the NotResponding dialog.',
  },
  submitSupportRequestLabel: {
    id: 'wallet.notResponding.submitSupportRequestLabel',
    defaultMessage: '!!!Submit a support request',
    description: 'Submit Support Request Label on the NotResponding dialog',
  },
  submitSupportRequestUrl: {
    id: 'wallet.notResponding.submitSupportRequestUrl',
    defaultMessage: '!!!https://iohk.zendesk.com/hc/en-us/requests/new/',
    description: 'Submit Support Request Url on the NotResponding dialog',
  },
});
class NotResponding extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  render() {
    const { intl } = this.context;
    const { walletName, onRestartNode, onOpenExternalLink } = this.props;
    return react_1.default.createElement(
      'div',
      { className: NotResponding_scss_1.default.component },
      react_1.default.createElement(
        'div',
        { className: NotResponding_scss_1.default.content },
        react_1.default.createElement(react_svg_inline_1.default, {
          svg: not_responding_inline_svg_1.default,
          className: NotResponding_scss_1.default.icon,
        }),
        react_1.default.createElement(
          'div',
          { className: NotResponding_scss_1.default.title },
          intl.formatMessage(messages.title)
        ),
        react_1.default.createElement(
          'div',
          { className: NotResponding_scss_1.default.description },
          react_1.default.createElement(react_intl_1.FormattedHTMLMessage, {
            ...messages.description,
            values: {
              walletName,
            },
          })
        ),
        react_1.default.createElement(Button_1.Button, {
          className: NotResponding_scss_1.default.restartNodeButton,
          label: intl.formatMessage(messages.restartNodeButtonLabel),
          onClick: onRestartNode,
          skin: ButtonSkin_1.ButtonSkin,
        }),
        react_1.default.createElement(Link_1.Link, {
          className: NotResponding_scss_1.default.submitSupportLink,
          onClick: () =>
            onOpenExternalLink(
              intl.formatMessage(messages.submitSupportRequestUrl)
            ),
          label: intl.formatMessage(messages.submitSupportRequestLabel),
          skin: LinkSkin_1.LinkSkin,
        })
      )
    );
  }
}
exports.default = NotResponding;
//# sourceMappingURL=NotResponding.js.map
