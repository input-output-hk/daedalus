'use strict';
// @ts-nocheck
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
const Button_1 = require('@react-polymorph/components/Button');
const LinkSkin_1 = require('@react-polymorph/skins/simple/LinkSkin');
const Link_1 = require('@react-polymorph/components/Link');
const DialogCloseButton_1 = __importDefault(
  require('../../widgets/DialogCloseButton')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/close-c... Remove this comment to see the full error message
const close_cross_thin_inline_svg_1 = __importDefault(
  require('../../../assets/images/close-cross-thin.inline.svg')
);
const RTSFlagsRecommendationOverlay_scss_1 = __importDefault(
  require('./RTSFlagsRecommendationOverlay.scss')
);
const messages = (0, react_intl_1.defineMessages)({
  title: {
    id: 'knownIssues.rtsRecommendationOverlay.title',
    defaultMessage: '!!!Recommended hardware requirements status',
    description: 'Title of the RTS flags recommendation overlay',
  },
  content: {
    id: 'knownIssues.rtsRecommendationOverlay.content',
    defaultMessage:
      '!!!<p>Your system specifications do not meet Daedalus’ recommended hardware requirements.</p><p>You can enable RAM management (RTS Flags), an experimental setting that can reduce memory usage on computers with less than 16GB of RAM.</p><p>You can enable it now by clicking the ‘Enable and quit’ button. Note that you will have to restart Daedalus for this change to take effect. To enable or disable it at any time, go to the Help menu.</p>',
    description: 'Content of the RTS flags recommendation overlay',
  },
  enableAndQuitButtonLabel: {
    id: 'knownIssues.rtsRecommendationOverlay.enableAndQuitButtonLabel',
    defaultMessage: '!!!Enable and quit',
    description: 'Enable and quit button label',
  },
  decideLaterButtonLabel: {
    id: 'knownIssues.rtsRecommendationOverlay.decideLaterButtonLabel',
    defaultMessage: '!!!Decide later',
    description: 'Decide later button label',
  },
});
let RTSFlagsRecommendationOverlay = class RTSFlagsRecommendationOverlay extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  render() {
    const { intl } = this.context;
    const { onClose, onConfirm } = this.props;
    // TODO reduce duplication with AlertsOverlay
    // https://input-output.atlassian.net/browse/DDW-928
    return react_1.default.createElement(
      'div',
      { className: RTSFlagsRecommendationOverlay_scss_1.default.component },
      react_1.default.createElement(DialogCloseButton_1.default, {
        className: RTSFlagsRecommendationOverlay_scss_1.default.closeButton,
        icon: close_cross_thin_inline_svg_1.default,
        onClose: onClose,
      }),
      react_1.default.createElement(
        'h1',
        { className: RTSFlagsRecommendationOverlay_scss_1.default.title },
        intl.formatMessage(messages.title)
      ),
      react_1.default.createElement(
        'div',
        { className: RTSFlagsRecommendationOverlay_scss_1.default.content },
        react_1.default.createElement(react_intl_1.FormattedHTMLMessage, {
          ...messages.content,
        })
      ),
      react_1.default.createElement(Button_1.Button, {
        className: RTSFlagsRecommendationOverlay_scss_1.default.actionBtn,
        onClick: onConfirm,
        label: intl.formatMessage(messages.enableAndQuitButtonLabel),
        linkProps: {
          hasIconBefore: false,
          hasIconAfter: false,
        },
      }),
      react_1.default.createElement(Link_1.Link, {
        className: RTSFlagsRecommendationOverlay_scss_1.default.decideLaterLink,
        onClick: onClose,
        label: intl.formatMessage(messages.decideLaterButtonLabel),
        hasIconAfter: false,
        skin: LinkSkin_1.LinkSkin,
      })
    );
  }
};
RTSFlagsRecommendationOverlay = __decorate(
  [mobx_react_1.observer],
  RTSFlagsRecommendationOverlay
);
exports.default = RTSFlagsRecommendationOverlay;
//# sourceMappingURL=RTSFlagsRecommendationOverlay.js.map
