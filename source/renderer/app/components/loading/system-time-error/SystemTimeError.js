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
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
const mobx_react_1 = require('mobx-react');
const react_intl_1 = require('react-intl');
const classnames_1 = __importDefault(require('classnames'));
const Link_1 = require('@react-polymorph/components/Link');
const LinkSkin_1 = require('@react-polymorph/skins/simple/LinkSkin');
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/attenti... Remove this comment to see the full error message
const attention_big_light_inline_svg_1 = __importDefault(
  require('../../../assets/images/attention-big-light.inline.svg')
);
const timingConfig_1 = require('../../../config/timingConfig');
const humanizeDurationByLocale_1 = __importDefault(
  require('../../../utils/humanizeDurationByLocale')
);
const SystemTimeError_scss_1 = __importDefault(
  require('./SystemTimeError.scss')
);
const messages = (0, react_intl_1.defineMessages)({
  overlayTitle: {
    id: 'systemTime.error.overlayTitle',
    defaultMessage: '!!!Unable to sync - incorrect time',
    description: 'Title of Sync error overlay',
  },
  overlayTextP1: {
    id: 'systemTime.error.overlayTextP1',
    defaultMessage:
      '!!!Attention, Daedalus is unable to sync with the blockchain because the time on your machine is different from the global time. Your time is off by 2 hours 12 minutes 54 seconds.',
    description: 'First paragraph of Sync error overlay',
  },
  overlayTextP2: {
    id: 'systemTime.error.overlayTextP2',
    defaultMessage:
      '!!!To synchronise the time and fix the issue, please read our {supportPortalLink} article.',
    description: 'Second paragraph of Sync error overlay',
  },
  ntpUnreachableTextP1: {
    id: 'systemTime.error.ntpUnreachableTextP1',
    defaultMessage:
      '!!!Attention, Daedalus is unable to check if the clock on your computer is synchronized with global time because NTP (Network Time Protocol) servers are unreachable, possibly due to firewalls on your network.',
    description: 'Text of Sync error overlay when NTP service is unreachable',
  },
  ntpUnreachableTextP2: {
    id: 'systemTime.error.ntpUnreachableTextP2',
    defaultMessage:
      '!!!If your computer clock is off by more than 15 seconds, Daedalus will be unable to connect to the network. If you have this issue, please read our Support Portal article to synchronize the time on your machine.',
    description: 'Text of Sync error overlay when NTP service is unreachable',
  },
  supportPortalLink: {
    id: 'systemTime.error.supportPortalLink',
    defaultMessage: '!!!Support Portal',
    description: '"Support Portal" link text',
  },
  supportPortalLinkUrl: {
    id: 'systemTime.error.supportPortalLinkUrl',
    defaultMessage:
      '!!!https://iohk.zendesk.com/hc/en-us/articles/360010230873',
    description:
      'Link to "Machine clock out of sync with Cardano network" support page',
  },
  onCheckTheTimeAgainLink: {
    id: 'systemTime.error.onCheckTheTimeAgainLink',
    defaultMessage: '!!!Check the time again',
    description: 'Text of Check the time again button',
  },
  onContinueWithoutClockSyncCheckLink: {
    id: 'systemTime.error.onContinueWithoutClockSyncCheckLink',
    defaultMessage: '!!!Continue without clock synchronization checks',
    description:
      'Text of "Continue without clock synchronization checks" button',
  },
});
let SystemTimeError = class SystemTimeError extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  render() {
    const { intl } = this.context;
    const {
      localTimeDifference,
      currentLocale,
      isCheckingSystemTime,
      onCheckTheTimeAgain,
      onContinueWithoutClockSyncCheck,
      onExternalLinkClick,
    } = this.props;
    const supportPortalLinkUrl = intl.formatMessage(
      messages.supportPortalLinkUrl
    );
    const supportPortalLink = react_1.default.createElement(Link_1.Link, {
      className: SystemTimeError_scss_1.default.supportPortalLink,
      onClick: (event) => onExternalLinkClick(supportPortalLinkUrl, event),
      label: intl.formatMessage(messages.supportPortalLink),
      skin: LinkSkin_1.LinkSkin,
    });
    const isNTPServiceReachable = !!localTimeDifference;
    const allowedTimeDifferenceInSeconds =
      timingConfig_1.ALLOWED_TIME_DIFFERENCE / 1000000;
    const rawTimeOffset = (localTimeDifference || 0) / 1000;
    const timeOffset = (0, humanizeDurationByLocale_1.default)(
      rawTimeOffset,
      currentLocale,
      {
        delimiter: ' ',
        units: ['y', 'mo', 'w', 'd', 'h', 'm', 's', 'ms'],
        localeConfig: {
          'ja-JP': {
            spacer: '',
            delimiter: '',
            serialComma: false,
          },
        },
      }
    );
    return react_1.default.createElement(
      'div',
      { className: SystemTimeError_scss_1.default.component },
      react_1.default.createElement(react_svg_inline_1.default, {
        svg: attention_big_light_inline_svg_1.default,
        className: SystemTimeError_scss_1.default.icon,
      }),
      isNTPServiceReachable
        ? react_1.default.createElement(
            'div',
            null,
            react_1.default.createElement(
              'p',
              null,
              react_1.default.createElement(react_intl_1.FormattedMessage, {
                ...messages.overlayTextP1,
                values: {
                  timeOffset: react_1.default.createElement(
                    'em',
                    { className: 'time-off' },
                    timeOffset
                  ),
                },
              })
            ),
            react_1.default.createElement(
              'p',
              null,
              react_1.default.createElement(react_intl_1.FormattedMessage, {
                ...messages.overlayTextP2,
                values: {
                  supportPortalLink,
                },
              })
            ),
            react_1.default.createElement(Link_1.Link, {
              className: (0, classnames_1.default)([
                SystemTimeError_scss_1.default.checkLink,
                isCheckingSystemTime
                  ? SystemTimeError_scss_1.default.disabled
                  : null,
              ]),
              onClick: () => onCheckTheTimeAgain(),
              label: intl.formatMessage(messages.onCheckTheTimeAgainLink),
              hasIconAfter: false,
              skin: LinkSkin_1.LinkSkin,
            })
          )
        : react_1.default.createElement(
            'div',
            null,
            react_1.default.createElement(
              'p',
              null,
              react_1.default.createElement(react_intl_1.FormattedMessage, {
                ...messages.ntpUnreachableTextP1,
                values: {
                  timeOffset,
                },
              })
            ),
            react_1.default.createElement(
              'p',
              null,
              react_1.default.createElement(react_intl_1.FormattedMessage, {
                ...messages.ntpUnreachableTextP2,
                values: {
                  supportPortalLink,
                  allowedTimeDifferenceInSeconds,
                },
              })
            ),
            react_1.default.createElement(Link_1.Link, {
              className: SystemTimeError_scss_1.default.checkLink,
              onClick: () => onContinueWithoutClockSyncCheck(),
              label: intl.formatMessage(
                messages.onContinueWithoutClockSyncCheckLink
              ),
              hasIconAfter: false,
              skin: LinkSkin_1.LinkSkin,
            })
          )
    );
  }
};
SystemTimeError = __decorate([mobx_react_1.observer], SystemTimeError);
exports.default = SystemTimeError;
//# sourceMappingURL=SystemTimeError.js.map
