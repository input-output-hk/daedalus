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
exports.messages = void 0;
// @ts-nocheck
const react_1 = __importStar(require('react'));
const mobx_react_1 = require('mobx-react');
const lodash_1 = require('lodash');
const classnames_1 = __importDefault(require('classnames'));
const react_intl_1 = require('react-intl');
const Button_1 = require('@react-polymorph/components/Button');
const ButtonSkin_1 = require('@react-polymorph/skins/simple/ButtonSkin');
const PopOver_1 = require('@react-polymorph/components/PopOver');
const moment_1 = __importDefault(require('moment'));
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/recover... Remove this comment to see the full error message
const recovery_phrase_verification_ok_inline_svg_1 = __importDefault(
  require('../../../assets/images/recovery-phrase-verification-ok.inline.svg')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/recover... Remove this comment to see the full error message
const recovery_phrase_verification_warning_inline_svg_1 = __importDefault(
  require('../../../assets/images/recovery-phrase-verification-warning.inline.svg')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/recover... Remove this comment to see the full error message
const recovery_phrase_verification_notification_inline_svg_1 = __importDefault(
  require('../../../assets/images/recovery-phrase-verification-notification.inline.svg')
);
const WalletRecoveryPhraseVerificationWidget_scss_1 = __importDefault(
  require('./WalletRecoveryPhraseVerificationWidget.scss')
);
const walletRecoveryPhraseVerificationConfig_1 = require('../../../config/walletRecoveryPhraseVerificationConfig');
const walletRecoveryPhraseVerificationUtils_1 = require('../../../utils/walletRecoveryPhraseVerificationUtils');
const locales_types_1 = require('../../../../../common/types/locales.types');
exports.messages = (0, react_intl_1.defineMessages)({
  title: {
    id: 'wallet.settings.recoveryPhraseVerification.title',
    defaultMessage: '!!!Do you have your wallet recovery phrase?',
    description:
      'Label for the recoveryPhraseVerificationTitle on wallet settings.',
  },
  description: {
    id: 'wallet.settings.recoveryPhraseVerification.description',
    defaultMessage:
      '!!!Funds in this wallet can only be recovered using the correct wallet recovery phrase, which is a unique {wordCount}-word string you were shown and asked to write down when creating this wallet. You can re-enter your wallet recovery phrase to verify that you have the correct recovery phrase for this wallet.',
    description:
      'Label for the recoveryPhraseVerificationDescription on wallet settings.',
  },
  neverOkTimeUntil: {
    id: 'wallet.settings.recoveryPhraseVerification.neverOkTimeUntil',
    defaultMessage:
      '!!!We recommend that you verify your wallet recovery phrase in <b>{timeUntilWarning}</b>.',
    description:
      'Label for the recoveryPhraseVerificationNeverOk on wallet settings.',
  },
  neverOkFewMonths: {
    id: 'wallet.settings.recoveryPhraseVerification.neverOkFewMonths',
    defaultMessage:
      '!!!We recommend that you verify your wallet recovery phrase in a few months.',
    description:
      'Label for the recoveryPhraseVerificationNeverOk on wallet settings.',
  },
  neverOkFewWeeks: {
    id: 'wallet.settings.recoveryPhraseVerification.neverOkFewWeeks',
    defaultMessage:
      '!!!We recommend that you verify your wallet recovery phrase in a few weeks.',
    description:
      'Label for the recoveryPhraseVerificationNeverOk on wallet settings.',
  },
  neverOkFewDays: {
    id: 'wallet.settings.recoveryPhraseVerification.neverOkFewDays',
    defaultMessage:
      '!!!We recommend that you verify your wallet recovery phrase in a few days.',
    description:
      'Label for the recoveryPhraseVerificationNeverOk on wallet settings.',
  },
  neverWarning: {
    id: 'wallet.settings.recoveryPhraseVerification.neverWarning',
    defaultMessage:
      '!!!We recommend that you verify your wallet recovery phrase.',
    description:
      'Label for the recoveryPhraseVerificationNeverWarning on wallet settings.',
  },
  neverNotification: {
    id: 'wallet.settings.recoveryPhraseVerification.neverNotification',
    defaultMessage:
      '!!!We recommend that you verify your wallet recovery phrase.',
    description:
      'Label for the recoveryPhraseVerificationNeverNotification on wallet settings.',
  },
  checkedOk: {
    id: 'wallet.settings.recoveryPhraseVerification.checkedOk',
    defaultMessage:
      '!!!You verified the recovery phrase for this wallet <b>{timeAgo}</b>.',
    description:
      'Label for the recoveryPhraseVerificationCheckedOk on wallet settings.',
  },
  checkedWarning: {
    id: 'wallet.settings.recoveryPhraseVerification.checkedWarning',
    defaultMessage:
      '!!!You verified the recovery phrase for this wallet <b>{timeAgo}</b>.',
    description:
      'Label for the recoveryPhraseVerificationCheckedWarning on wallet settings.',
  },
  checkedNotification: {
    id: 'wallet.settings.recoveryPhraseVerification.checkedNotification',
    defaultMessage:
      '!!!You verified the recovery phrase for this wallet <b>{timeAgo}</b>. We recommend that you verify your wallet recovery phrase again.',
    description:
      'Label for the recoveryPhraseVerificationCheckedNotification on wallet settings.',
  },
  paperWalletDescription: {
    id: 'wallet.settings.recoveryPhraseVerification.paperWalletDescription',
    defaultMessage:
      '!!!If this wallet was restored from a paper wallet certificate, you cannot use this feature to verify your wallet recovery phrase. Paper wallet recovery phrase to regular wallet recovery phrase conversion will be available in Daedalus soon.',
    description:
      'Description for the paperWallet instructions on wallet settings.',
  },
  paperWalletTitle: {
    id: 'wallet.settings.recoveryPhraseVerification.paperWalletTitle',
    defaultMessage: '!!!Paper wallet',
    description: 'Title for the paperWallet instructions on wallet settings.',
  },
  button: {
    id: 'wallet.settings.recoveryPhraseVerification.button',
    defaultMessage: '!!!Verify wallet recovery phrase',
    description:
      'Label for the recoveryPhraseVerificationButton on wallet settings.',
  },
  timeUntilWarningReplacement: {
    id:
      'wallet.settings.recoveryPhraseVerification.timeUntilWarningReplacement',
    defaultMessage: '!!!ヶ月,か月',
    description:
      'Label for the recoveryPhraseVerificationButton on wallet settings.',
  },
});
let WalletRecoveryPhraseVerificationWidget = class WalletRecoveryPhraseVerificationWidget extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  get statuses() {
    return {
      neverVerified_okTimeUntil: {
        icon: recovery_phrase_verification_ok_inline_svg_1.default,
        message: exports.messages.neverOkTimeUntil,
      },
      neverVerified_okFewMonths: {
        icon: recovery_phrase_verification_ok_inline_svg_1.default,
        message: exports.messages.neverOkFewMonths,
      },
      neverVerified_okFewWeeks: {
        icon: recovery_phrase_verification_ok_inline_svg_1.default,
        message: exports.messages.neverOkFewWeeks,
      },
      neverVerified_okFewDays: {
        icon: recovery_phrase_verification_ok_inline_svg_1.default,
        message: exports.messages.neverOkFewDays,
      },
      neverVerified_warning: {
        icon: recovery_phrase_verification_warning_inline_svg_1.default,
        message: exports.messages.neverWarning,
      },
      neverVerified_notification: {
        icon: recovery_phrase_verification_notification_inline_svg_1.default,
        message: exports.messages.neverNotification,
      },
      alreadyVerified_ok: {
        icon: recovery_phrase_verification_ok_inline_svg_1.default,
        message: exports.messages.checkedOk,
      },
      alreadyVerified_warning: {
        icon: recovery_phrase_verification_warning_inline_svg_1.default,
        message: exports.messages.checkedWarning,
      },
      alreadyVerified_notification: {
        icon: recovery_phrase_verification_notification_inline_svg_1.default,
        message: exports.messages.checkedNotification,
      },
    };
  }
  get recoveryPhraseStatus() {
    const { creationDate, recoveryPhraseVerificationDate, locale } = this.props;
    const {
      recoveryPhraseVerificationStatus: status,
      recoveryPhraseVerificationStatusType: type,
    } = (0, walletRecoveryPhraseVerificationUtils_1.getStatusFromWalletData)({
      creationDate,
      recoveryPhraseVerificationDate,
    });
    const { icon, message } = this.statuses[`${type}_${status}`];
    const timeAgo = (0, moment_1.default)(
      recoveryPhraseVerificationDate
    ).fromNow();
    const timeFromCreationToWarning = (0, moment_1.default)(
      new Date(creationDate)
    ).add(
      walletRecoveryPhraseVerificationConfig_1
        .RECOVERY_PHRASE_VERIFICATION_TIMES.warning,
      'days'
    );
    let timeUntilWarning = (0, moment_1.default)()
      .locale(locale)
      .to(timeFromCreationToWarning, true);
    // The content is generated by `moment`, but we need to replace `ヶ月` by `か月`
    if (locale === locales_types_1.LOCALES.japanese) {
      const replacement = this.context.intl
        .formatMessage(exports.messages.timeUntilWarningReplacement)
        .split(',');
      // @ts-ignore ts-migrate(2556) FIXME: Expected 2 arguments, but got 0 or more.
      timeUntilWarning = timeUntilWarning.replace(...replacement);
    }
    return {
      icon,
      message,
      timeAgo,
      timeUntilWarning,
    };
  }
  render() {
    const { intl } = this.context;
    const {
      onVerify,
      wordCount,
      creationDate,
      recoveryPhraseVerificationDate,
      isLegacy,
    } = this.props;
    const {
      icon,
      message,
      timeAgo,
      timeUntilWarning,
    } = this.recoveryPhraseStatus;
    const { recoveryPhraseVerificationStatus } = (0,
    walletRecoveryPhraseVerificationUtils_1.getStatusFromWalletData)({
      creationDate,
      recoveryPhraseVerificationDate,
    });
    const statusStyle = (0, lodash_1.camelCase)(
      `status ${recoveryPhraseVerificationStatus}`
    );
    const statusStyles = (0, classnames_1.default)([
      WalletRecoveryPhraseVerificationWidget_scss_1.default.status,
      WalletRecoveryPhraseVerificationWidget_scss_1.default[statusStyle],
    ]);
    let statusButtonType = 'flat';
    if (
      recoveryPhraseVerificationStatus ===
      walletRecoveryPhraseVerificationConfig_1
        .RECOVERY_PHRASE_VERIFICATION_STATUSES.WARNING
    )
      statusButtonType = 'primary';
    else if (
      recoveryPhraseVerificationStatus ===
      walletRecoveryPhraseVerificationConfig_1
        .RECOVERY_PHRASE_VERIFICATION_STATUSES.NOTIFICATION
    )
      statusButtonType = 'attention';
    const statusButtonStyles = (0, classnames_1.default)([
      WalletRecoveryPhraseVerificationWidget_scss_1.default.statusButton,
      statusButtonType,
    ]);
    return react_1.default.createElement(
      'div',
      {
        className:
          WalletRecoveryPhraseVerificationWidget_scss_1.default.component,
      },
      react_1.default.createElement(
        'h2',
        null,
        intl.formatMessage(exports.messages.title)
      ),
      react_1.default.createElement(
        'div',
        null,
        intl.formatMessage(exports.messages.description, {
          wordCount,
        }),
        isLegacy &&
          react_1.default.createElement(
            react_1.default.Fragment,
            null,
            '\u00A0',
            react_1.default.createElement(
              PopOver_1.PopOver,
              {
                maxWidth: 700,
                content: react_1.default.createElement(
                  'div',
                  {
                    className:
                      WalletRecoveryPhraseVerificationWidget_scss_1.default
                        .paperWalletTooltip,
                  },
                  intl.formatMessage(exports.messages.paperWalletDescription)
                ),
              },
              react_1.default.createElement(
                'div',
                {
                  className:
                    WalletRecoveryPhraseVerificationWidget_scss_1.default
                      .paperWallet,
                },
                intl.formatMessage(exports.messages.paperWalletTitle)
              )
            )
          )
      ),
      react_1.default.createElement(
        'div',
        { className: statusStyles },
        react_1.default.createElement(react_svg_inline_1.default, {
          svg: icon,
          className:
            WalletRecoveryPhraseVerificationWidget_scss_1.default.statusIcon,
        }),
        react_1.default.createElement(react_intl_1.FormattedHTMLMessage, {
          ...message,
          values: {
            timeAgo,
            timeUntilWarning,
          },
        }),
        react_1.default.createElement(Button_1.Button, {
          className: statusButtonStyles,
          themeOverrides: WalletRecoveryPhraseVerificationWidget_scss_1.default,
          label: intl.formatMessage(exports.messages.button),
          onClick: onVerify,
          skin: ButtonSkin_1.ButtonSkin,
        })
      )
    );
  }
};
WalletRecoveryPhraseVerificationWidget = __decorate(
  [mobx_react_1.observer],
  WalletRecoveryPhraseVerificationWidget
);
exports.default = WalletRecoveryPhraseVerificationWidget;
//# sourceMappingURL=WalletRecoveryPhraseVerificationWidget.js.map
