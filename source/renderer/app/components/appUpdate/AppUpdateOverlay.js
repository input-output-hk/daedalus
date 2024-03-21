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
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
const classnames_1 = __importDefault(require('classnames'));
const lodash_1 = require('lodash');
const react_intl_1 = require('react-intl');
const Button_1 = require('@react-polymorph/components/Button');
const Checkbox_1 = require('@react-polymorph/components/Checkbox');
const CheckboxSkin_1 = require('@react-polymorph/skins/simple/CheckboxSkin');
const Link_1 = require('@react-polymorph/components/Link');
const LinkSkin_1 = require('@react-polymorph/skins/simple/LinkSkin');
const ButtonSkin_1 = require('@react-polymorph/skins/simple/ButtonSkin');
const ButtonSpinnerSkin_1 = require('@react-polymorph/skins/simple/ButtonSpinnerSkin');
const react_markdown_1 = __importDefault(require('react-markdown'));
const AppUpdateOverlay_scss_1 = __importDefault(
  require('./AppUpdateOverlay.scss')
);
const DialogCloseButton_1 = __importDefault(
  require('../widgets/DialogCloseButton')
);
const ProgressBarLarge_1 = __importDefault(
  require('../widgets/ProgressBarLarge')
);
const link_ic_inline_svg_1 = __importDefault(
  require('../../assets/images/link-ic.inline.svg')
);
const close_cross_thin_inline_svg_1 = __importDefault(
  require('../../assets/images/close-cross-thin.inline.svg')
);
const messages = (0, react_intl_1.defineMessages)({
  title: {
    id: 'appUpdate.overlay.title',
    defaultMessage: '!!!Software update available!',
    description: '"title" for the App Update Overlay',
  },
  subtitle: {
    id: 'appUpdate.overlay.subtitle',
    defaultMessage:
      '!!!You are currently running Daedalus version {currentAppVersion}.<br />Daedalus version {availableAppVersion} is now available to download.',
    description: '"subtitle" for the App Update Overlay',
  },
  checkboxLabel: {
    id: 'appUpdate.overlay.checkboxLabel',
    defaultMessage:
      '!!!I understand that I need to complete the installation before starting Daedalus.',
    description: '"checkboxLabel" for the App Update Overlay',
  },
  buttonLaunchInstallerLabel: {
    id: 'appUpdate.overlay.button.launchInstaller.label',
    defaultMessage: '!!!Quit Daedalus and start the installation',
    description: '"buttonLaunchInstallerLabel" for the App Update Overlay',
  },
  buttonInstallUpdateLabel: {
    id: 'appUpdate.overlay.button.installUpdate.label',
    defaultMessage: '!!!Install the update and restart Daedalus',
    description: '"buttonInstallUpdateLabel" for the App Update Overlay',
  },
  postponeInstallLinkLabel: {
    id: 'appUpdate.overlay.postponeInstall.link.label',
    defaultMessage: '!!!Postpone the update',
    description: '"manualUpdateLinkLabel" for the App Update Overlay',
  },
  installingUpdateLabel: {
    id: 'appUpdate.overlay.installingUpdate.link.label',
    defaultMessage: '!!!Installing update...',
    description: '"installingUpdateLabel" for the App Update Overlay',
  },
  downloadProgressLabel: {
    id: 'appUpdate.overlay.downloadProgressLabel',
    defaultMessage: '!!!Download in progress',
    description: '"downloadProgressLabel" for the App Update Overlay',
  },
  downloadTimeLeft: {
    id: 'appUpdate.overlay.downloadTimeLeft',
    defaultMessage: '!!!{downloadTimeLeft} left',
    description: '"downloadTimeLeft" for the App Update Overlay',
  },
  downloadProgressData: {
    id: 'appUpdate.overlay.downloadProgressData',
    defaultMessage: '!!!({totalDownloaded} of {totalDownloadSize} downloaded)',
    description: '"downloadProgressData" for the App Update Overlay',
  },
  manualUpdateDescriptionError: {
    id: 'appUpdate.overlay.manualUpdate.description.error',
    defaultMessage:
      '!!!We were unable to launch the update installer automatically.',
    description: '"manualUpdateDescriptionError" for the App Update Overlay',
  },
  manualUpdateDescriptionErrorLinux: {
    id: 'appUpdate.overlay.manualUpdate.description.errorLinux',
    defaultMessage: '!!!We were unable to install the update.',
    description:
      '"manualUpdateDescriptionErrorLinux" for the App Update Overlay',
  },
  manualUpdateDescriptionAction: {
    id: 'appUpdate.overlay.manualUpdate.description.action',
    defaultMessage: '!!!Please manually update Daedalus to its latest version.',
    description: '"manualUpdateDescriptionAction" for the App Update Overlay',
  },
  manualUpdateButtonLabel: {
    id: 'appUpdate.overlay.manualUpdate.button.label',
    defaultMessage: '!!!Follow instructions and manually update',
    description: '"manualUpdateButtonLabel" for the App Update Overlay',
  },
  manualUpdateButtonUrlForMainnet: {
    id: 'appUpdate.overlay.manualUpdate.button.url.mainnet',
    defaultMessage: '!!!https://daedaluswallet.io/en/download/',
    description:
      '"manualUpdateButtonUrl" for the App Update Overlay on Mainnet',
  },
  manualUpdateButtonUrlForFlight: {
    id: 'appUpdate.overlay.manualUpdate.button.url.flight',
    defaultMessage: '!!!https://daedaluswallet.io/en/flight/',
    description: '"manualUpdateButtonUrl" for the App Update Overlay on Flight',
  },
  manualUpdateButtonUrlForTestnet: {
    id: 'appUpdate.overlay.manualUpdate.button.url.testnet',
    defaultMessage:
      '!!!https://developers.cardano.org/en/testnets/cardano/get-started/wallet/',
    description:
      '"manualUpdateButtonUrl" for the App Update Overlay on Testnet',
  },
});
let AppUpdateOverlay = class AppUpdateOverlay extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  state = {
    areTermsOfUseAccepted: this.props.isLinux,
  };
  toggleAcceptance = () => {
    this.setState((prevState) => ({
      areTermsOfUseAccepted: !prevState.areTermsOfUseAccepted,
    }));
  };
  contentClickHandler(event) {
    const linkUrl = (0, lodash_1.get)(event, ['target', 'href']);
    if (linkUrl) {
      event.preventDefault();
      event.stopPropagation();
      this.props.onExternalLinkClick(linkUrl);
    }
  }
  progressActions = () => {
    const { intl } = this.context;
    const {
      downloadTimeLeft,
      totalDownloaded,
      totalDownloadSize,
      downloadProgress,
    } = this.props;
    return react_1.default.createElement(
      'div',
      { className: AppUpdateOverlay_scss_1.default.progressBar },
      react_1.default.createElement(ProgressBarLarge_1.default, {
        leftLabel: intl.formatMessage(messages.downloadProgressLabel),
        rightLabel1: intl.formatMessage(messages.downloadTimeLeft, {
          downloadTimeLeft,
        }),
        rightLabel2: intl.formatMessage(messages.downloadProgressData, {
          totalDownloaded,
          totalDownloadSize,
        }),
        progress: downloadProgress,
        isDarkMode: true,
      })
    );
  };
  openInstallerAction = () => {
    const { intl } = this.context;
    const {
      onInstallUpdate,
      onPostponeUpdate,
      isWaitingToQuitDaedalus,
      isLinux,
      installationProgress,
    } = this.props;
    const { areTermsOfUseAccepted } = this.state;
    const isCheckboxDisabled = isWaitingToQuitDaedalus;
    const checkboxStyles = (0, classnames_1.default)([
      AppUpdateOverlay_scss_1.default.checkbox,
      isCheckboxDisabled ? AppUpdateOverlay_scss_1.default.disabled : null,
    ]);
    const isButtonDisabled = !areTermsOfUseAccepted || isWaitingToQuitDaedalus;
    const buttonStyles = (0, classnames_1.default)([
      AppUpdateOverlay_scss_1.default.button,
      isButtonDisabled ? AppUpdateOverlay_scss_1.default.disabled : null,
    ]);
    const buttonLabel = isLinux
      ? messages.buttonInstallUpdateLabel
      : messages.buttonLaunchInstallerLabel;
    const postponeLinkStyles = (0, classnames_1.default)([
      AppUpdateOverlay_scss_1.default.postponeLink,
      !isLinux && isWaitingToQuitDaedalus
        ? AppUpdateOverlay_scss_1.default.disabled
        : null,
      isLinux && isWaitingToQuitDaedalus
        ? AppUpdateOverlay_scss_1.default.noLink
        : null,
    ]);
    const postponeLabel =
      isLinux && isWaitingToQuitDaedalus
        ? messages.installingUpdateLabel
        : messages.postponeInstallLinkLabel;
    const postponeAction = !isWaitingToQuitDaedalus
      ? onPostponeUpdate
      : () => {};
    const actionsStyles = (0, classnames_1.default)([
      !isLinux || !isWaitingToQuitDaedalus
        ? AppUpdateOverlay_scss_1.default.actions
        : null,
    ]);
    return react_1.default.createElement(
      'div',
      { className: actionsStyles },
      !isLinux &&
        react_1.default.createElement(Checkbox_1.Checkbox, {
          label: intl.formatMessage(messages.checkboxLabel),
          onChange: this.toggleAcceptance,
          className: checkboxStyles,
          checked: areTermsOfUseAccepted || isWaitingToQuitDaedalus,
          skin: CheckboxSkin_1.CheckboxSkin,
          themeOverrides: AppUpdateOverlay_scss_1.default.checkbox,
          disabled: isCheckboxDisabled,
        }),
      isLinux && isWaitingToQuitDaedalus
        ? react_1.default.createElement(
            'div',
            { className: AppUpdateOverlay_scss_1.default.progressBar },
            react_1.default.createElement(ProgressBarLarge_1.default, {
              progress: installationProgress,
              // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
              labelLeft: intl.formatMessage(messages.installingUpdateLabel),
              isDarkMode: true,
            })
          )
        : react_1.default.createElement(
            react_1.default.Fragment,
            null,
            react_1.default.createElement(Button_1.Button, {
              className: buttonStyles,
              onClick: onInstallUpdate,
              skin: ButtonSpinnerSkin_1.ButtonSpinnerSkin,
              loading: isWaitingToQuitDaedalus,
              label: intl.formatMessage(buttonLabel),
              disabled: isButtonDisabled,
            }),
            react_1.default.createElement(Link_1.Link, {
              className: postponeLinkStyles,
              onClick: postponeAction,
              label: intl.formatMessage(postponeLabel),
              skin: LinkSkin_1.LinkSkin,
              hasIconAfter: false,
            })
          )
    );
  };
  manualUpdateAction = () => {
    const { intl } = this.context;
    const {
      onExternalLinkClick,
      onPostponeUpdate,
      isLinux,
      isFlight,
      isTestnet,
    } = this.props;
    const errorMessage = isLinux
      ? messages.manualUpdateDescriptionErrorLinux
      : messages.manualUpdateDescriptionError;
    let manualUpdateButtonUrl = intl.formatMessage(
      messages.manualUpdateButtonUrlForMainnet
    );
    if (isTestnet) {
      manualUpdateButtonUrl = intl.formatMessage(
        messages.manualUpdateButtonUrlForTestnet
      );
    }
    if (isFlight) {
      manualUpdateButtonUrl = intl.formatMessage(
        messages.manualUpdateButtonUrlForFlight
      );
    }
    return react_1.default.createElement(
      'div',
      { className: AppUpdateOverlay_scss_1.default.actions },
      react_1.default.createElement(
        'div',
        { className: AppUpdateOverlay_scss_1.default.manualUpdateDescription },
        react_1.default.createElement(
          'p',
          null,
          intl.formatMessage(errorMessage)
        ),
        react_1.default.createElement(
          'p',
          null,
          intl.formatMessage(messages.manualUpdateDescriptionAction)
        )
      ),
      react_1.default.createElement(Button_1.Button, {
        className: AppUpdateOverlay_scss_1.default.button,
        onClick: () => onExternalLinkClick(manualUpdateButtonUrl),
        skin: ButtonSkin_1.ButtonSkin,
        label: react_1.default.createElement(
          'span',
          null,
          react_1.default.createElement(react_svg_inline_1.default, {
            svg: link_ic_inline_svg_1.default,
            className: AppUpdateOverlay_scss_1.default.externalLinkIcon,
          }),
          intl.formatMessage(messages.manualUpdateButtonLabel)
        ),
      }),
      react_1.default.createElement(Link_1.Link, {
        className: AppUpdateOverlay_scss_1.default.postponeLink,
        onClick: onPostponeUpdate,
        label: intl.formatMessage(messages.postponeInstallLinkLabel),
        skin: LinkSkin_1.LinkSkin,
        hasIconAfter: false,
      })
    );
  };
  render() {
    const { intl } = this.context;
    const {
      update,
      onClose,
      isUpdateDownloaded,
      availableAppVersion,
      currentAppVersion,
      isAutomaticUpdateFailed,
    } = this.props;
    const { content } = update;
    let actions;
    if (isAutomaticUpdateFailed) actions = this.manualUpdateAction();
    else if (!isUpdateDownloaded) actions = this.progressActions();
    else actions = this.openInstallerAction();
    return react_1.default.createElement(
      'div',
      {
        className: AppUpdateOverlay_scss_1.default.component,
        role: 'presentation',
        onClick: !isUpdateDownloaded ? onClose : () => {},
      },
      !isUpdateDownloaded &&
        !isAutomaticUpdateFailed &&
        react_1.default.createElement(DialogCloseButton_1.default, {
          onClose: onClose,
          icon: close_cross_thin_inline_svg_1.default,
          className: AppUpdateOverlay_scss_1.default.closeButton,
        }),
      react_1.default.createElement(
        'h1',
        { className: AppUpdateOverlay_scss_1.default.title },
        intl.formatMessage(messages.title)
      ),
      react_1.default.createElement(
        'span',
        { className: AppUpdateOverlay_scss_1.default.subtitle },
        react_1.default.createElement(react_intl_1.FormattedHTMLMessage, {
          ...messages.subtitle,
          values: {
            availableAppVersion,
            currentAppVersion,
          },
        })
      ),
      react_1.default.createElement(
        'div',
        {
          className: AppUpdateOverlay_scss_1.default.content,
          role: 'presentation',
          onClick: this.contentClickHandler.bind(this),
        },
        react_1.default.createElement(react_markdown_1.default, {
          escapeHtml: false,
          source: content,
        })
      ),
      actions
    );
  }
};
AppUpdateOverlay = __decorate([mobx_react_1.observer], AppUpdateOverlay);
exports.default = AppUpdateOverlay;
//# sourceMappingURL=AppUpdateOverlay.js.map
