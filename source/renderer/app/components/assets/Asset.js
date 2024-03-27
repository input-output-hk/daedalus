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
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
const classnames_1 = __importDefault(require('classnames'));
const PopOver_1 = require('@react-polymorph/components/PopOver');
const react_intl_1 = require('react-intl');
const mobx_react_1 = require('mobx-react');
const Asset_scss_1 = __importDefault(require('./Asset.scss'));
const strings_1 = require('../../utils/strings');
const AssetContent_1 = __importDefault(require('./AssetContent'));
const asset_token_settings_ic_inline_svg_1 = __importDefault(
  require('../../assets/images/asset-token-settings-ic.inline.svg')
);
const asset_token_warning_ic_inline_svg_1 = __importDefault(
  require('../../assets/images/asset-token-warning-ic.inline.svg')
);
const timingConfig_1 = require('../../config/timingConfig');
const messages = (0, react_intl_1.defineMessages)({
  fingerprintItem: {
    id: 'assets.assetToken.param.fingerprint',
    defaultMessage: '!!!Fingerprint',
    description: '"fingerprint" item.',
  },
  policyIdItem: {
    id: 'assets.assetToken.param.policyId',
    defaultMessage: '!!!Policy Id',
    description: '"policyId" item.',
  },
  assetNameItem: {
    id: 'assets.assetToken.param.assetName',
    defaultMessage: '!!!Asset name',
    description: '"assetName" item.',
  },
  nameItem: {
    id: 'assets.assetToken.param.name',
    defaultMessage: '!!!Name',
    description: '"name" item.',
  },
  tickerItem: {
    id: 'assets.assetToken.param.ticker',
    defaultMessage: '!!!Ticker',
    description: '"ticker" item.',
  },
  descriptionItem: {
    id: 'assets.assetToken.param.description',
    defaultMessage: '!!!Description',
    description: '"description" item.',
  },
  blank: {
    id: 'assets.assetToken.param.blank',
    defaultMessage: '!!!Blank',
    description: '"Blank" item value.',
  },
  settingsCogPopOver: {
    id: 'assets.assetToken.settings.cogPopOver',
    defaultMessage:
      '!!!You can configure the number of decimal places for this native token.',
    description: 'Asset settings pop over content',
  },
  settingsWarningPopOverAvailable: {
    id: 'assets.warning.available',
    defaultMessage:
      '!!!Recommended configuration for decimal places for this native token is available.',
    description: 'Asset settings recommended pop over content',
  },
  settingsWarningPopOverNotUsing: {
    id: 'assets.warning.notUsing',
    defaultMessage:
      '!!!You are not using the recommended decimal place configuration for this native token.',
    description: 'Asset settings recommended pop over content',
  },
});
let Asset = class Asset extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'TimeoutID'.
  displayDelayTimeout;
  state = {
    isPillPopOverVisible: false,
    isHoveringSettingsIcon: false,
  };
  // We need to track the mounted state in order to avoid calling
  // setState promise handling code after the component was already unmounted:
  // Read more: https://facebook.github.io/react/blog/2015/12/16/ismounted-antipattern.html
  _isMounted = false;
  componentDidMount() {
    this._isMounted = true;
  }
  componentWillUnmount() {
    this._isMounted = false;
  }
  handleShowPillPopOver = () => {
    clearTimeout(this.displayDelayTimeout);
    this.displayDelayTimeout = setTimeout(() => {
      if (this._isMounted) {
        this.setState({
          isPillPopOverVisible: true,
        });
      }
    }, timingConfig_1.ASSET_TOKEN_DISPLAY_DELAY);
  };
  handleHidePillPopOver = () => {
    clearTimeout(this.displayDelayTimeout);
    this.displayDelayTimeout = setTimeout(() => {
      if (this._isMounted) {
        this.setState({
          isPillPopOverVisible: false,
        });
      }
    }, timingConfig_1.ASSET_TOKEN_DISPLAY_DELAY);
  };
  handleSettingsMouseEnter = () => {
    this.setState({
      isHoveringSettingsIcon: true,
    });
  };
  handleSettingsMouseLeave = () => {
    this.setState({
      isHoveringSettingsIcon: false,
    });
  };
  get isSettingsPopOverVisible() {
    const { assetSettingsDialogWasOpened, anyAssetWasHovered } = this.props;
    const { isHoveringSettingsIcon } = this.state;
    if (isHoveringSettingsIcon) {
      return true;
    }
    if (
      assetSettingsDialogWasOpened === false &&
      anyAssetWasHovered === false
    ) {
      return true;
    }
    return false;
  }
  renderPillContent() {
    const { intl } = this.context;
    const {
      asset,
      metadataNameChars,
      small = true,
      fullFingerprint,
      hasWarning,
      hasError,
    } = this.props;
    const {
      fingerprint,
      metadata,
      decimals,
      recommendedDecimals,
      assetName,
    } = asset;
    const hasMetadataName = !!metadata?.name;
    const name =
      metadata?.name ||
      (assetName && `ASCII: ${(0, strings_1.hexToString)(assetName)}`) ||
      '';
    const displayName = metadataNameChars
      ? (0, strings_1.ellipsis)(name, metadataNameChars)
      : name;
    const contentStyles = (0, classnames_1.default)([
      Asset_scss_1.default.pill,
      hasError ? Asset_scss_1.default.error : null,
    ]);
    const [startCharAmount, endCharAmount] = small ? [9, 4] : [12, 12];
    let warningPopOverMessage;
    if (hasWarning) {
      warningPopOverMessage =
        typeof decimals === 'number'
          ? messages.settingsWarningPopOverNotUsing
          : messages.settingsWarningPopOverAvailable;
    }
    return react_1.default.createElement(
      'div',
      { className: contentStyles },
      react_1.default.createElement(
        'div',
        { className: Asset_scss_1.default.fingerprint },
        fullFingerprint
          ? fingerprint
          : (0, strings_1.ellipsis)(
              fingerprint || '',
              startCharAmount,
              endCharAmount
            )
      ),
      displayName &&
        react_1.default.createElement(
          'div',
          {
            'data-testid': 'assetName',
            className: (0, classnames_1.default)(
              Asset_scss_1.default.metadataName,
              !hasMetadataName && Asset_scss_1.default.ascii
            ),
          },
          displayName
        ),
      hasWarning &&
        react_1.default.createElement(
          'div',
          { className: Asset_scss_1.default.warningIconWrapper },
          react_1.default.createElement(
            PopOver_1.PopOver,
            {
              content: intl.formatMessage(warningPopOverMessage, {
                recommendedDecimals,
              }),
              className: Asset_scss_1.default.warningIconWrapper,
            },
            react_1.default.createElement(
              'span',
              { 'data-testid': 'warning-icon' },
              react_1.default.createElement(react_svg_inline_1.default, {
                className: Asset_scss_1.default.warningIcon,
                svg: asset_token_warning_ic_inline_svg_1.default,
              })
            )
          )
        )
    );
  }
  renderPillPopOverContainer = () => {
    const { asset, onCopyAssetParam } = this.props;
    const pillContent = this.renderPillContent();
    const popOverContent = react_1.default.createElement(
      AssetContent_1.default,
      {
        asset: asset,
        onCopyAssetParam: onCopyAssetParam,
        className: Asset_scss_1.default.popOverContent,
        highlightFingerprint: true,
      }
    );
    const { isPillPopOverVisible } = this.state;
    return react_1.default.createElement(
      'div',
      {
        className: Asset_scss_1.default.popOverContainer,
        onMouseEnter: this.handleShowPillPopOver,
        onMouseLeave: this.handleHidePillPopOver,
      },
      react_1.default.createElement(
        PopOver_1.PopOver,
        {
          themeVariables: {
            '--rp-pop-over-bg-color':
              'var(--theme-widgets-asset-token-background-color)',
            '--rp-pop-over-text-color': 'var(--theme-bordered-box-text-color)',
            '--rp-pop-over-border-color':
              'var(--theme-staking-stake-pool-tooltip-border-color)',
            '--rp-pop-over-border-width': '1px',
            '--rp-pop-over-border-style': 'solid',
            '--rp-pop-over-box-shadow':
              '0 5px 20px 0 var(--theme-widgets-asset-token-box-shadow)',
          },
          content: popOverContent,
          visible: isPillPopOverVisible,
          appendTo: 'parent',
          maxWidth: 376,
          allowHTML: true,
          interactive: true,
        },
        pillContent
      )
    );
  };
  renderSettingsContent = () => {
    const { intl } = this.context;
    const { asset, onClickSettings, hasWarning } = this.props;
    if (!onClickSettings) return null;
    const {
      isSettingsPopOverVisible,
      handleSettingsMouseEnter,
      handleSettingsMouseLeave,
    } = this;
    const onClickSettingsBind = () => onClickSettings && onClickSettings(asset);
    const { decimals, recommendedDecimals } = asset;
    let warningPopOverMessage;
    if (hasWarning) {
      warningPopOverMessage =
        typeof decimals === 'number'
          ? messages.settingsWarningPopOverNotUsing
          : messages.settingsWarningPopOverAvailable;
    }
    return react_1.default.createElement(
      'button',
      {
        className: Asset_scss_1.default.settingsButton,
        onClick: onClickSettingsBind,
      },
      react_1.default.createElement(
        PopOver_1.PopOver,
        {
          content: intl.formatMessage(messages.settingsCogPopOver),
          visible: isSettingsPopOverVisible,
        },
        react_1.default.createElement(
          react_svg_inline_1.default,
          // @ts-ignore ts-migrate(2322) FIXME: Type '{ onMouseEnter: () => void; onMouseLeave: ()... Remove this comment to see the full error message
          {
            // @ts-ignore ts-migrate(2322) FIXME: Type '{ onMouseEnter: () => void; onMouseLeave: ()... Remove this comment to see the full error message
            onMouseEnter: handleSettingsMouseEnter,
            onMouseLeave: handleSettingsMouseLeave,
            className: Asset_scss_1.default.settingsIcon,
            svg: asset_token_settings_ic_inline_svg_1.default,
          }
        )
      ),
      hasWarning &&
        react_1.default.createElement(
          PopOver_1.PopOver,
          {
            content: intl.formatMessage(warningPopOverMessage, {
              recommendedDecimals,
            }),
          },
          react_1.default.createElement(react_svg_inline_1.default, {
            className: Asset_scss_1.default.warningIcon,
            svg: asset_token_warning_ic_inline_svg_1.default,
          })
        )
    );
  };
  render() {
    const { hidePopOver, className } = this.props;
    const content = hidePopOver
      ? this.renderPillContent()
      : this.renderPillPopOverContainer();
    const settingsContent = this.renderSettingsContent();
    const componentClassnames = (0, classnames_1.default)([
      Asset_scss_1.default.component,
      className,
    ]);
    return react_1.default.createElement(
      'div',
      { className: componentClassnames },
      content,
      settingsContent
    );
  }
};
Asset = __decorate([mobx_react_1.observer], Asset);
exports.default = Asset;
//# sourceMappingURL=Asset.js.map
