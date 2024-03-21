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
var __metadata =
  (this && this.__metadata) ||
  function (k, v) {
    if (typeof Reflect === 'object' && typeof Reflect.metadata === 'function')
      return Reflect.metadata(k, v);
  };
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const react_1 = __importStar(require('react'));
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
const lodash_1 = require('lodash');
const mobx_react_1 = require('mobx-react');
const Select_1 = require('@react-polymorph/components/Select');
const PopOver_1 = require('@react-polymorph/components/PopOver');
const react_intl_1 = require('react-intl');
const Asset_1 = __importDefault(require('./Asset'));
const DialogCloseButton_1 = __importDefault(
  require('../widgets/DialogCloseButton')
);
const Dialog_1 = __importDefault(require('../widgets/Dialog'));
const AssetSettingsDialog_scss_1 = __importDefault(
  require('./AssetSettingsDialog.scss')
);
const global_messages_1 = __importDefault(
  require('../../i18n/global-messages')
);
const asset_token_warning_ic_inline_svg_1 = __importDefault(
  require('../../assets/images/asset-token-warning-ic.inline.svg')
);
const assetsConfig_1 = require('../../config/assetsConfig');
const discreet_mode_1 = require('../../features/discreet-mode');
const helpers_1 = require('../wallet/tokens/wallet-token/helpers');
const messages = (0, react_intl_1.defineMessages)({
  title: {
    id: 'assets.settings.dialog.title',
    defaultMessage: '!!!Native token settings',
    description: '"title" for the Asset settings dialog',
  },
  description: {
    id: 'assets.settings.dialog.description',
    defaultMessage:
      '!!!Updates made here will be applied in other wallets containing this token too.',
    description: '"description" for the Asset settings dialog',
  },
  formattedBalanceLabel: {
    id: 'assets.settings.dialog.formattedAmount.label',
    defaultMessage: '!!!Unformatted amount',
    description: '"formattedBalanceLabel" for the Asset settings dialog',
  },
  unformattedBalanceLabel: {
    id: 'assets.settings.dialog.unformattedAmount.label',
    defaultMessage: '!!!Formatted amount',
    description: '"unformattedBalanceLabel" for the Asset settings dialog',
  },
  decimalPrecisionLabel: {
    id: 'assets.settings.dialog.decimalPrecision.label',
    defaultMessage: '!!!Number of decimal places',
    description: '"decimalPrecisionLabel" for the Asset settings dialog',
  },
  recommended: {
    id: 'assets.settings.dialog.recommended',
    defaultMessage: '!!!(recommended)',
    description: '"recommended" for the Asset settings dialog',
  },
  default: {
    id: 'assets.settings.dialog.default',
    defaultMessage: '!!!(default)',
    description: '"default" for the Asset settings dialog',
  },
  warningPopOverAvailable: {
    id: 'assets.warning.available',
    defaultMessage:
      '!!!Recommended configuration for decimal places for this native token is available.',
    description: 'Asset settings recommended pop over content',
  },
  warningPopOverNotUsing: {
    id: 'assets.warning.notUsing',
    defaultMessage:
      '!!!You are not using the recommended decimal place configuration for this native token.',
    description: 'Asset settings recommended pop over content',
  },
});
let AssetSettingsDialog = class AssetSettingsDialog extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  constructor(props) {
    super(props);
    const { asset } = props;
    const { decimals: savedDecimals, recommendedDecimals } = asset;
    const hasSavedDecimals = typeof savedDecimals === 'number';
    this.state = {
      decimals: hasSavedDecimals
        ? savedDecimals
        : recommendedDecimals || assetsConfig_1.DEFAULT_DECIMAL_PRECISION,
    };
  }
  onSetDecimalPrecision = (decimals) => {
    this.setState({
      decimals,
    });
  };
  optionRenderer = (props) => {
    const { value } = props;
    const { intl } = this.context;
    const { recommendedDecimals } = this.props.asset;
    let extraLabel;
    if (
      typeof recommendedDecimals === 'number' &&
      recommendedDecimals === value
    ) {
      extraLabel = messages.recommended;
    } else if (value === assetsConfig_1.DEFAULT_DECIMAL_PRECISION) {
      extraLabel = messages.default;
    }
    if (extraLabel) {
      return react_1.default.createElement(
        'div',
        null,
        value,
        ' ',
        react_1.default.createElement('i', null, intl.formatMessage(extraLabel))
      );
    }
    return value;
  };
  selectionRenderer = (props) =>
    react_1.default.createElement(
      'div',
      { className: AssetSettingsDialog_scss_1.default.selection },
      this.optionRenderer(props)
    );
  render() {
    const { intl } = this.context;
    const { onCancel, onSubmit, asset } = this.props;
    const { decimals: savedDecimals, recommendedDecimals } = asset;
    const { decimals } = this.state;
    const hasSavedDecimals = typeof savedDecimals === 'number';
    const options = (0, lodash_1.range)(
      assetsConfig_1.MAX_DECIMAL_PRECISION + 1
    ).map((value) => ({
      value,
    }));
    const actions = [
      {
        label: intl.formatMessage(global_messages_1.default.cancel),
        onClick: onCancel,
      },
      {
        label: intl.formatMessage(global_messages_1.default.save),
        primary: true,
        disabled:
          (hasSavedDecimals && decimals === savedDecimals) ||
          (!hasSavedDecimals &&
            decimals === assetsConfig_1.DEFAULT_DECIMAL_PRECISION),
        onClick: () => onSubmit(asset, decimals),
      },
    ];
    const hasWarning = (0, helpers_1.isNonRecommendedDecimalSettingUsed)({
      recommendedDecimals,
      decimals: savedDecimals,
    });
    let warningPopOverMessage;
    if (hasWarning) {
      warningPopOverMessage = hasSavedDecimals
        ? messages.warningPopOverNotUsing
        : messages.warningPopOverAvailable;
    }
    return react_1.default.createElement(
      Dialog_1.default,
      {
        className: AssetSettingsDialog_scss_1.default.component,
        title: intl.formatMessage(messages.title),
        subtitle: react_1.default.createElement(Asset_1.default, {
          asset: asset,
          small: false,
          className: AssetSettingsDialog_scss_1.default.assetToken,
        }),
        actions: actions,
        closeOnOverlayClick: true,
        onClose: onCancel,
        closeButton: react_1.default.createElement(
          DialogCloseButton_1.default,
          null
        ),
        // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
        className1: AssetSettingsDialog_scss_1.default.dialog,
      },
      react_1.default.createElement(
        'div',
        null,
        react_1.default.createElement(
          'div',
          { className: AssetSettingsDialog_scss_1.default.description },
          react_1.default.createElement(react_intl_1.FormattedHTMLMessage, {
            ...messages.description,
          })
        ),
        react_1.default.createElement(
          'div',
          { className: AssetSettingsDialog_scss_1.default.label },
          intl.formatMessage(messages.unformattedBalanceLabel)
        ),
        react_1.default.createElement(
          'p',
          null,
          react_1.default.createElement(
            discreet_mode_1.DiscreetTokenWalletAmount,
            { amount: asset.quantity, decimals: 0 }
          )
        ),
        react_1.default.createElement(
          'div',
          { className: AssetSettingsDialog_scss_1.default.label },
          intl.formatMessage(messages.formattedBalanceLabel)
        ),
        react_1.default.createElement(
          'p',
          null,
          react_1.default.createElement(
            discreet_mode_1.DiscreetTokenWalletAmount,
            {
              amount: asset.quantity,
              metadata: asset.metadata,
              decimals: decimals,
            }
          )
        ),
        react_1.default.createElement(Select_1.Select, {
          options: options,
          value: decimals,
          label: react_1.default.createElement(
            'span',
            {
              className:
                AssetSettingsDialog_scss_1.default.decimalsDropdownLabel,
            },
            intl.formatMessage(messages.decimalPrecisionLabel),
            hasWarning &&
              react_1.default.createElement(
                PopOver_1.PopOver,
                {
                  content: intl.formatMessage(warningPopOverMessage, {
                    recommendedDecimals,
                  }),
                },
                react_1.default.createElement(
                  'span',
                  { 'data-testid': 'warning-icon' },
                  react_1.default.createElement(react_svg_inline_1.default, {
                    className: AssetSettingsDialog_scss_1.default.warningIcon,
                    svg: asset_token_warning_ic_inline_svg_1.default,
                  })
                )
              )
          ),
          onChange: this.onSetDecimalPrecision,
          optionRenderer: this.optionRenderer,
          selectionRenderer: this.selectionRenderer,
        })
      )
    );
  }
};
AssetSettingsDialog = __decorate(
  [mobx_react_1.observer, __metadata('design:paramtypes', [Object])],
  AssetSettingsDialog
);
exports.default = AssetSettingsDialog;
//# sourceMappingURL=AssetSettingsDialog.js.map
