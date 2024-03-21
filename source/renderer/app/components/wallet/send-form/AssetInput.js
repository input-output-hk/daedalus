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
const mobx_react_1 = require('mobx-react');
const react_intl_1 = require('react-intl');
const lodash_1 = require('lodash');
const classnames_1 = __importDefault(require('classnames'));
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
const NumericInput_1 = require('@react-polymorph/components/NumericInput');
const AmountInputSkin_1 = __importDefault(require('../skins/AmountInputSkin'));
const remove_inline_svg_1 = __importDefault(
  require('../../../assets/images/remove.inline.svg')
);
const discreet_mode_1 = require('../../../features/discreet-mode');
const Asset_1 = __importDefault(require('../../assets/Asset'));
const VerticalSeparator_1 = require('../widgets/VerticalSeparator');
const ClearButton_1 = require('../widgets/ClearButton');
const AssetInput_scss_1 = __importDefault(require('./AssetInput.scss'));
const messages_1 = __importDefault(require('./messages'));
const INPUT_FIELD_PADDING_DELTA = 10;
let AssetInput = class AssetInput extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  rightContentRef;
  constructor(props) {
    super(props);
    this.rightContentRef = react_1.default.createRef();
  }
  hasAssetValue = (asset) => {
    return (0, lodash_1.get)(asset, 'value', false);
  };
  generateInputFieldStyle = () => {
    const { current: rightContentDom } = this.rightContentRef;
    if (!rightContentDom) {
      return null;
    }
    const rightContentDomRect = rightContentDom.getBoundingClientRect();
    return {
      paddingRight: `${
        rightContentDomRect.width + INPUT_FIELD_PADDING_DELTA
      }px`,
    };
  };
  render() {
    const { intl } = this.context;
    const {
      uniqueId,
      getAssetByUniqueId,
      assetFields,
      addFocusableField,
      currentNumberFormat,
      removeAssetRow,
      handleSubmitOnEnter,
      clearAssetFieldValue,
      autoFocus,
    } = this.props;
    const asset = getAssetByUniqueId(uniqueId);
    if (!asset) {
      return false;
    }
    const { quantity, metadata, decimals } = asset;
    const ticker = (0, lodash_1.get)(metadata, 'ticker', null);
    const assetField = assetFields[uniqueId];
    const inputFieldStyle = this.generateInputFieldStyle();
    return react_1.default.createElement(
      'div',
      {
        key: `receiver_asset_${uniqueId}`,
        className: AssetInput_scss_1.default.component,
      },
      react_1.default.createElement(
        'div',
        { className: AssetInput_scss_1.default.inputBlock },
        quantity.isPositive() &&
          react_1.default.createElement(
            'div',
            { className: AssetInput_scss_1.default.amountTokenTotal },
            intl.formatMessage(messages_1.default.ofLabel),
            react_1.default.createElement(
              'span',
              { className: AssetInput_scss_1.default.amountValue },
              react_1.default.createElement(
                discreet_mode_1.DiscreetTokenWalletAmount,
                { amount: quantity, metadata: metadata, decimals: decimals }
              )
            )
          ),
        react_1.default.createElement(NumericInput_1.NumericInput, {
          ...assetField.bind(),
          ref: (field) => addFocusableField(field),
          placeholder: decimals
            ? `0${currentNumberFormat.decimalSeparator}${'0'.repeat(decimals)}`
            : '0',
          className: AssetInput_scss_1.default.assetItem,
          label: react_1.default.createElement(Asset_1.default, {
            asset: asset,
            hidePopOver: true,
            small: true,
          }),
          'data-testid': `assetInput:${uniqueId}`,
          bigNumberFormat: decimals ? currentNumberFormat : null,
          decimalPlaces: decimals,
          numberLocaleOptions: {
            minimumFractionDigits: decimals,
          },
          onChange: (value) => {
            assetField.onChange(value);
          },
          currency: ticker,
          value: assetField.value,
          error: assetField.error,
          skin: AmountInputSkin_1.default,
          style: inputFieldStyle,
          onKeyPress: (evt) => {
            if (decimals === 0) {
              const { charCode } = evt;
              if (charCode === 190 || charCode === 110 || charCode === 46) {
                evt.persist();
                evt.preventDefault();
                evt.stopPropagation();
              }
            }
            handleSubmitOnEnter(evt);
          },
          allowSigns: false,
          autoFocus: autoFocus,
        }),
        react_1.default.createElement(
          'div',
          {
            className: AssetInput_scss_1.default.rightContent,
            ref: this.rightContentRef,
          },
          this.hasAssetValue(assetField) &&
            react_1.default.createElement(
              'div',
              { className: AssetInput_scss_1.default.clearAssetContainer },
              react_1.default.createElement(ClearButton_1.ClearButton, {
                label: intl.formatMessage(messages_1.default.clearLabel),
                onClick: () => clearAssetFieldValue(assetField),
              })
            ),
          ticker
            ? react_1.default.createElement(
                react_1.default.Fragment,
                null,
                react_1.default.createElement(
                  VerticalSeparator_1.VerticalSeparator,
                  null
                ),
                react_1.default.createElement(
                  'span',
                  { className: AssetInput_scss_1.default.ticker },
                  ticker
                )
              )
            : null
        )
      ),
      react_1.default.createElement(
        'div',
        { className: AssetInput_scss_1.default.removeAssetBlock },
        react_1.default.createElement(
          'span',
          {
            className: (0, classnames_1.default)([
              AssetInput_scss_1.default.removeAssetButton,
              'flat',
            ]),
            onClick: () => removeAssetRow(uniqueId),
            'data-testid': `removeAsset:${uniqueId}`,
          },
          react_1.default.createElement(react_svg_inline_1.default, {
            svg: remove_inline_svg_1.default,
            className: AssetInput_scss_1.default.removeIcon,
          })
        )
      )
    );
  }
};
AssetInput = __decorate(
  [mobx_react_1.observer, __metadata('design:paramtypes', [Object])],
  AssetInput
);
exports.default = AssetInput;
//# sourceMappingURL=AssetInput.js.map
