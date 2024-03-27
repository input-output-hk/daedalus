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
const classnames_1 = __importDefault(require('classnames'));
const react_intl_1 = require('react-intl');
const react_copy_to_clipboard_1 = __importDefault(
  require('react-copy-to-clipboard')
);
const mobx_react_1 = require('mobx-react');
const AssetContent_scss_1 = __importDefault(require('./AssetContent.scss'));
const strings_1 = require('../../utils/strings');
const copy_asset_inline_svg_1 = __importDefault(
  require('../../assets/images/copy-asset.inline.svg')
);
const check_w_inline_svg_1 = __importDefault(
  require('../../assets/images/check-w.inline.svg')
);
const timingConfig_1 = require('../../config/timingConfig');
const messages = (0, react_intl_1.defineMessages)({
  fingerprintAssetParam: {
    id: 'assets.assetToken.param.fingerprint',
    defaultMessage: '!!!Fingerprint',
    description: '"fingerprint" param.',
  },
  policyIdAssetParam: {
    id: 'assets.assetToken.param.policyId',
    defaultMessage: '!!!Policy Id',
    description: '"policyId" param.',
  },
  assetNameAssetParam: {
    id: 'assets.assetToken.param.assetName',
    defaultMessage: '!!!Asset name',
    description: '"assetName" param.',
  },
  nameAssetParam: {
    id: 'assets.assetToken.param.name',
    defaultMessage: '!!!Name',
    description: '"name" param.',
  },
  tickerAssetParam: {
    id: 'assets.assetToken.param.ticker',
    defaultMessage: '!!!Ticker',
    description: '"ticker" param.',
  },
  descriptionAssetParam: {
    id: 'assets.assetToken.param.description',
    defaultMessage: '!!!Description',
    description: '"description" param.',
  },
  blank: {
    id: 'assets.assetToken.param.blank',
    defaultMessage: '!!!Blank',
    description: '"Blank" param value.',
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
const AssetContent = (0, mobx_react_1.observer)((props) => {
  const [paramCopied, setParamCopied] = (0, react_1.useState)(null);
  // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'TimeoutID'.
  let copyNotificationTimeout;
  const handleCopyParam = (newParamCopied, param, fullValue) => {
    const { onCopyAssetParam } = props;
    if (onCopyAssetParam) {
      onCopyAssetParam({
        param,
        fullValue,
      });
    }
    clearTimeout(copyNotificationTimeout);
    setParamCopied(newParamCopied);
    copyNotificationTimeout = setTimeout(() => {
      setParamCopied(null);
    }, timingConfig_1.ASSET_TOKEN_ID_COPY_FEEDBACK);
  };
  const renderAssetParam = (assetId, param, value) => {
    const icon =
      paramCopied === assetId
        ? check_w_inline_svg_1.default
        : copy_asset_inline_svg_1.default;
    const iconClassnames = (0, classnames_1.default)([
      AssetContent_scss_1.default.copyIcon,
      paramCopied === assetId ? AssetContent_scss_1.default.copiedIcon : null,
    ]);
    const onCopy = () => {
      handleCopyParam(assetId, param, value);
    };
    return react_1.default.createElement(
      react_copy_to_clipboard_1.default,
      { text: value, onCopy: onCopy },
      react_1.default.createElement(
        'div',
        { className: AssetContent_scss_1.default.assetParam },
        react_1.default.createElement(
          'div',
          { className: AssetContent_scss_1.default.value },
          value,
          react_1.default.createElement(react_svg_inline_1.default, {
            svg: icon,
            className: iconClassnames,
          })
        ),
        assetId === 'assetName' &&
          react_1.default.createElement(
            'div',
            { className: AssetContent_scss_1.default.assetAsciiName },
            '(ASCII: ',
            (0, strings_1.hexToString)(value),
            ')'
          )
      )
    );
  };
  const { asset, highlightFingerprint, className, intl, hasError } = props;
  const { fingerprint, policyId, assetName, metadata } = asset;
  const { name, ticker, description } = metadata || {};
  const componentStyles = (0, classnames_1.default)([
    AssetContent_scss_1.default.component,
    className,
    highlightFingerprint
      ? AssetContent_scss_1.default.highlightFingerprint
      : null,
    hasError ? AssetContent_scss_1.default.error : null,
  ]);
  return react_1.default.createElement(
    'div',
    { className: componentStyles },
    highlightFingerprint &&
      react_1.default.createElement(
        'div',
        { className: AssetContent_scss_1.default.fingerprint },
        renderAssetParam(
          'fingerprint',
          intl.formatMessage(messages.fingerprintAssetParam),
          fingerprint
        )
      ),
    react_1.default.createElement(
      'dl',
      null,
      !highlightFingerprint &&
        react_1.default.createElement(
          react_1.default.Fragment,
          null,
          react_1.default.createElement(
            'dt',
            null,
            intl.formatMessage(messages.fingerprintAssetParam)
          ),
          react_1.default.createElement(
            'dd',
            null,
            renderAssetParam(
              'fingerprint',
              intl.formatMessage(messages.fingerprintAssetParam),
              fingerprint
            )
          )
        ),
      ticker &&
        react_1.default.createElement(
          react_1.default.Fragment,
          null,
          react_1.default.createElement(
            'dt',
            null,
            intl.formatMessage(messages.tickerAssetParam)
          ),
          react_1.default.createElement(
            'dd',
            null,
            renderAssetParam(
              'ticker',
              intl.formatMessage(messages.tickerAssetParam),
              ticker
            )
          )
        ),
      name &&
        react_1.default.createElement(
          react_1.default.Fragment,
          null,
          react_1.default.createElement(
            'dt',
            null,
            intl.formatMessage(messages.nameAssetParam)
          ),
          react_1.default.createElement(
            'dd',
            null,
            renderAssetParam(
              'name',
              intl.formatMessage(messages.nameAssetParam),
              name
            )
          )
        ),
      description &&
        react_1.default.createElement(
          react_1.default.Fragment,
          null,
          react_1.default.createElement(
            'dt',
            null,
            intl.formatMessage(messages.descriptionAssetParam)
          ),
          react_1.default.createElement(
            'dd',
            null,
            renderAssetParam(
              'description',
              intl.formatMessage(messages.descriptionAssetParam),
              description
            )
          )
        ),
      react_1.default.createElement(
        'dt',
        null,
        intl.formatMessage(messages.policyIdAssetParam)
      ),
      react_1.default.createElement(
        'dd',
        null,
        renderAssetParam(
          'policyId',
          intl.formatMessage(messages.policyIdAssetParam),
          policyId
        )
      ),
      react_1.default.createElement(
        'dt',
        null,
        intl.formatMessage(messages.assetNameAssetParam)
      ),
      react_1.default.createElement(
        'dd',
        null,
        assetName
          ? renderAssetParam(
              'assetName',
              intl.formatMessage(messages.assetNameAssetParam),
              assetName
            )
          : react_1.default.createElement(
              'span',
              { className: AssetContent_scss_1.default.blankValue },
              intl.formatMessage(messages.blank)
            )
      )
    )
  );
});
exports.default = (0, react_intl_1.injectIntl)(AssetContent);
//# sourceMappingURL=AssetContent.js.map
