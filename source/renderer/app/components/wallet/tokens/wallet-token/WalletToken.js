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
const mobx_react_1 = require('mobx-react');
const classnames_1 = __importDefault(require('classnames'));
const WalletToken_scss_1 = __importDefault(require('./WalletToken.scss'));
const AssetContent_1 = __importDefault(require('../../../assets/AssetContent'));
const WalletTokenFooter_1 = __importDefault(require('./WalletTokenFooter'));
const WalletTokenHeader_1 = __importDefault(require('./WalletTokenHeader'));
const helpers_1 = require('./helpers');
const WalletToken = (0, mobx_react_1.observer)((props) => {
  const {
    anyAssetWasHovered,
    asset,
    assetSettingsDialogWasOpened,
    className,
    headerClassName,
    footerClassName,
    fullFingerprint = true,
    isFavorite,
    isLoading,
    onAssetSettings,
    onCopyAssetParam,
    onOpenAssetSend,
    onToggleFavorite,
    isInsertingAsset,
    isRemovingAsset,
  } = props;
  const [isExpanded, setIsExpanded] = (0, react_1.useState)(false);
  const toggleIsExpanded = (0, react_1.useCallback)(() => {
    setIsExpanded(!isExpanded);
  }, [setIsExpanded, isExpanded]);
  const hasWarning = (0, helpers_1.isNonRecommendedDecimalSettingUsed)({
    decimals: asset.decimals,
    recommendedDecimals: asset.recommendedDecimals,
  });
  const componentStyles = (0, react_1.useMemo)(
    () =>
      (0, classnames_1.default)(
        WalletToken_scss_1.default.component,
        isExpanded && WalletToken_scss_1.default.isExpanded,
        isInsertingAsset && WalletToken_scss_1.default.inserting,
        isRemovingAsset && WalletToken_scss_1.default.removing,
        className
      ),
    [
      className,
      WalletToken_scss_1.default,
      isExpanded,
      isInsertingAsset,
      isRemovingAsset,
    ]
  );
  return react_1.default.createElement(
    'div',
    { className: componentStyles },
    react_1.default.createElement(WalletTokenHeader_1.default, {
      asset: asset,
      className: headerClassName,
      isFavorite: isFavorite,
      isExpanded: isExpanded,
      isLoading: isLoading,
      fullFingerprint: fullFingerprint,
      anyAssetWasHovered: anyAssetWasHovered,
      onClick: toggleIsExpanded,
      onCopyAssetParam: onCopyAssetParam,
      onToggleFavorite: onToggleFavorite,
      assetSettingsDialogWasOpened: assetSettingsDialogWasOpened,
      hasWarning: hasWarning,
    }),
    react_1.default.createElement(
      'div',
      { className: WalletToken_scss_1.default.content },
      react_1.default.createElement(AssetContent_1.default, {
        asset: asset,
        onCopyAssetParam: onCopyAssetParam,
        highlightFingerprint: false,
      }),
      react_1.default.createElement(WalletTokenFooter_1.default, {
        asset: asset,
        className: footerClassName,
        isLoading: isLoading,
        onAssetSettings: onAssetSettings,
        onOpenAssetSend: onOpenAssetSend,
        hasWarning: hasWarning,
      })
    )
  );
});
exports.default = WalletToken;
//# sourceMappingURL=WalletToken.js.map
