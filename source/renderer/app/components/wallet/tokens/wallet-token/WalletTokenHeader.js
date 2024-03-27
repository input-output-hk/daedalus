'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const react_1 = __importDefault(require('react'));
const mobx_react_1 = require('mobx-react');
const classnames_1 = __importDefault(require('classnames'));
const lodash_1 = require('lodash');
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
const WalletTokenHeader_scss_1 = __importDefault(
  require('./WalletTokenHeader.scss')
);
const Asset_1 = __importDefault(require('../../../assets/Asset'));
const AssetAmount_1 = __importDefault(require('../../../assets/AssetAmount'));
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../../assets/images/coll... Remove this comment to see the full error message
const collapse_arrow_small_inline_svg_1 = __importDefault(
  require('../../../../assets/images/collapse-arrow-small.inline.svg')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../../assets/images/star... Remove this comment to see the full error message
const star_not_filled_inline_svg_1 = __importDefault(
  require('../../../../assets/images/star-not-filled.inline.svg')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../../assets/images/star... Remove this comment to see the full error message
const star_filled_inline_svg_1 = __importDefault(
  require('../../../../assets/images/star-filled.inline.svg')
);
function WalletTokenHeader(props) {
  const {
    anyAssetWasHovered,
    asset,
    assetSettingsDialogWasOpened,
    className,
    fullFingerprint = true,
    isExpanded,
    isFavorite,
    isLoading,
    hasWarning,
    onClick,
    onCopyAssetParam,
    onToggleFavorite,
  } = props;
  const { uniqueId } = asset;
  const starIcon = isFavorite
    ? star_filled_inline_svg_1.default
    : star_not_filled_inline_svg_1.default;
  const rootStyles = (0, classnames_1.default)(
    WalletTokenHeader_scss_1.default.root,
    isExpanded && WalletTokenHeader_scss_1.default.isExpanded,
    className
  );
  const favoriteIconStyles = (0, classnames_1.default)(
    WalletTokenHeader_scss_1.default.favoriteIcon,
    isFavorite && WalletTokenHeader_scss_1.default.isFavorite
  );
  return react_1.default.createElement(
    'div',
    { className: rootStyles, onClick: onClick },
    onToggleFavorite &&
      react_1.default.createElement(
        'button',
        {
          className: favoriteIconStyles,
          onClick: (event) => {
            event.persist();
            event.stopPropagation();
            onToggleFavorite({
              uniqueId,
              isFavorite,
            });
          },
        },
        react_1.default.createElement(react_svg_inline_1.default, {
          svg: starIcon,
        })
      ),
    react_1.default.createElement(Asset_1.default, {
      asset: asset,
      small: false,
      onCopyAssetParam: onCopyAssetParam,
      // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
      metadataNameChars: (0, lodash_1.get)('name', asset.metadata, 0),
      assetSettingsDialogWasOpened: assetSettingsDialogWasOpened,
      anyAssetWasHovered: anyAssetWasHovered,
      className: WalletTokenHeader_scss_1.default.asset,
      hidePopOver: true,
      fullFingerprint: fullFingerprint,
      hasWarning: hasWarning,
    }),
    react_1.default.createElement(AssetAmount_1.default, {
      amount: asset.quantity,
      metadata: asset.metadata,
      decimals: asset.decimals,
      isLoading: isLoading,
      className: WalletTokenHeader_scss_1.default.assetAmount,
      isShort: true,
    }),
    react_1.default.createElement(react_svg_inline_1.default, {
      svg: collapse_arrow_small_inline_svg_1.default,
      className: WalletTokenHeader_scss_1.default.arrow,
    })
  );
}
exports.default = (0, mobx_react_1.observer)(WalletTokenHeader);
//# sourceMappingURL=WalletTokenHeader.js.map
