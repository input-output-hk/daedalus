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
const react_intl_1 = require('react-intl');
const Button_1 = require('@react-polymorph/components/Button');
const classnames_1 = __importDefault(require('classnames'));
const mobx_react_1 = require('mobx-react');
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
const assets_1 = require('../../../../utils/assets');
const WalletTokensList_scss_1 = __importDefault(
  require('./WalletTokensList.scss')
);
const BorderedBox_1 = __importDefault(require('../../../widgets/BorderedBox'));
const LoadingSpinner_1 = __importDefault(
  require('../../../widgets/LoadingSpinner')
);
const WalletToken_1 = __importDefault(require('../wallet-token/WalletToken'));
const WalletNoTokens_1 = __importDefault(
  require('../wallet-no-tokens/WalletNoTokens')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../../assets/images/ascending... Remove this comment to see the full error message
const ascending_inline_svg_1 = __importDefault(
  require('../../../../assets/images/ascending.inline.svg')
);
const WalletTokensList_messages_1 = require('./WalletTokensList.messages');
const getSortIconClasses = (item, sortBy, sortDirection) => {
  const isSorted = item === sortBy;
  return (0, classnames_1.default)([
    WalletTokensList_scss_1.default.sortIcon,
    isSorted ? WalletTokensList_scss_1.default.sorted : null,
    isSorted
      ? WalletTokensList_scss_1.default[`${sortDirection}Sorting`]
      : WalletTokensList_scss_1.default.ascSorting,
  ]);
};
const WalletTokensList = (0, mobx_react_1.observer)((props) => {
  const [sortDirection, setSortDirection] = (0, react_1.useState)('asc');
  const [sortBy, setSortBy] = (0, react_1.useState)('token');
  const {
    assets,
    assetSettingsDialogWasOpened,
    insertingAssetUniqueId,
    intl,
    isLoadingAssets,
    onAssetSettings,
    onCopyAssetParam,
    onExternalLinkClick,
    onOpenAssetSend,
    onToggleFavorite,
    onViewAllButtonClick,
    removingAssetUniqueId,
    searchValue = '',
    title,
    tokenFavorites,
    wallet,
  } = props;
  const isRestoreActive = wallet.isRestoring;
  const sortedAssets = (0, react_1.useMemo)(() => {
    return [...assets].sort((0, assets_1.sortAssets)(sortBy, sortDirection));
  }, [assets, sortBy, sortDirection]);
  const filteredAssets =
    (0, assets_1.searchAssets)(searchValue, sortedAssets) || [];
  const hasSearch =
    !isLoadingAssets && !!searchValue && searchValue.trim().length >= 3;
  const noResults = hasSearch && !filteredAssets.length;
  const viewAllButtonStyles = (0, classnames_1.default)([
    'flat',
    WalletTokensList_scss_1.default.viewAllButton,
  ]);
  const hasSorting = filteredAssets.length && filteredAssets.length > 1;
  const columnsStyles = (0, classnames_1.default)([
    WalletTokensList_scss_1.default.columns,
    hasSorting ? WalletTokensList_scss_1.default.hasSorting : null,
  ]);
  const sortIconClassesToken = (0, react_1.useMemo)(
    () => getSortIconClasses('token', sortBy, sortDirection),
    [sortBy, sortDirection]
  );
  const sortIconClassesAmount = (0, react_1.useMemo)(
    () => getSortIconClasses('quantity', sortBy, sortDirection),
    [sortBy, sortDirection]
  );
  const toggleSortDirection = (0, react_1.useCallback)(() => {
    if (sortDirection === 'asc') {
      setSortDirection('desc');
    } else {
      setSortDirection('asc');
    }
  }, [sortDirection]);
  const onSortBy = (0, react_1.useCallback)(
    (newSortBy) => {
      if (!hasSorting) return;
      if (newSortBy === sortBy) {
        toggleSortDirection();
      } else {
        setSortDirection('asc');
        setSortBy(newSortBy);
      }
    },
    [sortDirection, hasSorting, sortBy]
  );
  const onSortByToken = (0, react_1.useCallback)(() => onSortBy('token'), [
    sortDirection,
    sortBy,
    hasSorting,
  ]);
  const onSortByAmount = (0, react_1.useCallback)(() => onSortBy('quantity'), [
    sortDirection,
    sortBy,
    hasSorting,
  ]);
  const hasTokens = assets.length || isLoadingAssets;
  if (!hasTokens)
    return react_1.default.createElement(WalletNoTokens_1.default, {
      numberOfAssets: assets.length,
      isLoadingAssets: isLoadingAssets,
      onExternalLinkClick: onExternalLinkClick,
    });
  let content;
  if (isLoadingAssets) {
    content = react_1.default.createElement(
      'div',
      null,
      react_1.default.createElement(LoadingSpinner_1.default, { big: true })
    );
  } else if (noResults) {
    content = react_1.default.createElement(
      'p',
      { className: WalletTokensList_scss_1.default.noResults },
      intl.formatMessage(WalletTokensList_messages_1.messages.noResults)
    );
  } else {
    content = filteredAssets.map((asset) => {
      return react_1.default.createElement(WalletToken_1.default, {
        key: asset.uniqueId,
        asset: asset,
        onOpenAssetSend: onOpenAssetSend,
        onCopyAssetParam: onCopyAssetParam,
        onAssetSettings: onAssetSettings,
        anyAssetWasHovered: true,
        isLoading: isRestoreActive,
        assetSettingsDialogWasOpened: assetSettingsDialogWasOpened,
        onToggleFavorite: onToggleFavorite,
        isFavorite: tokenFavorites[asset.uniqueId],
        isInsertingAsset: insertingAssetUniqueId === asset.uniqueId,
        isRemovingAsset: removingAssetUniqueId === asset.uniqueId,
      });
    });
  }
  return react_1.default.createElement(
    'div',
    { className: WalletTokensList_scss_1.default.component },
    react_1.default.createElement(
      'div',
      { className: WalletTokensList_scss_1.default.outerHeader },
      react_1.default.createElement(
        'div',
        { className: WalletTokensList_scss_1.default.title },
        title,
        hasSearch &&
          !noResults &&
          react_1.default.createElement(
            react_1.default.Fragment,
            null,
            intl.formatMessage(
              WalletTokensList_messages_1.messages.searchResults
            ),
            ' (',
            filteredAssets.length,
            ')'
          )
      )
    ),
    react_1.default.createElement(
      BorderedBox_1.default,
      null,
      react_1.default.createElement(
        'div',
        { className: columnsStyles },
        react_1.default.createElement(
          'div',
          {
            className: WalletTokensList_scss_1.default.column,
            onClick: onSortByToken,
          },
          intl.formatMessage(WalletTokensList_messages_1.messages.columnToken),
          react_1.default.createElement(react_svg_inline_1.default, {
            svg: ascending_inline_svg_1.default,
            className: sortIconClassesToken,
          })
        ),
        react_1.default.createElement(
          'div',
          {
            className: WalletTokensList_scss_1.default.column,
            onClick: onSortByAmount,
          },
          intl.formatMessage(WalletTokensList_messages_1.messages.columnAmount),
          react_1.default.createElement(react_svg_inline_1.default, {
            svg: ascending_inline_svg_1.default,
            className: sortIconClassesAmount,
          })
        )
      ),
      content,
      onViewAllButtonClick &&
        react_1.default.createElement(Button_1.Button, {
          className: viewAllButtonStyles,
          onClick: onViewAllButtonClick,
          label: intl.formatMessage(
            WalletTokensList_messages_1.messages.viewAllButtonLabel
          ),
        })
    )
  );
});
exports.default = (0, react_intl_1.injectIntl)(WalletTokensList);
//# sourceMappingURL=WalletTokensList.js.map
