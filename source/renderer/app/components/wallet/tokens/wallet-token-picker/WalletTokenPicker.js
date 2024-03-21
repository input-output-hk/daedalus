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
const react_intl_1 = require('react-intl');
const classnames_1 = __importDefault(require('classnames'));
const Select_1 = require('@react-polymorph/components/Select');
const Dialog_1 = __importDefault(require('../../../widgets/Dialog'));
const WalletToken_1 = __importDefault(require('../wallet-token/WalletToken'));
const WalletTokensSearch_1 = __importDefault(
  require('../wallet-tokens-search/WalletTokensSearch')
);
const WalletTokenPickerCheckbox_1 = __importDefault(
  require('./WalletTokenPickerCheckbox')
);
const DialogCloseButton_1 = __importDefault(
  require('../../../widgets/DialogCloseButton')
);
const WalletTokenPicker_scss_1 = __importDefault(
  require('./WalletTokenPicker.scss')
);
const WalletTokenPicker_messages_1 = require('./WalletTokenPicker.messages');
const helpers_1 = require('./helpers');
const hooks_1 = require('./hooks');
const const_1 = require('./const');
function WalletTokenPicker({
  intl,
  assets,
  walletName,
  tokenFavorites,
  previouslyCheckedIds = [],
  onAdd,
  onCancel,
}) {
  const { onScroll, scrollPosition } = (0, hooks_1.useScrollPosition)();
  const {
    searchValue,
    setSearchValue,
    currentAssets,
    filterOption,
    setFilterOption,
  } = (0, hooks_1.useFilters)({
    assets,
    tokenFavorites,
  });
  const {
    checkboxes,
    totalCheckedCount,
    checkedIds,
    previouslyCheckedIdsSet,
    isMaxTotalCount,
    isToggleAllDisabled,
    isClearAllMode,
    toggleAllFn,
    toggleCheckbox,
  } = (0, hooks_1.useCheckboxes)({
    assets,
    currentAssets,
    previouslyCheckedIds,
  });
  const scrollNotTop = scrollPosition !== const_1.ScrollPositionEnum.TOP;
  const toolbarStyles = (0, classnames_1.default)(
    WalletTokenPicker_scss_1.default.toolbar,
    scrollNotTop && WalletTokenPicker_scss_1.default.scrollNotTop
  );
  const toolbarContainerStyles = (0, classnames_1.default)(
    scrollNotTop && WalletTokenPicker_scss_1.default.toolbarContainer
  );
  const actions = (0, react_1.useMemo)(
    () => [
      {
        label: intl.formatMessage(
          WalletTokenPicker_messages_1.messages.cancelButtonLabel
        ),
        onClick: onCancel,
      },
      {
        label: intl.formatMessage(
          WalletTokenPicker_messages_1.messages.addButtonLabel
        ),
        primary: true,
        disabled: !checkedIds.length,
        onClick: () => onAdd(checkedIds),
      },
    ],
    [intl, checkedIds, onAdd]
  );
  return react_1.default.createElement(
    Dialog_1.default,
    {
      className: WalletTokenPicker_scss_1.default.dialog,
      title: intl.formatMessage(WalletTokenPicker_messages_1.messages.title),
      subtitle: walletName,
      closeOnOverlayClick: true,
      actions: actions,
      onClose: onCancel,
      closeButton: react_1.default.createElement(
        DialogCloseButton_1.default,
        null
      ),
    },
    react_1.default.createElement(
      'div',
      {
        className: WalletTokenPicker_scss_1.default.root,
        'data-testid': 'WalletTokenPicker',
      },
      react_1.default.createElement(
        'div',
        { className: WalletTokenPicker_scss_1.default.search },
        react_1.default.createElement(WalletTokensSearch_1.default, {
          searchValue: searchValue,
          onSearch: setSearchValue,
        })
      ),
      react_1.default.createElement(
        'div',
        { className: toolbarContainerStyles },
        react_1.default.createElement(
          'div',
          { className: toolbarStyles },
          react_1.default.createElement(Select_1.Select, {
            value: filterOption,
            onChange: setFilterOption,
            className: WalletTokenPicker_scss_1.default.filterSelect,
            options: (0, helpers_1.filterSelectOptions)(intl),
            selectionRenderer: (option) =>
              react_1.default.createElement(
                'span',
                null,
                option.label,
                react_1.default.createElement(
                  'span',
                  { className: WalletTokenPicker_scss_1.default.filterCounter },
                  (0, helpers_1.getTokenCounterText)({
                    assets,
                    currentAssets,
                  })
                )
              ),
            optionRenderer: (option) =>
              react_1.default.createElement(
                'span',
                { className: WalletTokenPicker_scss_1.default.filterOption },
                option.label
              ),
            optionHeight: 33,
          }),
          react_1.default.createElement(
            'span',
            { className: WalletTokenPicker_scss_1.default.count },
            intl.formatMessage(
              WalletTokenPicker_messages_1.messages.checkedCountLabel,
              {
                checkedCount: totalCheckedCount,
                maxTokens: Math.min(const_1.MAX_TOKENS, assets.length),
              }
            )
          ),
          react_1.default.createElement(
            'button',
            {
              className: WalletTokenPicker_scss_1.default.toggleAllButton,
              onClick: toggleAllFn,
              disabled: isToggleAllDisabled,
            },
            intl.formatMessage(
              WalletTokenPicker_messages_1.messages[
                (0, helpers_1.getToggleAllLabel)(isClearAllMode)
              ],
              {
                maxTokens: const_1.MAX_TOKENS,
              }
            )
          )
        )
      ),
      react_1.default.createElement(
        'div',
        {
          className: WalletTokenPicker_scss_1.default.list,
          onScroll: onScroll,
        },
        currentAssets?.length === 0 &&
          react_1.default.createElement(
            'span',
            { className: WalletTokenPicker_scss_1.default.noResults },
            intl.formatMessage(WalletTokenPicker_messages_1.messages.noResults)
          ),
        currentAssets?.map((asset) =>
          react_1.default.createElement(
            'div',
            {
              key: asset.uniqueId,
              className: WalletTokenPicker_scss_1.default.listItem,
            },
            react_1.default.createElement(WalletTokenPickerCheckbox_1.default, {
              className: WalletTokenPicker_scss_1.default.checkbox,
              isChecked: checkboxes[asset.uniqueId],
              isMaxCount: isMaxTotalCount,
              isPreviouslyChecked: previouslyCheckedIdsSet.has(asset.uniqueId),
              uniqueId: asset.uniqueId,
              toggleCheckbox: toggleCheckbox,
            }),
            react_1.default.createElement(WalletToken_1.default, {
              asset: asset,
              className: WalletTokenPicker_scss_1.default.token,
              headerClassName: WalletTokenPicker_scss_1.default.tokenHeader,
              footerClassName: WalletTokenPicker_scss_1.default.tokenFooter,
              fullFingerprint: false,
              isFavorite: tokenFavorites[asset.uniqueId],
            })
          )
        )
      )
    )
  );
}
exports.default = (0, react_intl_1.injectIntl)(
  (0, mobx_react_1.observer)(WalletTokenPicker)
);
//# sourceMappingURL=WalletTokenPicker.js.map
