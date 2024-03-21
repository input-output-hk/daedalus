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
exports.StakePoolsSearch = void 0;
const react_1 = __importStar(require('react'));
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
const react_intl_1 = require('react-intl');
const Input_1 = require('@react-polymorph/components/Input');
const InputSkin_1 = require('@react-polymorph/skins/simple/InputSkin');
const PopOver_1 = require('@react-polymorph/components/PopOver');
const classnames_1 = __importDefault(require('classnames'));
const StakePoolsSearch_scss_1 = __importDefault(
  require('./StakePoolsSearch.scss')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/search.... Remove this comment to see the full error message
const search_inline_svg_1 = __importDefault(
  require('../../../assets/images/search.inline.svg')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/close-c... Remove this comment to see the full error message
const close_cross_inline_svg_1 = __importDefault(
  require('../../../assets/images/close-cross.inline.svg')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/grid-ic... Remove this comment to see the full error message
const grid_ic_inline_svg_1 = __importDefault(
  require('../../../assets/images/grid-ic.inline.svg')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/grid-re... Remove this comment to see the full error message
const grid_rewards_inline_svg_1 = __importDefault(
  require('../../../assets/images/grid-rewards.inline.svg')
);
const stakingConfig_1 = require('../../../config/stakingConfig');
const StakePoolsSearch_messages_1 = require('./StakePoolsSearch.messages');
const StakePoolsSearchListViewButton_1 = require('./StakePoolsSearchListViewButton');
const StakingWithNavigation_1 = require('../layouts/StakingWithNavigation');
function StakePoolsSearchComponent({
  label,
  onClearSearch,
  onSearch,
  onGridView,
  onGridRewardsView,
  onListView,
  onListViewVisited,
  placeholder,
  search,
  isListView,
  isListViewTooltipVisible,
  isGridView,
  isGridRewardsView,
  intl,
}) {
  const searchInput = (0, react_1.useRef)(null);
  const [isSearchInputFocused, setSearchInputFocused] = (0, react_1.useState)(
    false
  );
  const autoSelectOnFocus = () => {
    searchInput?.current
      ? searchInput.current.inputElement?.current?.select()
      : false;
    setSearchInputFocused(true);
  };
  const handleClearSearch = () => {
    onClearSearch();
    searchInput?.current?.inputElement.current.focus();
    setSearchInputFocused(true);
  };
  const hasSearchClearButton = () => {
    return search.length > 0;
  };
  const gridButtonClasses = (0, classnames_1.default)([
    StakePoolsSearch_scss_1.default.gridView,
    isGridView ? StakePoolsSearch_scss_1.default.selected : null,
  ]);
  const gridRewardsButtonClasses = (0, classnames_1.default)([
    StakePoolsSearch_scss_1.default.gridRewardsView,
    isGridRewardsView ? StakePoolsSearch_scss_1.default.selected : null,
  ]);
  const isBigSearchComponent = isListView || isGridView || isGridRewardsView;
  const searchInputClasses = (0, classnames_1.default)([
    StakePoolsSearch_scss_1.default.searchInput,
    isBigSearchComponent
      ? StakePoolsSearch_scss_1.default.inputExtrasSearch
      : null,
    stakingConfig_1.IS_GRID_REWARDS_VIEW_AVAILABLE
      ? StakePoolsSearch_scss_1.default.withGridRewardsView
      : null,
  ]);
  const clearSearchClasses = (0, classnames_1.default)([
    StakePoolsSearch_scss_1.default.inputExtras,
    isBigSearchComponent
      ? StakePoolsSearch_scss_1.default.inputExtrasSearch
      : null,
    stakingConfig_1.IS_GRID_REWARDS_VIEW_AVAILABLE
      ? StakePoolsSearch_scss_1.default.withGridRewardsView
      : null,
  ]);
  return react_1.default.createElement(
    StakingWithNavigation_1.StakingPageScrollContext.Consumer,
    null,
    ({ scrollElementRef }) =>
      react_1.default.createElement(
        'div',
        { className: StakePoolsSearch_scss_1.default.component },
        react_1.default.createElement(
          'div',
          { className: StakePoolsSearch_scss_1.default.container },
          react_1.default.createElement(react_svg_inline_1.default, {
            svg: search_inline_svg_1.default,
            className: (0, classnames_1.default)(
              StakePoolsSearch_scss_1.default.searchIcon,
              isSearchInputFocused &&
                StakePoolsSearch_scss_1.default.searchIconFocus
            ),
          }),
          react_1.default.createElement(Input_1.Input, {
            autoFocus: true,
            label: label || null,
            className: searchInputClasses,
            onChange: onSearch,
            ref: (input) => {
              searchInput.current = input;
            },
            onFocus: autoSelectOnFocus,
            onBlur: () => setSearchInputFocused(false),
            placeholder:
              placeholder ||
              intl.formatMessage(
                StakePoolsSearch_messages_1.messages.searchInputPlaceholder
              ),
            skin: InputSkin_1.InputSkin,
            value: search,
            maxLength: 150,
          }),
          hasSearchClearButton &&
            react_1.default.createElement(
              'div',
              { className: clearSearchClasses },
              react_1.default.createElement(
                PopOver_1.PopOver,
                {
                  content: intl.formatMessage(
                    StakePoolsSearch_messages_1.messages.clearTooltip
                  ),
                },
                react_1.default.createElement(
                  'button',
                  {
                    onClick: handleClearSearch,
                    className:
                      StakePoolsSearch_scss_1.default.clearSearchButton,
                  },
                  react_1.default.createElement(react_svg_inline_1.default, {
                    svg: close_cross_inline_svg_1.default,
                    className: StakePoolsSearch_scss_1.default.clearSearchIcon,
                  })
                )
              )
            )
        ),
        isBigSearchComponent &&
          react_1.default.createElement(
            'div',
            { className: StakePoolsSearch_scss_1.default.viewButtons },
            react_1.default.createElement(
              PopOver_1.PopOver,
              {
                content: intl.formatMessage(
                  StakePoolsSearch_messages_1.messages.gridIconTooltip
                ),
                appendTo: () => scrollElementRef.current,
                popperOptions: {
                  placement: 'top',
                  modifiers: [
                    {
                      name: 'flip',
                      options: {
                        fallbackPlacements: ['bottom'],
                      },
                    },
                  ],
                },
              },
              react_1.default.createElement(
                'button',
                { className: gridButtonClasses, onClick: onGridView },
                react_1.default.createElement(react_svg_inline_1.default, {
                  svg: grid_ic_inline_svg_1.default,
                })
              )
            ),
            stakingConfig_1.IS_GRID_REWARDS_VIEW_AVAILABLE &&
              react_1.default.createElement(
                PopOver_1.PopOver,
                {
                  content: intl.formatMessage(
                    StakePoolsSearch_messages_1.messages.gridRewardsIconTooltip
                  ),
                  appendTo: () => scrollElementRef.current,
                  popperOptions: {
                    placement: 'top',
                    modifiers: [
                      {
                        name: 'flip',
                        options: {
                          fallbackPlacements: ['bottom', 'left'],
                        },
                      },
                    ],
                  },
                },
                react_1.default.createElement(
                  'button',
                  {
                    className: gridRewardsButtonClasses,
                    onClick: onGridRewardsView,
                  },
                  react_1.default.createElement(react_svg_inline_1.default, {
                    svg: grid_rewards_inline_svg_1.default,
                  })
                )
              ),
            react_1.default.createElement(
              StakePoolsSearchListViewButton_1.StakePoolsSearchListViewButton,
              {
                isListViewTooltipVisible: isListViewTooltipVisible,
                isListView: isListView,
                onClick: onListView,
                onListViewVisited: onListViewVisited,
                tooltipTarget: scrollElementRef.current,
              }
            )
          )
      )
  );
}
exports.StakePoolsSearch = (0, react_intl_1.injectIntl)(
  StakePoolsSearchComponent
);
//# sourceMappingURL=StakePoolsSearch.js.map
