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
const mobx_react_1 = require('mobx-react');
const react_intl_1 = require('react-intl');
const classnames_1 = __importDefault(require('classnames'));
const lodash_1 = require('lodash');
const StakingWithNavigation_1 = require('../layouts/StakingWithNavigation');
const StakePoolsRanking_1 = __importDefault(require('./StakePoolsRanking'));
const StakePoolsList_1 = require('./StakePoolsList');
const StakePoolsTable_1 = require('./StakePoolsTable');
const StakePoolsSearch_1 = require('./StakePoolsSearch');
const BackToTopButton_1 = __importDefault(
  require('../../widgets/BackToTopButton')
);
const LoadingSpinner_1 = __importDefault(
  require('../../widgets/LoadingSpinner')
);
const StakePools_scss_1 = __importDefault(require('./StakePools.scss'));
const helpers_1 = require('./helpers');
const formatters_1 = require('../../../utils/formatters');
const stakingConfig_1 = require('../../../config/stakingConfig');
const smash_settings_ic_inline_svg_1 = __importDefault(
  require('../../../assets/images/smash-settings-ic.inline.svg')
);
const spinner_tiny_inline_svg_1 = __importDefault(
  require('../../../assets/images/spinner-tiny.inline.svg')
);
const staking_1 = require('../../../utils/staking');
const analytics_1 = require('../../../analytics');
const messages = (0, react_intl_1.defineMessages)({
  delegatingListTitle: {
    id: 'staking.stakePools.delegatingListTitle',
    defaultMessage: '!!!Stake pools to which you are delegating',
    description: '"delegatingListTitle" for the Stake Pools page.',
  },
  listTitle: {
    id: 'staking.stakePools.listTitle',
    defaultMessage: '!!!Stake pools',
    description: '"listTitle" for the Stake Pools page.',
  },
  listTitleLoading: {
    id: 'staking.stakePools.listTitleLoading',
    defaultMessage: '!!!Loading stake pools',
    description: '"listTitleLoading" for the Stake Pools page.',
  },
  listTitleSearch: {
    id: 'staking.stakePools.listTitleSearch',
    defaultMessage: '!!!Stake pools. Search results:',
    description: '"listTitleSearch" for the Stake Pools page.',
  },
  listTitleStakePools: {
    id: 'staking.stakePools.listTitleStakePools',
    defaultMessage: '!!!({pools})',
    description: '"listTitleStakePools" for the Stake Pools page.',
  },
  loadingStakePoolsMessage: {
    id: 'staking.stakePools.loadingStakePoolsMessage',
    defaultMessage: '!!!Loading stake pools',
    description:
      'Loading stake pool message for the Delegation center body section.',
  },
  moderatedBy: {
    id: 'staking.stakePools.moderatedBy',
    defaultMessage: '!!!Moderated by',
    description: 'moderatedBy message for the Delegation center body section.',
  },
  unmoderated: {
    id: 'staking.stakePools.unmoderated',
    defaultMessage: '!!!Unmoderated',
    description: 'unmoderated message for the Delegation center body section.',
  },
});
const SELECTED_INDEX_TABLE = 'selectedIndexTable';
const initialState = {
  search: '',
  selectedList: null,
  isGridView: true,
  isGridRewardsView: false,
  isListView: false,
  isTableHeaderHovered: false,
};
let StakePools = class StakePools extends react_1.Component {
  loadingSpinner;
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  state = { ...initialState };
  sendSearchAnalyticsEvent = (0, lodash_1.debounce)(
    () =>
      this.props.analyticsTracker.sendEvent(
        analytics_1.EventCategories.STAKE_POOLS,
        'Used stake pools search'
      ),
    5000
  );
  handleSearch = (search) => {
    this.setState({
      search,
    });
    this.sendSearchAnalyticsEvent();
  };
  handleClearSearch = () =>
    this.setState({
      search: '',
    });
  handleGridView = () => {
    this.setState({
      isGridView: true,
      isGridRewardsView: false,
      isListView: false,
    });
    this.props.analyticsTracker.sendEvent(
      analytics_1.EventCategories.STAKE_POOLS,
      'Changed view to grid view'
    );
  };
  handleGridRewardsView = () => {
    this.setState({
      isGridView: false,
      isGridRewardsView: true,
      isListView: false,
    });
    this.props.analyticsTracker.sendEvent(
      analytics_1.EventCategories.STAKE_POOLS,
      'Changed view to grid rewards view'
    );
  };
  handleListView = () => {
    this.setState({
      isGridView: false,
      isGridRewardsView: false,
      isListView: true,
    });
    this.props.analyticsTracker.sendEvent(
      analytics_1.EventCategories.STAKE_POOLS,
      'Changed view to list view'
    );
  };
  handleSetListActive = (selectedList) =>
    this.setState({
      selectedList,
    });
  handleTableHeaderMouseEnter = () =>
    this.setState({
      isTableHeaderHovered: true,
    });
  handleTableHeaderMouseLeave = () =>
    this.setState({
      isTableHeaderHovered: false,
    });
  onDelegate = (poolId) => {
    const { onDelegate } = this.props;
    onDelegate(poolId);
  };
  render() {
    const { intl } = this.context;
    const {
      wallets,
      currentLocale,
      stakePoolsList,
      updateDelegatingStake,
      rankStakePools,
      selectedDelegationWalletId,
      stake,
      onOpenExternalLink,
      onListViewVisited,
      currentTheme,
      isFetching,
      isListViewTooltipVisible,
      isLoading,
      isRanking,
      stakePoolsDelegatingList,
      getStakePoolById,
      smashServerUrl,
      onSmashSettingsClick,
      maxDelegationFunds,
    } = this.props;
    const {
      search,
      selectedList,
      isListView,
      isGridView,
      isGridRewardsView,
      isTableHeaderHovered,
    } = this.state;
    const filteredStakePoolsList = (0, helpers_1.getFilteredStakePoolsList)(
      stakePoolsList,
      search
    );
    const numberOfRankedStakePools = stakePoolsList.filter(
      (stakePool) =>
        stakingConfig_1.IS_RANKING_DATA_AVAILABLE &&
        stakePool.nonMyopicMemberRewards
    ).length;
    const listTitleMessage = isFetching
      ? messages.listTitleLoading
      : messages.listTitle;
    const listTitleSearchMessage =
      !!search.trim().length && intl.formatMessage(messages.listTitleSearch);
    const loadingSpinner = react_1.default.createElement(
      LoadingSpinner_1.default,
      {
        big: true,
        ref: (component) => {
          this.loadingSpinner = component;
        },
      }
    );
    const componentClasses = (0, classnames_1.default)([
      StakePools_scss_1.default.component,
      isLoading ? StakePools_scss_1.default.isLoading : null,
    ]);
    const smashServer = smashServerUrl
      ? (0, staking_1.getSmashServerNameFromUrl)(smashServerUrl)
      : null;
    const tinyLoadingSpinner =
      isFetching &&
      react_1.default.createElement(react_svg_inline_1.default, {
        svg: spinner_tiny_inline_svg_1.default,
        className: StakePools_scss_1.default.tinySpinner,
      });
    const smashSettings = react_1.default.createElement(
      'button',
      {
        onClick: onSmashSettingsClick,
        className: StakePools_scss_1.default.smashSettings,
      },
      react_1.default.createElement(
        'span',
        null,
        smashServer && smashServer !== stakingConfig_1.SMASH_SERVER_TYPES.DIRECT
          ? intl.formatMessage(messages.moderatedBy, {
              smashServer,
            })
          : intl.formatMessage(messages.unmoderated)
      ),
      react_1.default.createElement(react_svg_inline_1.default, {
        svg: smash_settings_ic_inline_svg_1.default,
        className: StakePools_scss_1.default.smashSettingsIcon,
      })
    );
    return react_1.default.createElement(
      'div',
      { className: componentClasses },
      isLoading
        ? react_1.default.createElement(
            'div',
            { className: StakePools_scss_1.default.loadingBlockWrapper },
            react_1.default.createElement(
              'p',
              null,
              intl.formatMessage(messages.loadingStakePoolsMessage)
            ),
            loadingSpinner
          )
        : react_1.default.createElement(
            react_1.Fragment,
            null,
            react_1.default.createElement(BackToTopButton_1.default, {
              scrollableElementClassName: 'StakingWithNavigation_page',
              scrollTopToActivate: isListView ? 400 : 340,
              buttonTopPosition: isListView ? 184 : 144,
              isForceHidden: isTableHeaderHovered,
            }),
            react_1.default.createElement(StakePoolsRanking_1.default, {
              wallets: wallets,
              onOpenExternalLink: onOpenExternalLink,
              updateDelegatingStake: updateDelegatingStake,
              rankStakePools: rankStakePools,
              selectedDelegationWalletId: selectedDelegationWalletId,
              stake: stake,
              isLoading: isLoading,
              isRanking: isRanking,
              numberOfStakePools: stakePoolsList.length,
              getStakePoolById: getStakePoolById,
              maxDelegationFunds: maxDelegationFunds,
              maxDelegationFundsLog: Math.log(maxDelegationFunds),
            }),
            react_1.default.createElement(StakePoolsSearch_1.StakePoolsSearch, {
              search: search,
              onSearch: this.handleSearch,
              onClearSearch: this.handleClearSearch,
              onGridView: this.handleGridView,
              onGridRewardsView: this.handleGridRewardsView,
              onListView: this.handleListView,
              onListViewVisited: onListViewVisited,
              isListView: isListView,
              isListViewTooltipVisible: isListViewTooltipVisible,
              isGridView: isGridView,
              isGridRewardsView: isGridRewardsView,
              // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
              smashServer: smashServer,
            }),
            stakePoolsDelegatingList.length > 0 &&
              react_1.default.createElement(
                react_1.Fragment,
                null,
                react_1.default.createElement(
                  'h2',
                  { className: StakePools_scss_1.default.listTitle },
                  react_1.default.createElement(
                    'span',
                    { className: StakePools_scss_1.default.leftContent },
                    react_1.default.createElement(
                      'span',
                      null,
                      intl.formatMessage(messages.delegatingListTitle)
                    )
                  )
                ),
                isListView
                  ? react_1.default.createElement(
                      StakePoolsTable_1.StakePoolsTable,
                      {
                        listName: SELECTED_INDEX_TABLE,
                        currentLocale: currentLocale,
                        stakePoolsList: stakePoolsDelegatingList,
                        onOpenExternalLink: onOpenExternalLink,
                        currentTheme: currentTheme,
                        isListActive: selectedList === SELECTED_INDEX_TABLE,
                        setListActive: this.handleSetListActive,
                        containerClassName: 'StakingWithNavigation_page',
                        onSelect: this.onDelegate,
                        numberOfRankedStakePools: numberOfRankedStakePools,
                        showWithSelectButton: true,
                        onTableHeaderMouseEnter: this
                          .handleTableHeaderMouseEnter,
                        onTableHeaderMouseLeave: this
                          .handleTableHeaderMouseLeave,
                      }
                    )
                  : react_1.default.createElement(
                      StakingWithNavigation_1.StakingPageScrollContext.Consumer,
                      null,
                      (stakePoolsScrollContext) =>
                        react_1.default.createElement(
                          StakePoolsList_1.StakePoolsList,
                          {
                            stakePoolsList: stakePoolsDelegatingList,
                            onOpenExternalLink: onOpenExternalLink,
                            currentTheme: currentTheme,
                            containerClassName: 'StakingWithNavigation_page',
                            onSelect: this.onDelegate,
                            numberOfRankedStakePools: numberOfRankedStakePools,
                            isGridRewardsView: isGridRewardsView,
                            showWithSelectButton: true,
                            scrollElementRef:
                              stakePoolsScrollContext.scrollElementRef,
                          }
                        )
                    )
              ),
            isListView &&
              react_1.default.createElement(
                react_1.Fragment,
                null,
                react_1.default.createElement(
                  'h2',
                  null,
                  react_1.default.createElement(
                    'span',
                    { className: StakePools_scss_1.default.leftContent },
                    react_1.default.createElement(
                      'span',
                      null,
                      intl.formatMessage(listTitleMessage),
                      listTitleSearchMessage,
                      intl.formatMessage(messages.listTitleStakePools, {
                        pools: (0, formatters_1.formattedNumber)(
                          filteredStakePoolsList.length
                        ),
                      })
                    ),
                    tinyLoadingSpinner
                  ),
                  smashSettings
                ),
                react_1.default.createElement(
                  StakePoolsTable_1.StakePoolsTable,
                  {
                    listName: SELECTED_INDEX_TABLE,
                    currentLocale: currentLocale,
                    stakePoolsList: filteredStakePoolsList,
                    onOpenExternalLink: onOpenExternalLink,
                    currentTheme: currentTheme,
                    isListActive: selectedList === SELECTED_INDEX_TABLE,
                    setListActive: this.handleSetListActive,
                    containerClassName: 'StakingWithNavigation_page',
                    onSelect: this.onDelegate,
                    numberOfRankedStakePools: numberOfRankedStakePools,
                    showWithSelectButton: true,
                    onTableHeaderMouseEnter: this.handleTableHeaderMouseEnter,
                    onTableHeaderMouseLeave: this.handleTableHeaderMouseLeave,
                  }
                )
              ),
            (isGridView || isGridRewardsView) &&
              react_1.default.createElement(
                react_1.Fragment,
                null,
                react_1.default.createElement(
                  'h2',
                  null,
                  react_1.default.createElement(
                    'span',
                    { className: StakePools_scss_1.default.leftContent },
                    react_1.default.createElement(
                      'span',
                      null,
                      intl.formatMessage(listTitleMessage),
                      listTitleSearchMessage,
                      intl.formatMessage(messages.listTitleStakePools, {
                        pools: (0, formatters_1.formattedNumber)(
                          filteredStakePoolsList.length
                        ),
                      })
                    ),
                    tinyLoadingSpinner
                  ),
                  smashSettings
                ),
                react_1.default.createElement(
                  StakingWithNavigation_1.StakingPageScrollContext.Consumer,
                  null,
                  (stakePoolsScrollContext) =>
                    react_1.default.createElement(
                      StakePoolsList_1.StakePoolsList,
                      {
                        showWithSelectButton: true,
                        stakePoolsList: filteredStakePoolsList,
                        onOpenExternalLink: onOpenExternalLink,
                        currentTheme: currentTheme,
                        containerClassName: 'StakingWithNavigation_page',
                        onSelect: this.onDelegate,
                        numberOfRankedStakePools: numberOfRankedStakePools,
                        isGridRewardsView: isGridRewardsView,
                        scrollElementRef:
                          stakePoolsScrollContext.scrollElementRef,
                      }
                    )
                )
              )
          )
    );
  }
};
StakePools = __decorate([mobx_react_1.observer], StakePools);
exports.default = StakePools;
//# sourceMappingURL=StakePools.js.map
