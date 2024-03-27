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
exports.StakingRewards = void 0;
const react_1 = __importStar(require('react'));
const mobx_react_1 = require('mobx-react');
const react_intl_1 = require('react-intl');
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
const lodash_1 = require('lodash');
const classnames_1 = __importDefault(require('classnames'));
const PopOver_1 = require('@react-polymorph/components/PopOver');
const Button_1 = require('@react-polymorph/components/Button');
const ButtonSkin_1 = require('@react-polymorph/skins/simple/ButtonSkin');
const react_copy_to_clipboard_1 = __importDefault(
  require('react-copy-to-clipboard')
);
const numbersConfig_1 = require('../../../config/numbersConfig');
const sortComparators_1 = require('../../../utils/sortComparators');
const BorderedBox_1 = __importDefault(require('../../widgets/BorderedBox'));
const LoadingSpinner_1 = __importDefault(
  require('../../widgets/LoadingSpinner')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/ascendi... Remove this comment to see the full error message
const ascending_inline_svg_1 = __importDefault(
  require('../../../assets/images/ascending.inline.svg')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/downloa... Remove this comment to see the full error message
const download_ic_inline_svg_1 = __importDefault(
  require('../../../assets/images/download-ic.inline.svg')
);
const StakingRewards_scss_1 = __importDefault(require('./StakingRewards.scss'));
const global_messages_1 = __importDefault(
  require('../../../i18n/global-messages')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/clipboa... Remove this comment to see the full error message
const clipboard_ic_inline_svg_1 = __importDefault(
  require('../../../assets/images/clipboard-ic.inline.svg')
);
const ButtonLink_1 = __importDefault(require('../../widgets/ButtonLink'));
const RewardAmount_1 = require('./RewardAmount');
const messages = (0, react_intl_1.defineMessages)({
  title: {
    id: 'staking.rewards.title',
    defaultMessage: '!!!Earned delegation rewards',
    description:
      'Title "Earned delegation rewards" label on the staking rewards page.',
  },
  csvFilenamePrefix: {
    id: 'staking.rewards.csvFilenamePrefix',
    defaultMessage: '!!!Rewards',
    description:
      'Filename prefix for the "Export CSV" on the staking rewards page.',
  },
  exportButtonLabel: {
    id: 'staking.rewards.exportButtonLabel',
    defaultMessage: '!!!Export CSV',
    description:
      'Label for the "Export CSV" button on the staking rewards page.',
  },
  noRewards: {
    id: 'staking.rewards.no.rewards',
    defaultMessage: '!!!No rewards',
    description: '"No rewards" rewards label on staking rewards page.',
  },
  tableHeaderWallet: {
    id: 'staking.rewards.tableHeader.wallet',
    defaultMessage: '!!!Wallet',
    description: 'Table header "Wallet" label on staking rewards page',
  },
  tableHeaderRewardTotal: {
    id: 'staking.rewards.tableHeader.total',
    defaultMessage: '!!!Total rewards earned (ADA)',
    description: 'Table header "Total Reward" label on staking rewards page',
  },
  tableHeaderRewardUnspent: {
    id: 'staking.rewards.tableHeader.unspent',
    defaultMessage: '!!!Unspent (ADA)',
    description: 'Table header "Unspent" label on staking rewards page',
  },
  tableHeaderRewardsAddress: {
    id: 'staking.rewards.tableHeader.rewardsAddress',
    defaultMessage: '!!!Rewards address',
    description: 'Table header "Rewards address" label on staking rewards page',
  },
  tableHeaderDate: {
    id: 'staking.rewards.tableHeader.date',
    defaultMessage: '!!!Date',
    description: 'Table header "Date" label in exported csv file',
  },
  note: {
    id: 'staking.rewards.note',
    defaultMessage:
      '!!!<p>Rewards earned by delegating your stake are automatically collected into your reward account.</p><p>Rewards earned on the Incentivized Testnet are not added to your Rewards wallet balance. They will be paid to you in real ada on the Cardano mainnet after the end of the Incentivized Testnet.</p><p>If you are using funds from this wallet to operate a stake pool, the rewards displayed here may include your pledged stake, which will not be counted when reward balances are paid out on the Cardano mainnet.</p>',
    description: 'Rewards description text on staking rewards page',
  },
  syncingTooltipLabel: {
    id: 'staking.delegationCenter.syncingTooltipLabel',
    defaultMessage: '!!!Syncing {syncingProgress}%',
    description: 'unknown stake pool label on staking rewards page.',
  },
  actionViewInExplorer: {
    id: 'staking.rewards.actionViewInExplorer',
    defaultMessage: '!!!View in explorer',
    description: 'View in explorer button label on staking rewards page.',
  },
});
const REWARD_FIELDS = {
  WALLET_NAME: 'wallet',
  IS_RESTORING: 'isRestoring',
  SYNCING_PROGRESS: 'syncingProgress',
  REWARD_TOTAL: 'total',
  REWARD_UNSPENT: 'unspent',
  REWARDS_ADDRESS: 'rewardsAddress',
};
const REWARD_ORDERS = {
  ASCENDING: 'asc',
  DESCENDING: 'desc',
};
const IS_EXPLORER_LINK_BUTTON_ENABLED = false;
let StakingRewards = class StakingRewards extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  static defaultProps = {
    isLoading: false,
    isExporting: false,
  };
  constructor(props) {
    super(props);
    this.state = {
      rewardsOrder: REWARD_ORDERS.DESCENDING,
      rewardsSortBy: REWARD_FIELDS.WALLET_NAME,
      contentScrollTop: 0,
    };
  }
  handleExportCsv = (availableTableHeaders, sortedRewards) => {
    const { onExportCsv } = this.props;
    const { intl } = this.context;
    const exportedHeader = availableTableHeaders.map((header) => header.title);
    const exportedBody = sortedRewards.map((reward) => {
      const rewardWallet = (0, lodash_1.get)(reward, REWARD_FIELDS.WALLET_NAME);
      const isRestoring = (0, lodash_1.get)(reward, REWARD_FIELDS.IS_RESTORING);
      const rewardTotal = (0, lodash_1.get)(
        reward,
        REWARD_FIELDS.REWARD_TOTAL
      )?.toFixed(numbersConfig_1.DECIMAL_PLACES_IN_ADA);
      const rewardUnspent = (0, lodash_1.get)(
        reward,
        REWARD_FIELDS.REWARD_UNSPENT
      )?.toFixed(numbersConfig_1.DECIMAL_PLACES_IN_ADA);
      const rewardsAddress = (0, lodash_1.get)(
        reward,
        REWARD_FIELDS.REWARDS_ADDRESS
      );
      return [
        rewardWallet,
        rewardsAddress,
        isRestoring ? '-' : rewardTotal,
        isRestoring ? '-' : rewardUnspent,
      ];
    });
    const exportedContent = [exportedHeader, ...exportedBody];
    onExportCsv({
      fileContent: exportedContent,
      filenamePrefix: intl.formatMessage(messages.csvFilenamePrefix),
    });
  };
  getSortedRewards = () => {
    const { rewards } = this.props;
    const { rewardsOrder, rewardsSortBy } = this.state;
    return rewards.slice().sort((rewardA, rewardB) => {
      const totalCompareResult = (0, sortComparators_1.bigNumberComparator)(
        rewardA.total,
        rewardB.total,
        rewardsOrder === REWARD_ORDERS.ASCENDING
      );
      const unspentCompareResult = (0, sortComparators_1.bigNumberComparator)(
        rewardA.unspent,
        rewardB.unspent,
        rewardsOrder === REWARD_ORDERS.ASCENDING
      );
      const walletNameCompareResult = (0, sortComparators_1.stringComparator)(
        rewardA.wallet,
        rewardB.wallet,
        rewardsOrder === REWARD_ORDERS.ASCENDING
      );
      const walletAddressCompareResult = (0,
      sortComparators_1.stringComparator)(
        rewardA.rewardsAddress,
        rewardB.rewardsAddress,
        rewardsOrder === REWARD_ORDERS.ASCENDING
      );
      if (rewardsSortBy === REWARD_FIELDS.REWARD_TOTAL) {
        if (totalCompareResult !== 0) return totalCompareResult;
        if (walletNameCompareResult !== 0) return walletNameCompareResult;
        return walletAddressCompareResult;
      }
      if (rewardsSortBy === REWARD_FIELDS.REWARD_UNSPENT) {
        if (unspentCompareResult !== 0) return unspentCompareResult;
        if (walletNameCompareResult !== 0) return walletNameCompareResult;
        return walletAddressCompareResult;
      }
      if (rewardsSortBy === REWARD_FIELDS.WALLET_NAME) {
        if (walletNameCompareResult !== 0) return walletNameCompareResult;
        if (totalCompareResult !== 0) return totalCompareResult;
        return walletAddressCompareResult;
      }
      if (rewardsSortBy === REWARD_FIELDS.REWARDS_ADDRESS) {
        if (walletAddressCompareResult !== 0) return walletAddressCompareResult;
        if (walletNameCompareResult !== 0) return walletNameCompareResult;
        return totalCompareResult;
      }
      return 0;
    });
  };
  handleContentScroll = (evt) => {
    this.setState({
      contentScrollTop: evt.currentTarget.scrollTop,
    });
  };
  render() {
    const {
      rewards,
      isLoading,
      isExporting,
      onCopyAddress,
      onOpenExternalLink,
    } = this.props;
    const { rewardsOrder, rewardsSortBy, contentScrollTop } = this.state;
    const { intl } = this.context;
    const noRewards = !isLoading && ((rewards && !rewards.length) || !rewards);
    const showRewards = rewards && rewards.length > 0 && !isLoading;
    const sortedRewards = showRewards ? this.getSortedRewards() : [];
    const availableTableHeaders = [
      {
        name: REWARD_FIELDS.WALLET_NAME,
        title: intl.formatMessage(messages.tableHeaderWallet),
      },
      {
        name: REWARD_FIELDS.REWARDS_ADDRESS,
        title: intl.formatMessage(messages.tableHeaderRewardsAddress),
      },
      {
        name: REWARD_FIELDS.REWARD_TOTAL,
        title: `${intl.formatMessage(
          messages.tableHeaderRewardTotal
        )} (${intl.formatMessage(global_messages_1.default.adaUnit)})`,
      },
      {
        name: REWARD_FIELDS.REWARD_UNSPENT,
        title: `${intl.formatMessage(
          messages.tableHeaderRewardUnspent
        )} (${intl.formatMessage(global_messages_1.default.adaUnit)})`,
      },
    ];
    const exportCsvButtonLabel = isExporting
      ? react_1.default.createElement(
          'div',
          { className: StakingRewards_scss_1.default.exportingSpinnerWrapper },
          react_1.default.createElement(LoadingSpinner_1.default, null)
        )
      : react_1.default.createElement(
          react_1.default.Fragment,
          null,
          react_1.default.createElement(
            'div',
            { className: StakingRewards_scss_1.default.actionLabel },
            intl.formatMessage(messages.exportButtonLabel)
          ),
          react_1.default.createElement(react_svg_inline_1.default, {
            svg: download_ic_inline_svg_1.default,
            className: StakingRewards_scss_1.default.downloadIcon,
          })
        );
    const exportCsvButtonClasses = (0, classnames_1.default)([
      'flat',
      StakingRewards_scss_1.default.actionButton,
    ]);
    const explorerButtonClasses = (0, classnames_1.default)([
      'flat',
      StakingRewards_scss_1.default.actionExplorerLink,
    ]);
    const headerWrapperClasses = (0, classnames_1.default)([
      StakingRewards_scss_1.default.headerWrapper,
      contentScrollTop > 10
        ? StakingRewards_scss_1.default.headerWrapperWithShadow
        : null,
    ]);
    return react_1.default.createElement(
      'div',
      { className: StakingRewards_scss_1.default.component },
      react_1.default.createElement(
        'div',
        { className: headerWrapperClasses },
        react_1.default.createElement(
          'div',
          { className: StakingRewards_scss_1.default.title },
          intl.formatMessage(messages.title)
        ),
        !noRewards &&
          react_1.default.createElement(Button_1.Button, {
            className: exportCsvButtonClasses,
            label: exportCsvButtonLabel,
            onClick: () =>
              this.handleExportCsv(availableTableHeaders, sortedRewards),
            skin: ButtonSkin_1.ButtonSkin,
          })
      ),
      react_1.default.createElement(
        'div',
        {
          className: StakingRewards_scss_1.default.contentWrapper,
          onScroll: this.handleContentScroll,
        },
        react_1.default.createElement(
          BorderedBox_1.default,
          null,
          noRewards &&
            react_1.default.createElement(
              'div',
              { className: StakingRewards_scss_1.default.noRewardsLabel },
              intl.formatMessage(messages.noRewards)
            ),
          sortedRewards.length > 0 &&
            react_1.default.createElement(
              'table',
              null,
              react_1.default.createElement(
                'thead',
                null,
                react_1.default.createElement(
                  'tr',
                  null,
                  (0, lodash_1.map)(availableTableHeaders, (tableHeader) => {
                    const isSorted = tableHeader.name === rewardsSortBy;
                    const sortIconClasses = (0, classnames_1.default)([
                      StakingRewards_scss_1.default.sortIcon,
                      isSorted ? StakingRewards_scss_1.default.sorted : null,
                      isSorted && rewardsOrder === 'asc'
                        ? StakingRewards_scss_1.default.ascending
                        : null,
                    ]);
                    return react_1.default.createElement(
                      'th',
                      {
                        className:
                          StakingRewards_scss_1.default[tableHeader.name],
                        key: tableHeader.name,
                        onClick: () => this.handleRewardsSort(tableHeader.name),
                      },
                      tableHeader.title,
                      react_1.default.createElement(
                        react_svg_inline_1.default,
                        {
                          svg: ascending_inline_svg_1.default,
                          className: sortIconClasses,
                        }
                      )
                    );
                  })
                )
              ),
              react_1.default.createElement(
                'tbody',
                null,
                (0, lodash_1.map)(sortedRewards, (reward, key) => {
                  const rewardWallet = (0, lodash_1.get)(
                    reward,
                    REWARD_FIELDS.WALLET_NAME
                  );
                  const isRestoring = (0, lodash_1.get)(
                    reward,
                    REWARD_FIELDS.IS_RESTORING
                  );
                  const syncingProgress = (0, lodash_1.get)(
                    reward,
                    REWARD_FIELDS.SYNCING_PROGRESS
                  );
                  const rewardTotal = (0, lodash_1.get)(
                    reward,
                    REWARD_FIELDS.REWARD_TOTAL
                  ).toFormat(numbersConfig_1.DECIMAL_PLACES_IN_ADA);
                  const rewardUnspent = (0, lodash_1.get)(
                    reward,
                    REWARD_FIELDS.REWARD_UNSPENT
                  ).toFormat(numbersConfig_1.DECIMAL_PLACES_IN_ADA);
                  const rewardsAddress = (0, lodash_1.get)(
                    reward,
                    REWARD_FIELDS.REWARDS_ADDRESS
                  );
                  return react_1.default.createElement(
                    'tr',
                    { key: key },
                    react_1.default.createElement(
                      'td',
                      { className: StakingRewards_scss_1.default.rewardWallet },
                      rewardWallet
                    ),
                    react_1.default.createElement(
                      'td',
                      {
                        className: StakingRewards_scss_1.default.rewardsAddress,
                      },
                      rewardsAddress &&
                        react_1.default.createElement(
                          'div',
                          null,
                          react_1.default.createElement(
                            react_copy_to_clipboard_1.default,
                            {
                              text: rewardsAddress,
                              onCopy: () => onCopyAddress(rewardsAddress),
                            },
                            react_1.default.createElement(
                              'div',
                              {
                                className:
                                  StakingRewards_scss_1.default
                                    .addressContainer,
                              },
                              react_1.default.createElement(
                                'span',
                                {
                                  className:
                                    StakingRewards_scss_1.default.address,
                                },
                                rewardsAddress
                              ),
                              react_1.default.createElement(
                                'span',
                                {
                                  className:
                                    StakingRewards_scss_1.default.copyAddress,
                                },
                                react_1.default.createElement(
                                  react_svg_inline_1.default,
                                  {
                                    svg: clipboard_ic_inline_svg_1.default,
                                    className:
                                      StakingRewards_scss_1.default.copyIcon,
                                  }
                                )
                              )
                            )
                          ),
                          IS_EXPLORER_LINK_BUTTON_ENABLED &&
                            react_1.default.createElement(
                              ButtonLink_1.default,
                              // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
                              {
                                // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
                                className: explorerButtonClasses,
                                onClick: () =>
                                  onOpenExternalLink(rewardsAddress),
                                skin: ButtonSkin_1.ButtonSkin,
                                label: intl.formatMessage(
                                  messages.actionViewInExplorer
                                ),
                                linkProps: {
                                  className:
                                    StakingRewards_scss_1.default.externalLink,
                                  hasIconBefore: false,
                                  hasIconAfter: true,
                                },
                              }
                            )
                        )
                    ),
                    react_1.default.createElement(
                      'td',
                      { className: StakingRewards_scss_1.default.rewardTotal },
                      !isRestoring &&
                        react_1.default.createElement(
                          RewardAmount_1.RewardAmount,
                          { amount: rewardTotal }
                        ),
                      isRestoring &&
                        react_1.default.createElement(
                          'div',
                          {
                            className:
                              StakingRewards_scss_1.default.syncingProgress,
                          },
                          react_1.default.createElement(
                            PopOver_1.PopOver,
                            {
                              content: intl.formatMessage(
                                messages.syncingTooltipLabel,
                                {
                                  syncingProgress,
                                }
                              ),
                            },
                            react_1.default.createElement(
                              LoadingSpinner_1.default,
                              { medium: true }
                            )
                          )
                        )
                    ),
                    react_1.default.createElement(
                      'td',
                      {
                        className: StakingRewards_scss_1.default.rewardUnspent,
                      },
                      isRestoring
                        ? '-'
                        : react_1.default.createElement(
                            RewardAmount_1.RewardAmount,
                            { amount: rewardUnspent }
                          )
                    )
                  );
                })
              )
            ),
          isLoading &&
            react_1.default.createElement(
              'div',
              {
                className: StakingRewards_scss_1.default.loadingSpinnerWrapper,
              },
              react_1.default.createElement(LoadingSpinner_1.default, null)
            )
        ),
        react_1.default.createElement(
          'div',
          { className: StakingRewards_scss_1.default.note },
          react_1.default.createElement(
            'div',
            null,
            react_1.default.createElement(react_intl_1.FormattedHTMLMessage, {
              ...messages.note,
            })
          )
        )
      )
    );
  }
  handleRewardsSort = (newSortBy) => {
    const { rewardsOrder, rewardsSortBy } = this.state;
    let newRewardsOrder;
    if (rewardsSortBy === newSortBy) {
      // on same sort change order
      newRewardsOrder = rewardsOrder === 'asc' ? 'desc' : 'asc';
    } else {
      // on new sort instance, order by initial value 'descending'
      newRewardsOrder = 'desc';
    }
    this.setState({
      rewardsSortBy: newSortBy,
      rewardsOrder: newRewardsOrder,
    });
  };
};
StakingRewards = __decorate(
  [mobx_react_1.observer, __metadata('design:paramtypes', [Object])],
  StakingRewards
);
exports.StakingRewards = StakingRewards;
//# sourceMappingURL=StakingRewards.js.map
