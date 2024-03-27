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
const react_1 = __importStar(require('react'));
const mobx_react_1 = require('mobx-react');
const lodash_1 = require('lodash');
const react_intl_1 = require('react-intl');
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
const classnames_1 = __importDefault(require('classnames'));
const PopOver_1 = require('@react-polymorph/components/PopOver');
const Wallet_1 = require('../../../domains/Wallet');
const colors_1 = require('../../../utils/colors');
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/ada-sym... Remove this comment to see the full error message
const ada_symbol_inline_svg_1 = __importDefault(
  require('../../../assets/images/ada-symbol.inline.svg')
);
const numbersConfig_1 = require('../../../config/numbersConfig');
const PoolPopOver_1 = require('../widgets/PoolPopOver');
const WalletRow_scss_1 = __importDefault(require('./WalletRow.scss'));
const WalletRowPopOverOverrides_scss_1 = __importDefault(
  require('./WalletRowPopOverOverrides.scss')
);
const LoadingSpinner_1 = __importDefault(
  require('../../widgets/LoadingSpinner')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/collapse... Remove this comment to see the full error message
const collapse_arrow_inline_svg_1 = __importDefault(
  require('../../../assets/images/collapse-arrow.inline.svg')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/hardware... Remove this comment to see the full error message
const connect_ic_inline_svg_1 = __importDefault(
  require('../../../assets/images/hardware-wallet/connect-ic.inline.svg')
);
const stakingConfig_1 = require('../../../config/stakingConfig');
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/no-data... Remove this comment to see the full error message
const no_data_dash_big_inline_svg_1 = __importDefault(
  require('../../../assets/images/no-data-dash-big.inline.svg')
);
const widgets_1 = require('./widgets');
const messages = (0, react_intl_1.defineMessages)({
  walletAmount: {
    id: 'staking.delegationCenter.walletAmount',
    defaultMessage: '!!!{amount} ADA',
    description:
      'Amount of each wallet for the Delegation center body section.',
  },
  notDelegated: {
    id: 'staking.delegationCenter.notDelegated',
    defaultMessage: '!!!Undelegated',
    description: 'Undelegated label for the Delegation center body section.',
  },
  removeDelegation: {
    id: 'staking.delegationCenter.removeDelegation',
    defaultMessage: '!!!Undelegate',
    description:
      'Remove delegation label for the Delegation center body section.',
  },
  TooltipPoolTickerEpoch: {
    id: 'staking.delegationCenter.stakePoolTooltipTickerEpoch',
    defaultMessage: '!!!From epoch {fromEpoch}',
    description:
      'Delegated stake pool tooltip ticker for the Delegation center body section.',
  },
  TooltipPoolTickerEarningRewards: {
    id: 'staking.delegationCenter.stakePoolTooltipTickerEarningRewards',
    defaultMessage: '!!!Currently earning rewards',
    description:
      'Delegated stake pool tooltip ticker for the Delegation center body section.',
  },
  delegate: {
    id: 'staking.delegationCenter.delegate',
    defaultMessage: '!!!Delegate',
    description: 'Delegate label for the Delegation center body section.',
  },
  redelegate: {
    id: 'staking.delegationCenter.redelegate',
    defaultMessage: '!!!Redelegate',
    description: 'Redelegate label for the Delegation center body section.',
  },
  unknownStakePoolLabel: {
    id: 'staking.delegationCenter.unknownStakePoolLabel',
    defaultMessage: '!!!unknown',
    description:
      'unknown stake pool label for the Delegation center body section.',
  },
  syncingTooltipLabel: {
    id: 'staking.delegationCenter.syncingTooltipLabel',
    defaultMessage: '!!!Syncing {syncingProgress}%',
    description:
      'unknown stake pool label for the Delegation center body section.',
  },
});
const initialWalletRowState = {
  highlightedPoolId: false,
};
let WalletRow = class WalletRow extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  state = { ...initialWalletRowState };
  stakePoolFirstTileRef;
  stakePoolAdaSymbolRef;
  constructor(props) {
    super(props);
    this.stakePoolFirstTileRef = react_1.default.createRef();
    this.stakePoolAdaSymbolRef = react_1.default.createRef();
  }
  componentDidUpdate() {
    this.handleFirstTilePopOverStyle();
  }
  handleFirstTilePopOverStyle = () => {
    const {
      wallet: { id },
    } = this.props;
    const existingStyle = document.getElementById(`wallet-row-${id}-style`);
    const { current: firstTileDom } = this.stakePoolFirstTileRef;
    const { current: adaSymbolDom } = this.stakePoolAdaSymbolRef;
    if (!firstTileDom || !adaSymbolDom) {
      if (existingStyle) {
        existingStyle.remove();
      }
      return;
    }
    if (existingStyle) {
      return;
    }
    const firstTileDomRect = firstTileDom.getBoundingClientRect();
    const adaSymbolDomRect = adaSymbolDom.getBoundingClientRect();
    const horizontalDelta =
      firstTileDomRect.width / 2 -
      adaSymbolDomRect.width / 2 -
      (adaSymbolDomRect.left - firstTileDomRect.left);
    const firstTilePopOverStyle = document.createElement('style');
    firstTilePopOverStyle.setAttribute('id', `wallet-row-${id}-style`);
    firstTilePopOverStyle.innerHTML = `.wallet-row-${id} .tippy-arrow { transform: translate(-${horizontalDelta}px, 0); }`;
    document.getElementsByTagName('head')[0].appendChild(firstTilePopOverStyle);
  };
  handlePopOverOpen = () => {
    this.setState({
      highlightedPoolId: true,
    });
  };
  handlePopOverClose = () => {
    this.setState({
      highlightedPoolId: false,
    });
  };
  getPendingDelegatedStakePoolId = (epochNumber, fallbackStakePoolId) => {
    const {
      wallet: { pendingDelegations },
    } = this.props;
    if (!pendingDelegations || !pendingDelegations.length) {
      return fallbackStakePoolId;
    }
    const foundDelegation = pendingDelegations.find(
      (delegation) =>
        (0, lodash_1.get)(delegation, ['changes_at', 'epoch_number'], 0) ===
        epochNumber
    );
    if (!foundDelegation) {
      return fallbackStakePoolId;
    }
    const isDelegating =
      (0, lodash_1.get)(foundDelegation, 'status') ===
      Wallet_1.WalletDelegationStatuses.DELEGATING;
    if (!isDelegating) {
      return null;
    }
    return (0, lodash_1.get)(foundDelegation, 'target', null);
  };
  render() {
    const { intl } = this.context;
    const {
      wallet: {
        name,
        amount,
        isRestoring,
        syncState,
        delegatedStakePoolId,
        isHardwareWallet,
        id,
      },
      delegatedStakePool,
      numberOfRankedStakePools,
      getStakePoolById,
      onDelegate,
      onUndelegate,
      nextEpochNumber,
      futureEpochNumber,
      currentTheme,
      onOpenExternalLink,
      showWithSelectButton,
      containerClassName,
    } = this.props;
    const { highlightedPoolId } = this.state;
    // @TODO - remove once quit stake pool delegation is connected with rewards balance
    const isUndelegateBlocked = true;
    const syncingProgress = (0, lodash_1.get)(
      syncState,
      'progress.quantity',
      ''
    );
    const notDelegatedText = intl.formatMessage(messages.notDelegated);
    const removeDelegationText = intl.formatMessage(messages.removeDelegation);
    const delegateText = intl.formatMessage(messages.delegate);
    const redelegateText = intl.formatMessage(messages.redelegate);
    const nextPendingDelegatedStakePoolId = this.getPendingDelegatedStakePoolId(
      nextEpochNumber,
      delegatedStakePoolId
    );
    const nextPendingDelegatedStakePool = nextPendingDelegatedStakePoolId
      ? getStakePoolById(nextPendingDelegatedStakePoolId)
      : null;
    const futurePendingDelegatedStakePoolId = this.getPendingDelegatedStakePoolId(
      futureEpochNumber,
      nextPendingDelegatedStakePoolId
    );
    const futurePendingDelegatedStakePool = futurePendingDelegatedStakePoolId
      ? getStakePoolById(futurePendingDelegatedStakePoolId)
      : null;
    const stakePoolRankingColor = futurePendingDelegatedStakePool
      ? (0, colors_1.getColorFromRange)(
          futurePendingDelegatedStakePool.ranking,
          numberOfRankedStakePools
        )
      : '';
    const saturationStyles = (0, classnames_1.default)([
      WalletRow_scss_1.default.saturationBar,
      futurePendingDelegatedStakePool
        ? WalletRow_scss_1.default[
            (0, colors_1.getSaturationColor)(
              futurePendingDelegatedStakePool.saturation
            )
          ]
        : null,
    ]);
    const futureStakePoolTileStyles = (0, classnames_1.default)([
      WalletRow_scss_1.default.stakePoolTile,
      highlightedPoolId ? WalletRow_scss_1.default.active : null,
      futurePendingDelegatedStakePoolId && futurePendingDelegatedStakePool
        ? WalletRow_scss_1.default.futureStakePoolTileDelegated
        : WalletRow_scss_1.default.futureStakePoolTileUndelegated,
      futurePendingDelegatedStakePoolId && !futurePendingDelegatedStakePool
        ? WalletRow_scss_1.default.futureStakePoolTileUndefined
        : null,
    ]);
    const rightContainerStyles = (0, classnames_1.default)([
      WalletRow_scss_1.default.right,
      isRestoring ? WalletRow_scss_1.default.isRestoring : null,
    ]);
    const actionButtonStyles = (0, classnames_1.default)([
      WalletRow_scss_1.default.action,
      highlightedPoolId ? WalletRow_scss_1.default.active : null,
    ]);
    return react_1.default.createElement(
      'div',
      { className: WalletRow_scss_1.default.component },
      react_1.default.createElement(
        'div',
        { className: WalletRow_scss_1.default.left },
        react_1.default.createElement(
          'div',
          { className: WalletRow_scss_1.default.title },
          name,
          isHardwareWallet &&
            react_1.default.createElement(react_svg_inline_1.default, {
              svg: connect_ic_inline_svg_1.default,
              className: WalletRow_scss_1.default.hardwareWalletsIcon,
            })
        ),
        react_1.default.createElement(
          'div',
          { className: WalletRow_scss_1.default.description },
          !isRestoring
            ? react_1.default.createElement(widgets_1.WalletAmount, {
                walletAmount: messages.walletAmount,
                amount: amount.toFormat(numbersConfig_1.DECIMAL_PLACES_IN_ADA),
              })
            : '-'
        )
      ),
      react_1.default.createElement(
        'div',
        { className: rightContainerStyles },
        !isRestoring
          ? react_1.default.createElement(
              react_1.Fragment,
              null,
              delegatedStakePoolId
                ? react_1.default.createElement(
                    PopOver_1.PopOver,
                    {
                      themeOverrides: WalletRowPopOverOverrides_scss_1.default,
                      className: `wallet-row-${id}`,
                      content: react_1.default.createElement(
                        'div',
                        {
                          className:
                            WalletRow_scss_1.default.tooltipLabelWrapper,
                        },
                        react_1.default.createElement(
                          'span',
                          null,
                          intl.formatMessage(
                            messages.TooltipPoolTickerEarningRewards
                          )
                        )
                      ),
                    },
                    react_1.default.createElement(
                      'div',
                      {
                        className: WalletRow_scss_1.default.stakePoolTile,
                        ref: this.stakePoolFirstTileRef,
                      },
                      react_1.default.createElement(
                        'div',
                        null,
                        delegatedStakePool
                          ? react_1.default.createElement(
                              'div',
                              {
                                className:
                                  WalletRow_scss_1.default.stakePoolName,
                              },
                              react_1.default.createElement(
                                'div',
                                {
                                  className:
                                    WalletRow_scss_1.default.activeAdaSymbol,
                                  ref: this.stakePoolAdaSymbolRef,
                                },
                                react_1.default.createElement(
                                  react_svg_inline_1.default,
                                  { svg: ada_symbol_inline_svg_1.default }
                                )
                              ),
                              react_1.default.createElement(
                                'div',
                                {
                                  className:
                                    WalletRow_scss_1.default.stakePoolTicker,
                                },
                                delegatedStakePool.ticker
                              )
                            )
                          : react_1.default.createElement(
                              'div',
                              {
                                className:
                                  WalletRow_scss_1.default.stakePoolUnknown,
                              },
                              intl.formatMessage(messages.unknownStakePoolLabel)
                            )
                      )
                    )
                  )
                : react_1.default.createElement(
                    'div',
                    { className: WalletRow_scss_1.default.stakePoolTile },
                    react_1.default.createElement(
                      'div',
                      { className: WalletRow_scss_1.default.nonDelegatedText },
                      notDelegatedText
                    )
                  ),
              react_1.default.createElement(react_svg_inline_1.default, {
                svg: collapse_arrow_inline_svg_1.default,
                className: WalletRow_scss_1.default.arrow,
              }),
              react_1.default.createElement(
                'div',
                { className: WalletRow_scss_1.default.stakePoolTile },
                nextPendingDelegatedStakePoolId
                  ? react_1.default.createElement(
                      'div',
                      null,
                      nextPendingDelegatedStakePool
                        ? react_1.default.createElement(
                            'div',
                            {
                              className:
                                WalletRow_scss_1.default.stakePoolTicker,
                            },
                            nextPendingDelegatedStakePool.ticker
                          )
                        : react_1.default.createElement(
                            'div',
                            {
                              className:
                                WalletRow_scss_1.default.stakePoolUnknown,
                            },
                            intl.formatMessage(messages.unknownStakePoolLabel)
                          )
                    )
                  : react_1.default.createElement(
                      'div',
                      { className: WalletRow_scss_1.default.nonDelegatedText },
                      notDelegatedText
                    )
              ),
              react_1.default.createElement(react_svg_inline_1.default, {
                svg: collapse_arrow_inline_svg_1.default,
                className: WalletRow_scss_1.default.arrow,
              }),
              react_1.default.createElement(
                'div',
                { className: futureStakePoolTileStyles },
                futurePendingDelegatedStakePoolId
                  ? react_1.default.createElement(
                      react_1.default.Fragment,
                      null,
                      futurePendingDelegatedStakePool
                        ? react_1.default.createElement(
                            PoolPopOver_1.PoolPopOver,
                            {
                              openOnHover: true,
                              color: stakePoolRankingColor,
                              currentTheme: currentTheme,
                              onClose: this.handlePopOverClose,
                              onOpen: this.handlePopOverOpen,
                              onOpenExternalLink: onOpenExternalLink,
                              openWithDelay: false,
                              stakePool: futurePendingDelegatedStakePool,
                              containerClassName: containerClassName,
                              numberOfRankedStakePools: numberOfRankedStakePools,
                              showWithSelectButton: showWithSelectButton,
                            },
                            react_1.default.createElement(
                              'div',
                              {
                                className:
                                  WalletRow_scss_1.default.stakePoolTicker,
                              },
                              futurePendingDelegatedStakePool.ticker
                            ),
                            stakingConfig_1.IS_RANKING_DATA_AVAILABLE
                              ? react_1.default.createElement(
                                  'div',
                                  {
                                    className: WalletRow_scss_1.default.ranking,
                                    style: {
                                      color: stakePoolRankingColor,
                                    },
                                  },
                                  futurePendingDelegatedStakePool.nonMyopicMemberRewards
                                    ? futurePendingDelegatedStakePool.ranking
                                    : react_1.default.createElement(
                                        react_1.default.Fragment,
                                        null,
                                        numberOfRankedStakePools + 1,
                                        react_1.default.createElement(
                                          'sup',
                                          null,
                                          '*'
                                        )
                                      )
                                )
                              : react_1.default.createElement(
                                  'div',
                                  {
                                    className:
                                      WalletRow_scss_1.default.noDataDash,
                                  },
                                  react_1.default.createElement(
                                    react_svg_inline_1.default,
                                    {
                                      svg:
                                        no_data_dash_big_inline_svg_1.default,
                                    }
                                  )
                                ),
                            stakingConfig_1.IS_SATURATION_DATA_AVAILABLE &&
                              react_1.default.createElement(
                                'div',
                                { className: saturationStyles },
                                react_1.default.createElement('span', {
                                  style: {
                                    width: `${parseFloat(
                                      futurePendingDelegatedStakePool.saturation
                                    ).toFixed(2)}%`,
                                  },
                                })
                              ),
                            react_1.default.createElement('div', {
                              className:
                                WalletRow_scss_1.default
                                  .stakePoolRankingIndicator,
                              style: {
                                background: stakePoolRankingColor,
                              },
                            })
                          )
                        : react_1.default.createElement(
                            'div',
                            {
                              className:
                                WalletRow_scss_1.default.stakePoolUnknown,
                            },
                            intl.formatMessage(messages.unknownStakePoolLabel)
                          )
                    )
                  : react_1.default.createElement(
                      'div',
                      { className: WalletRow_scss_1.default.nonDelegatedText },
                      notDelegatedText
                    ),
                futurePendingDelegatedStakePoolId &&
                  !isUndelegateBlocked &&
                  react_1.default.createElement(
                    'div',
                    {
                      className: actionButtonStyles,
                      role: 'presentation',
                      onClick: onUndelegate,
                      key: 'undelegate',
                    },
                    removeDelegationText
                  ),
                react_1.default.createElement(
                  'div',
                  {
                    className: actionButtonStyles,
                    role: 'presentation',
                    onClick: onDelegate,
                  },
                  !futurePendingDelegatedStakePoolId
                    ? delegateText
                    : redelegateText
                )
              )
            )
          : react_1.default.createElement(
              PopOver_1.PopOver,
              {
                content: intl.formatMessage(messages.syncingTooltipLabel, {
                  syncingProgress,
                }),
              },
              react_1.default.createElement(LoadingSpinner_1.default, {
                medium: true,
              })
            )
      )
    );
  }
};
WalletRow = __decorate(
  [mobx_react_1.observer, __metadata('design:paramtypes', [Object])],
  WalletRow
);
exports.default = WalletRow;
//# sourceMappingURL=WalletRow.js.map
