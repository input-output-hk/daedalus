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
const PopOver_1 = require('@react-polymorph/components/PopOver');
const ButtonSkin_1 = require('@react-polymorph/skins/simple/ButtonSkin');
const bignumber_js_1 = __importDefault(require('bignumber.js'));
const formatters_1 = require('../../../utils/formatters');
const walletsForStakePoolsRanking_1 = require('../../../utils/walletsForStakePoolsRanking');
const stakingConfig_1 = require('../../../config/stakingConfig');
const discreet_mode_1 = require('../../../features/discreet-mode');
const WalletsDropdown_1 = __importDefault(
  require('../../widgets/forms/WalletsDropdown')
);
const ButtonLink_1 = __importDefault(require('../../widgets/ButtonLink'));
const Slider_1 = require('../../widgets/Slider');
const StakingWithNavigation_1 = require('../layouts/StakingWithNavigation');
const StakePoolsRanking_scss_1 = __importDefault(
  require('./StakePoolsRanking.scss')
);
const messages = (0, react_intl_1.defineMessages)({
  rankingAllWallets: {
    id: 'staking.stakePools.rankingAllWallets',
    defaultMessage: '!!!all your wallets',
    description: 'All wallets item of dropdown.',
  },
  rankingAllWalletsEnd: {
    id: 'staking.stakePools.rankingAllWalletsEnd',
    defaultMessage: '!!!.',
    description: 'All wallets description after dropdown.',
  },
  rankingAllWalletsStart: {
    id: 'staking.stakePools.rankingAllWalletsStart',
    defaultMessage:
      '!!!Stake pools are currently ranked based on the combined amount in',
    description: 'All wallets description before dropdown.',
  },
  rankingDescription: {
    id: 'staking.stakePools.rankingDescription',
    defaultMessage:
      '!!!Use the slider to rank the stake pools and check the potential rewards <strong>based on the amount of stake you intend to delegate</strong>.',
    description: 'Ranking description.',
  },
  rankingLearnMoreUrl: {
    id: 'staking.stakePools.rankingLearnMoreUrl',
    defaultMessage: '!!!https://iohk.zendesk.com/hc/en-us',
    description: 'Ranking learn more url.',
  },
  rankingOneWalletEnd: {
    id: 'staking.stakePools.rankingOneWalletEnd',
    defaultMessage: '!!!wallet.',
    description: 'One wallet description after dropdown.',
  },
  rankingOneWalletStart: {
    id: 'staking.stakePools.rankingOneWalletStart',
    defaultMessage:
      '!!!Stake pools are currently ranked based on the amount in',
    description: 'One wallet description before dropdown.',
  },
  rankingSelectWallet: {
    id: 'staking.stakePools.rankingSelectWallet',
    defaultMessage: '!!!select a wallet',
    description: 'Select wallet item of dropdown.',
  },
  rankingSelectWalletEnd: {
    id: 'staking.stakePools.rankingSelectWalletEnd',
    defaultMessage: '!!!to set the amount you intend to delegate.',
    description: 'Select wallet description after dropdown.',
  },
  rankingSelectWalletStart: {
    id: 'staking.stakePools.rankingSelectWalletStart',
    defaultMessage: '!!!Or',
    description: 'Select wallet description before dropdown.',
  },
  rankingExtraTooltip: {
    id: 'staking.stakePools.rankingExtraTooltip',
    defaultMessage: '!!!Circulating supply',
    description: 'Circulating supply slider tooltip.',
  },
  rankingMaxTooltip: {
    id: 'staking.stakePools.rankingMaxTooltip',
    defaultMessage: '!!!Saturation point',
    description: 'Saturation point slider tooltip.',
  },
  rankingMinTooltip: {
    id: 'staking.stakePools.rankingMinTooltip',
    defaultMessage: '!!!Minimum ADA required for staking',
    description: 'Minimum ADA required for staking slider tooltip.',
  },
  actionLearnMore: {
    id: 'staking.stakePools.learnMore',
    defaultMessage: '!!!Learn more',
    description: 'Learn more action of ranking panel.',
  },
});
let StakePoolsRanking = class StakePoolsRanking extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  static defaultProps = {
    wallets: [],
  };
  state = {
    sliderValue: Math.round(
      stakingConfig_1.INITIAL_DELEGATION_FUNDS_LOG *
        stakingConfig_1.RANKING_SLIDER_RATIO
    ),
    displayValue: (0, formatters_1.toFixedUserFormat)(
      stakingConfig_1.INITIAL_DELEGATION_FUNDS,
      0
    ),
  };
  componentDidMount() {
    const { stake } = this.props;
    if (stake) {
      const hasDecimal = stake - Math.floor(stake);
      const displayValue = hasDecimal
        ? (0, formatters_1.formattedWalletAmount)(
            new bignumber_js_1.default(stake),
            false
          )
        : (0, formatters_1.toFixedUserFormat)(stake, 0);
      this.setState({
        sliderValue: Math.round(
          Math.log(stake) * stakingConfig_1.RANKING_SLIDER_RATIO
        ),
        displayValue,
      });
    }
  }
  componentDidUpdate(prevProps) {
    const { selectedDelegationWalletId: prevWalletId } = prevProps;
    const { selectedDelegationWalletId: currentWalletId } = this.props;
    if (prevWalletId !== currentWalletId && currentWalletId) {
      this.onSelectedWalletChange(currentWalletId);
    }
  }
  onSelectedWalletChange = (selectedWalletId) => {
    const {
      wallets,
      updateDelegatingStake,
      rankStakePools,
      selectedDelegationWalletId,
      maxDelegationFunds,
    } = this.props;
    const selectedWallet = wallets.find(
      (wallet) => wallet.id === selectedWalletId
    );
    const hasSelectedWallet = !!selectedWallet;
    const isAllWalletsSelected =
      selectedWalletId === stakingConfig_1.ALL_WALLETS_SELECTION_ID;
    const wasSelectedWalletChanged =
      selectedWalletId !== selectedDelegationWalletId;
    // Prevent ranking stake pools if we don't have data for the selected wallet ready
    if (wasSelectedWalletChanged && !isAllWalletsSelected && !hasSelectedWallet)
      return;
    let amountValue = 0;
    let sliderValue = 0;
    if (selectedWalletId === stakingConfig_1.ALL_WALLETS_SELECTION_ID) {
      amountValue = Math.min(
        (0, walletsForStakePoolsRanking_1.getAllAmounts)(wallets).toNumber(),
        maxDelegationFunds
      );
    } else if (selectedWallet) {
      amountValue = selectedWallet.amount.toNumber();
    }
    amountValue = Math.max(amountValue, stakingConfig_1.MIN_DELEGATION_FUNDS);
    sliderValue = Math.round(
      Math.log(amountValue) * stakingConfig_1.RANKING_SLIDER_RATIO
    );
    const hasSliderValueChanged = sliderValue !== this.state.sliderValue;
    // Prevent ranking stake pools if selected wallet and slider value remains unchanged
    if (!wasSelectedWalletChanged && !hasSliderValueChanged) return;
    const displayValue = (0, formatters_1.formattedWalletAmount)(
      new bignumber_js_1.default(amountValue),
      false
    );
    this.setState({
      sliderValue,
      displayValue,
    });
    updateDelegatingStake(selectedWalletId, amountValue);
    rankStakePools();
  };
  onSliderChange = (sliderValue) => {
    const {
      updateDelegatingStake,
      maxDelegationFunds,
      maxDelegationFundsLog,
    } = this.props;
    let amountValue = null;
    if (
      sliderValue ===
      Math.round(
        stakingConfig_1.MIN_DELEGATION_FUNDS_LOG *
          stakingConfig_1.RANKING_SLIDER_RATIO
      )
    ) {
      amountValue = stakingConfig_1.MIN_DELEGATION_FUNDS;
    } else if (
      sliderValue ===
      Math.round(maxDelegationFundsLog * stakingConfig_1.RANKING_SLIDER_RATIO)
    ) {
      amountValue = maxDelegationFunds;
    } else {
      amountValue = (0, formatters_1.generateThousands)(
        Math.exp(sliderValue / stakingConfig_1.RANKING_SLIDER_RATIO)
      );
    }
    const displayValue = (0, formatters_1.toFixedUserFormat)(amountValue, 0);
    this.setState({
      sliderValue,
      displayValue,
    });
    updateDelegatingStake(null, amountValue);
  };
  generateInfo = () => {
    const { intl } = this.context;
    const { wallets, selectedDelegationWalletId } = this.props;
    const allWalletsItem = {
      id: stakingConfig_1.ALL_WALLETS_SELECTION_ID,
      name: intl.formatMessage(messages.rankingAllWallets),
      amount: (0, walletsForStakePoolsRanking_1.getAllAmounts)(wallets),
    };
    const filteredWallets = (0,
    walletsForStakePoolsRanking_1.getFilteredWallets)(wallets);
    const walletSelectorWallets = [allWalletsItem, ...filteredWallets];
    const walletSelectorClasses = (0, classnames_1.default)([
      StakePoolsRanking_scss_1.default.walletSelector,
      selectedDelegationWalletId === null ? 'noValueSelected' : null,
    ]);
    const walletSelectorContainerClasses = (0, classnames_1.default)([
      StakePoolsRanking_scss_1.default.walletSelectorContainer,
      StakePoolsRanking_scss_1.default.col,
    ]);
    const learnMoreUrl = intl.formatMessage(messages.rankingLearnMoreUrl);
    let walletSelectionStart = null;
    let walletSelectionEnd = null;
    if (selectedDelegationWalletId === null) {
      walletSelectionStart = intl.formatMessage(
        messages.rankingSelectWalletStart
      );
      walletSelectionEnd = intl.formatMessage(messages.rankingSelectWalletEnd);
    } else if (
      selectedDelegationWalletId === stakingConfig_1.ALL_WALLETS_SELECTION_ID
    ) {
      walletSelectionStart = intl.formatMessage(
        messages.rankingAllWalletsStart
      );
      walletSelectionEnd = intl.formatMessage(messages.rankingAllWalletsEnd);
    } else {
      walletSelectionStart = intl.formatMessage(messages.rankingOneWalletStart);
      walletSelectionEnd = intl.formatMessage(messages.rankingOneWalletEnd);
    }
    return {
      walletSelectorWallets,
      walletSelectorClasses,
      walletSelectorContainerClasses,
      walletSelectionStart,
      walletSelectionEnd,
      learnMoreUrl,
    };
  };
  render() {
    const { intl } = this.context;
    const {
      onOpenExternalLink,
      isLoading,
      isRanking,
      selectedDelegationWalletId,
      wallets,
      numberOfStakePools,
      getStakePoolById,
      rankStakePools,
      maxDelegationFunds,
      maxDelegationFundsLog,
      discreetModeFeature,
    } = this.props;
    const { sliderValue, displayValue } = this.state;
    const learnMoreButtonClasses = (0, classnames_1.default)([
      'flat',
      StakePoolsRanking_scss_1.default.actionLearnMore,
    ]);
    const {
      walletSelectorWallets,
      walletSelectorClasses,
      walletSelectorContainerClasses,
      walletSelectionStart,
      walletSelectionEnd,
      learnMoreUrl,
    } = this.generateInfo();
    if (!stakingConfig_1.IS_RANKING_DATA_AVAILABLE) {
      return null;
    }
    const shouldDisplayWalletsDropdown =
      !discreetModeFeature.isDiscreetMode &&
      (0, walletsForStakePoolsRanking_1.getFilteredWallets)(wallets).length > 0;
    return react_1.default.createElement(
      'div',
      { className: StakePoolsRanking_scss_1.default.component },
      react_1.default.createElement(
        'div',
        { className: StakePoolsRanking_scss_1.default.upper },
        react_1.default.createElement(
          'div',
          { className: StakePoolsRanking_scss_1.default.selectWallet },
          react_1.default.createElement(
            'div',
            { className: StakePoolsRanking_scss_1.default.row },
            react_1.default.createElement(
              'div',
              { className: StakePoolsRanking_scss_1.default.col },
              react_1.default.createElement(react_intl_1.FormattedHTMLMessage, {
                ...messages.rankingDescription,
              })
            )
          ),
          shouldDisplayWalletsDropdown
            ? react_1.default.createElement(
                'div',
                { className: StakePoolsRanking_scss_1.default.row },
                react_1.default.createElement(
                  'div',
                  { className: StakePoolsRanking_scss_1.default.col },
                  walletSelectionStart
                ),
                react_1.default.createElement(
                  'div',
                  { className: walletSelectorContainerClasses },
                  react_1.default.createElement(WalletsDropdown_1.default, {
                    className: walletSelectorClasses,
                    // @ts-ignore ts-migrate(2322) FIXME: Type '{ className: any; placeholder: any; wallets:... Remove this comment to see the full error message
                    placeholder: intl.formatMessage(
                      messages.rankingSelectWallet
                    ),
                    wallets: walletSelectorWallets,
                    onChange: this.onSelectedWalletChange,
                    disabled: isLoading || isRanking,
                    value: selectedDelegationWalletId || '0',
                    selectionRenderer: (option) =>
                      react_1.default.createElement(
                        'button',
                        {
                          className: 'customValue',
                          onClick: () => {
                            const selectionInput = document.querySelector(
                              '.StakePoolsRanking_walletSelectorContainer input'
                            );
                            if (selectionInput) {
                              // @ts-ignore ts-migrate(2339) FIXME: Property 'click' does not exist on type 'Element'.
                              selectionInput.click();
                            }
                          },
                        },
                        option.label
                      ),
                    numberOfStakePools: numberOfStakePools,
                    getStakePoolById: getStakePoolById,
                  })
                ),
                react_1.default.createElement(
                  'div',
                  { className: StakePoolsRanking_scss_1.default.col },
                  walletSelectionEnd
                )
              )
            : null
        ),
        false // eslint-disable-line
          ? react_1.default.createElement(
              ButtonLink_1.default,
              // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
              {
                // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
                className: learnMoreButtonClasses,
                onClick: () => onOpenExternalLink(learnMoreUrl),
                skin: ButtonSkin_1.ButtonSkin,
                label: intl.formatMessage(messages.actionLearnMore),
                linkProps: {
                  className: StakePoolsRanking_scss_1.default.externalLink,
                  hasIconBefore: false,
                  hasIconAfter: true,
                },
              }
            )
          : null
      ),
      react_1.default.createElement(
        StakingWithNavigation_1.StakingPageScrollContext.Consumer,
        null,
        ({ scrollElementRef }) =>
          react_1.default.createElement(
            'div',
            { className: StakePoolsRanking_scss_1.default.lower },
            react_1.default.createElement(
              'div',
              { className: StakePoolsRanking_scss_1.default.row },
              react_1.default.createElement(
                'div',
                { className: StakePoolsRanking_scss_1.default.col },
                react_1.default.createElement('div', {
                  className:
                    StakePoolsRanking_scss_1.default.outOfSliderRangeStart,
                })
              ),
              react_1.default.createElement(
                'div',
                { className: StakePoolsRanking_scss_1.default.slider },
                react_1.default.createElement(Slider_1.Slider, {
                  min: Math.round(
                    stakingConfig_1.MIN_DELEGATION_FUNDS_LOG *
                      stakingConfig_1.RANKING_SLIDER_RATIO
                  ),
                  minDisplayValue: stakingConfig_1.MIN_DELEGATION_FUNDS,
                  max: Math.round(
                    maxDelegationFundsLog * stakingConfig_1.RANKING_SLIDER_RATIO
                  ),
                  maxDisplayValue: maxDelegationFunds,
                  value: sliderValue,
                  displayValue: displayValue,
                  showRawValue: true,
                  onChange: this.onSliderChange,
                  onAfterChange: rankStakePools,
                  disabled: isLoading || isRanking,
                  showTooltip: true,
                  minTooltip: intl.formatMessage(messages.rankingMinTooltip),
                  maxTooltip: intl.formatMessage(messages.rankingMaxTooltip),
                  tooltipAppendTo: () => scrollElementRef.current,
                })
              ),
              react_1.default.createElement(
                'div',
                { className: StakePoolsRanking_scss_1.default.col },
                react_1.default.createElement(
                  'div',
                  {
                    className:
                      StakePoolsRanking_scss_1.default.outOfRangeMaxAmount,
                  },
                  react_1.default.createElement(
                    PopOver_1.PopOver,
                    {
                      content: intl.formatMessage(messages.rankingExtraTooltip),
                      appendTo: () => scrollElementRef.current,
                    },
                    (0, formatters_1.shortNumber)(
                      stakingConfig_1.CIRCULATING_SUPPLY
                    )
                  )
                ),
                react_1.default.createElement('div', {
                  className:
                    StakePoolsRanking_scss_1.default.outOfSliderRangeEnd,
                })
              )
            )
          )
      )
    );
  }
};
StakePoolsRanking = __decorate([mobx_react_1.observer], StakePoolsRanking);
exports.default = (0, discreet_mode_1.withDiscreetMode)(StakePoolsRanking);
//# sourceMappingURL=StakePoolsRanking.js.map
