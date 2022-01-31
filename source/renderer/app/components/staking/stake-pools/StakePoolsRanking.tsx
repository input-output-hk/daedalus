import React, { Component } from 'react';
// @ts-ignore ts-migrate(2305) FIXME: Module '"react"' has no exported member 'Config'.
import type { Config } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import classnames from 'classnames';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import BigNumber from 'bignumber.js';
import {
  shortNumber,
  generateThousands,
  formattedWalletAmount,
  toFixedUserFormat,
} from '../../../utils/formatters';
import {
  getFilteredWallets,
  getAllAmounts,
} from '../../../utils/walletsForStakePoolsRanking';
import Wallet from '../../../domains/Wallet';
import {
  RANKING_SLIDER_RATIO,
  MIN_DELEGATION_FUNDS_LOG,
  MIN_DELEGATION_FUNDS,
  INITIAL_DELEGATION_FUNDS_LOG,
  INITIAL_DELEGATION_FUNDS,
  CIRCULATING_SUPPLY,
  ALL_WALLETS_SELECTION_ID,
  IS_RANKING_DATA_AVAILABLE,
} from '../../../config/stakingConfig';
import { withDiscreetMode } from '../../../features/discreet-mode';
import type { DiscreetModeFeature } from '../../../features/discreet-mode';
import WalletsDropdown from '../../widgets/forms/WalletsDropdown';
import ButtonLink from '../../widgets/ButtonLink';
import { Slider } from '../../widgets/Slider';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './StakePoolsRanking.scss' or i... Remove this comment to see the full error message
import styles from './StakePoolsRanking.scss';

const messages = defineMessages({
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
type InjectedProps = {
  discreetModeFeature: DiscreetModeFeature;
};
type Props = InjectedProps & {
  wallets: Array<Wallet>;
  onOpenExternalLink: (...args: Array<any>) => any;
  updateDelegatingStake: (...args: Array<any>) => any;
  rankStakePools: (...args: Array<any>) => any;
  selectedDelegationWalletId?: string | null | undefined;
  stake?: number | null | undefined;
  isLoading: boolean;
  isRanking: boolean;
  numberOfStakePools: number;
  getStakePoolById: (...args: Array<any>) => any;
  maxDelegationFunds: number;
  maxDelegationFundsLog: number;
};
type State = {
  sliderValue: number;
  displayValue: string;
};

@observer
class StakePoolsRanking extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };
  static defaultProps = {
    wallets: [],
  };
  state = {
    sliderValue: Math.round(
      INITIAL_DELEGATION_FUNDS_LOG * RANKING_SLIDER_RATIO
    ),
    displayValue: toFixedUserFormat(INITIAL_DELEGATION_FUNDS, 0),
  };

  componentDidMount() {
    const { stake } = this.props;

    if (stake) {
      const hasDecimal = stake - Math.floor(stake);
      const displayValue = hasDecimal
        ? formattedWalletAmount(new BigNumber(stake), false)
        : toFixedUserFormat(stake, 0);
      this.setState({
        sliderValue: Math.round(Math.log(stake) * RANKING_SLIDER_RATIO),
        displayValue,
      });
    }
  }

  componentDidUpdate(prevProps: Props) {
    const { selectedDelegationWalletId: prevWalletId } = prevProps;
    const { selectedDelegationWalletId: currentWalletId } = this.props;

    if (prevWalletId !== currentWalletId && currentWalletId) {
      this.onSelectedWalletChange(currentWalletId);
    }
  }

  onSelectedWalletChange = (selectedWalletId: string) => {
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
    const isAllWalletsSelected = selectedWalletId === ALL_WALLETS_SELECTION_ID;
    const wasSelectedWalletChanged =
      selectedWalletId !== selectedDelegationWalletId;
    // Prevent ranking stake pools if we don't have data for the selected wallet ready
    if (wasSelectedWalletChanged && !isAllWalletsSelected && !hasSelectedWallet)
      return;
    let amountValue = 0;
    let sliderValue = 0;

    if (selectedWalletId === ALL_WALLETS_SELECTION_ID) {
      amountValue = Math.min(
        getAllAmounts(wallets).toNumber(),
        maxDelegationFunds
      );
    } else if (selectedWallet) {
      amountValue = selectedWallet.amount.toNumber();
    }

    amountValue = Math.max(amountValue, MIN_DELEGATION_FUNDS);
    sliderValue = Math.round(Math.log(amountValue) * RANKING_SLIDER_RATIO);
    const hasSliderValueChanged = sliderValue !== this.state.sliderValue;
    // Prevent ranking stake pools if selected wallet and slider value remains unchanged
    if (!wasSelectedWalletChanged && !hasSliderValueChanged) return;
    const displayValue = formattedWalletAmount(
      new BigNumber(amountValue),
      false
    );
    this.setState({
      sliderValue,
      displayValue,
    });
    updateDelegatingStake(selectedWalletId, amountValue);
    rankStakePools();
  };
  onSliderChange = (sliderValue: number) => {
    const {
      updateDelegatingStake,
      maxDelegationFunds,
      maxDelegationFundsLog,
    } = this.props;
    let amountValue = null;

    if (
      sliderValue ===
      Math.round(MIN_DELEGATION_FUNDS_LOG * RANKING_SLIDER_RATIO)
    ) {
      amountValue = MIN_DELEGATION_FUNDS;
    } else if (
      sliderValue === Math.round(maxDelegationFundsLog * RANKING_SLIDER_RATIO)
    ) {
      amountValue = maxDelegationFunds;
    } else {
      amountValue = generateThousands(
        Math.exp(sliderValue / RANKING_SLIDER_RATIO)
      );
    }

    const displayValue = toFixedUserFormat(amountValue, 0);
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
      id: ALL_WALLETS_SELECTION_ID,
      name: intl.formatMessage(messages.rankingAllWallets),
      amount: getAllAmounts(wallets),
    };
    const filteredWallets = getFilteredWallets(wallets);
    const walletSelectorWallets = [allWalletsItem, ...filteredWallets];
    const walletSelectorClasses = classnames([
      styles.walletSelector,
      selectedDelegationWalletId === null ? 'noValueSelected' : null,
    ]);
    const walletSelectorContainerClasses = classnames([
      styles.walletSelectorContainer,
      styles.col,
    ]);
    const learnMoreUrl = intl.formatMessage(messages.rankingLearnMoreUrl);
    let walletSelectionStart = null;
    let walletSelectionEnd = null;

    if (selectedDelegationWalletId === null) {
      walletSelectionStart = intl.formatMessage(
        messages.rankingSelectWalletStart
      );
      walletSelectionEnd = intl.formatMessage(messages.rankingSelectWalletEnd);
    } else if (selectedDelegationWalletId === ALL_WALLETS_SELECTION_ID) {
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
    const learnMoreButtonClasses = classnames(['flat', styles.actionLearnMore]);
    const {
      walletSelectorWallets,
      walletSelectorClasses,
      walletSelectorContainerClasses,
      walletSelectionStart,
      walletSelectionEnd,
      learnMoreUrl,
    } = this.generateInfo();

    if (!IS_RANKING_DATA_AVAILABLE) {
      return null;
    }

    const shouldDisplayWalletsDropdown =
      !discreetModeFeature.isDiscreetMode &&
      getFilteredWallets(wallets).length > 0;
    return (
      <div className={styles.component}>
        <div className={styles.upper}>
          <div className={styles.selectWallet}>
            <div className={styles.row}>
              <div className={styles.col}>
                <FormattedHTMLMessage {...messages.rankingDescription} />
              </div>
            </div>
            {shouldDisplayWalletsDropdown ? (
              <div className={styles.row}>
                <div className={styles.col}>{walletSelectionStart}</div>
                <div className={walletSelectorContainerClasses}>
                  <WalletsDropdown
                    className={walletSelectorClasses}
                    // @ts-ignore ts-migrate(2322) FIXME: Type '{ className: any; placeholder: any; wallets:... Remove this comment to see the full error message
                    placeholder={intl.formatMessage(
                      messages.rankingSelectWallet
                    )}
                    wallets={walletSelectorWallets}
                    onChange={this.onSelectedWalletChange}
                    disabled={isLoading || isRanking}
                    value={selectedDelegationWalletId || '0'}
                    selectionRenderer={(option) => (
                      <button
                        className="customValue"
                        onClick={() => {
                          const selectionInput = document.querySelector(
                            '.StakePoolsRanking_walletSelectorContainer input'
                          );

                          if (selectionInput) {
                            // @ts-ignore ts-migrate(2339) FIXME: Property 'click' does not exist on type 'Element'.
                            selectionInput.click();
                          }
                        }}
                      >
                        {option.label}
                      </button>
                    )}
                    numberOfStakePools={numberOfStakePools}
                    getStakePoolById={getStakePoolById}
                  />
                </div>
                <div className={styles.col}>{walletSelectionEnd}</div>
              </div>
            ) : null}
          </div>
          {false ? ( // eslint-disable-line
            <ButtonLink
              // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
              className={learnMoreButtonClasses}
              onClick={() => onOpenExternalLink(learnMoreUrl)}
              skin={ButtonSkin}
              label={intl.formatMessage(messages.actionLearnMore)}
              linkProps={{
                className: styles.externalLink,
                hasIconBefore: false,
                hasIconAfter: true,
              }}
            />
          ) : null}
        </div>
        <div className={styles.lower}>
          <div className={styles.row}>
            <div className={styles.col}>
              <div className={styles.outOfSliderRangeStart} />
            </div>
            <div className={styles.slider}>
              <Slider
                min={Math.round(
                  MIN_DELEGATION_FUNDS_LOG * RANKING_SLIDER_RATIO
                )}
                minDisplayValue={MIN_DELEGATION_FUNDS}
                max={Math.round(maxDelegationFundsLog * RANKING_SLIDER_RATIO)}
                maxDisplayValue={maxDelegationFunds}
                value={sliderValue}
                displayValue={displayValue}
                showRawValue
                onChange={this.onSliderChange}
                onAfterChange={rankStakePools}
                disabled={isLoading || isRanking}
                showTooltip
                minTooltip={intl.formatMessage(messages.rankingMinTooltip)}
                maxTooltip={intl.formatMessage(messages.rankingMaxTooltip)}
              />
            </div>
            <div className={styles.col}>
              <div className={styles.outOfRangeMaxAmount}>
                <PopOver
                  content={intl.formatMessage(messages.rankingExtraTooltip)}
                >
                  {shortNumber(CIRCULATING_SUPPLY)}
                </PopOver>
              </div>
              <div className={styles.outOfSliderRangeEnd} />
            </div>
          </div>
        </div>
      </div>
    );
  }
}

export default withDiscreetMode<Config<Props, InjectedProps>>(
  StakePoolsRanking
);
