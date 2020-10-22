// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import classnames from 'classnames';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import { TooltipSkin } from 'react-polymorph/lib/skins/simple/TooltipSkin';
import { Tooltip } from 'react-polymorph/lib/components/Tooltip';
import { shortNumber, generateThousands } from '../../../utils/formatters';
import {
  getFilteredWallets,
  getAllAmounts,
} from '../../../utils/walletsForStakePoolsRanking';
import Wallet from '../../../domains/Wallet';
import {
  RANKING_SLIDER_RATIO,
  MIN_DELEGATION_FUNDS_LOG,
  MIN_DELEGATION_FUNDS,
  MAX_DELEGATION_FUNDS_LOG,
  MAX_DELEGATION_FUNDS,
  INITIAL_DELEGATION_FUNDS_LOG,
  INITIAL_DELEGATION_FUNDS,
  OUT_OF_RANGE_MAX_DELEGATION_FUNDS,
  ALL_WALLETS_SELECTION_ID,
  IS_RANKING_DATA_AVAILABLE,
} from '../../../config/stakingConfig';
import WalletsDropdown from '../../widgets/forms/WalletsDropdown';
import ButtonLink from '../../widgets/ButtonLink';
import Slider from '../../widgets/Slider';
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

const walletSelectorLanguageMap = {
  'en-US': 'enSelector',
  'ja-JP': 'jaSelector',
};

type Props = {
  wallets: Array<Wallet>,
  currentLocale: string,
  onOpenExternalLink: Function,
  updateDelegatingStake: Function,
  rankStakePools: Function,
  selectedDelegationWalletId?: ?string,
  stake?: ?number,
  isLoading: boolean,
  isRanking: boolean,
  numberOfStakePools: number,
  getStakePoolById: Function,
};

type State = {
  sliderValue: number,
  amountValue: number,
};

@observer
export default class StakePoolsRanking extends Component<Props, State> {
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
    amountValue: INITIAL_DELEGATION_FUNDS,
  };

  componentDidMount() {
    const { stake } = this.props;
    if (stake) {
      this.setState({
        sliderValue: Math.round(Math.log(stake) * RANKING_SLIDER_RATIO),
        amountValue: stake,
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
    } = this.props;
    const selectedWallet = wallets.find(
      (wallet) => wallet.id === selectedWalletId
    );

    if (
      selectedWalletId === selectedDelegationWalletId ||
      (selectedWalletId !== ALL_WALLETS_SELECTION_ID && !selectedWallet)
    ) {
      return;
    }

    let amountValue = 0;
    let sliderValue = 0;

    if (selectedWalletId === ALL_WALLETS_SELECTION_ID) {
      amountValue = Math.min(
        getAllAmounts(wallets).toNumber(),
        MAX_DELEGATION_FUNDS
      );
    } else if (selectedWallet) {
      amountValue = selectedWallet.amount.toNumber();
    }
    amountValue = Math.max(amountValue, MIN_DELEGATION_FUNDS);
    sliderValue = Math.round(Math.log(amountValue) * RANKING_SLIDER_RATIO);
    this.setState({ sliderValue, amountValue });
    updateDelegatingStake(selectedWalletId, amountValue);
    rankStakePools();
  };

  onSliderChange = (sliderValue: number) => {
    const { updateDelegatingStake } = this.props;
    let amountValue = null;
    if (
      sliderValue ===
      Math.round(MIN_DELEGATION_FUNDS_LOG * RANKING_SLIDER_RATIO)
    ) {
      amountValue = MIN_DELEGATION_FUNDS;
    } else if (
      sliderValue ===
      Math.round(MAX_DELEGATION_FUNDS_LOG * RANKING_SLIDER_RATIO)
    ) {
      amountValue = MAX_DELEGATION_FUNDS;
    } else {
      amountValue = generateThousands(
        Math.exp(sliderValue / RANKING_SLIDER_RATIO)
      );
    }
    this.setState({ sliderValue, amountValue });
    updateDelegatingStake(null, amountValue);
  };

  generateInfo = () => {
    const { intl } = this.context;
    const { wallets, currentLocale, selectedDelegationWalletId } = this.props;
    const allWalletsItem = {
      id: ALL_WALLETS_SELECTION_ID,
      name: intl.formatMessage(messages.rankingAllWallets),
      amount: getAllAmounts(wallets),
    };
    const filteredWallets = getFilteredWallets(wallets);
    const walletSelectorWallets = [allWalletsItem, ...filteredWallets];
    const walletSelectorClasses = classnames([
      styles.walletSelector,
      walletSelectorLanguageMap[currentLocale],
      selectedDelegationWalletId === null ? 'noValueSelected' : null,
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
    } = this.props;
    const { sliderValue, amountValue } = this.state;
    const learnMoreButtonClasses = classnames(['flat', styles.actionLearnMore]);
    const {
      walletSelectorWallets,
      walletSelectorClasses,
      walletSelectionStart,
      walletSelectionEnd,
      learnMoreUrl,
    } = this.generateInfo();

    if (!IS_RANKING_DATA_AVAILABLE) {
      return null;
    }

    return (
      <div className={styles.component}>
        <div className={styles.upper}>
          <div className={styles.selectWallet}>
            <div className={styles.row}>
              <div className={styles.col}>
                <FormattedHTMLMessage {...messages.rankingDescription} />
              </div>
            </div>
            {getFilteredWallets(wallets).length > 0 ? (
              <div className={styles.row}>
                <div className={styles.col}>{walletSelectionStart}</div>
                <div className={styles.col}>
                  <WalletsDropdown
                    className={walletSelectorClasses}
                    placeholder={intl.formatMessage(
                      messages.rankingSelectWallet
                    )}
                    wallets={walletSelectorWallets}
                    onChange={this.onSelectedWalletChange}
                    disabled={isLoading || isRanking}
                    value={selectedDelegationWalletId}
                    selectionRenderer={(option) => (
                      <button
                        className="customValue"
                        onClick={() => {
                          const selectionInput = document.querySelector(
                            '.StakePoolsRanking_walletSelector .SimpleInput_input'
                          );
                          if (selectionInput) {
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
                max={Math.round(
                  MAX_DELEGATION_FUNDS_LOG * RANKING_SLIDER_RATIO
                )}
                maxDisplayValue={MAX_DELEGATION_FUNDS}
                value={sliderValue}
                displayValue={amountValue}
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
                <Tooltip
                  skin={TooltipSkin}
                  tip={intl.formatMessage(messages.rankingExtraTooltip)}
                >
                  {shortNumber(OUT_OF_RANGE_MAX_DELEGATION_FUNDS)}
                </Tooltip>
              </div>
              <div className={styles.outOfSliderRangeEnd} />
            </div>
          </div>
        </div>
      </div>
    );
  }
}
