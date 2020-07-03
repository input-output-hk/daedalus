// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import BigNumber from 'bignumber.js';
import classnames from 'classnames';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import { Select } from 'react-polymorph/lib/components/Select';
import { SelectSkin } from 'react-polymorph/lib/skins/simple/SelectSkin';
import { TooltipSkin } from 'react-polymorph/lib/skins/simple/TooltipSkin';
import { Tooltip } from 'react-polymorph/lib/components/Tooltip';
import { shortNumber } from '../../../utils/formatters';
import Wallet from '../../../domains/Wallet';
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
      '!!!Use the slider to rank the stake pools based on the amount you intend to delegate.',
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
  onRank: Function,
  isLoading: boolean,
};

type State = {
  selectedWalletId: ?string,
  sliderValue: number,
};

const OUT_OF_RANGE_MAX_AMOUNT = new BigNumber('11000000000');
const MIN_AMOUNT = new BigNumber('10');
const MAX_AMOUNT = new BigNumber('44000000');

@observer
export default class StakePoolsRanking extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  static defaultProps = {
    wallets: [],
  };

  state = {
    selectedWalletId: null,
    sliderValue: MIN_AMOUNT.toNumber(),
  };

  componentDidMount() {
    this.onSelectedWalletChange('0');
  }

  getAllAvailableAmount = () => {
    const { wallets } = this.props;

    if (wallets.length) {
      return wallets
        .map((w: Wallet) => w.availableAmount)
        .reduce(
          (acc: BigNumber, cur: BigNumber) => acc.plus(cur),
          new BigNumber(0)
        );
    }

    return MIN_AMOUNT;
  };

  onSelectedWalletChange = (selectedWalletId: string) => {
    const { wallets } = this.props;
    const selectedWallet = wallets.find(
      wallet => wallet.id === selectedWalletId
    );
    let sliderValue = null;

    if (selectedWalletId === '0') {
      sliderValue = Math.min(
        Math.floor(this.getAllAvailableAmount().toNumber()),
        MAX_AMOUNT.toNumber()
      );
    } else if (selectedWallet) {
      sliderValue = Math.floor(selectedWallet.availableAmount.toNumber());
    } else {
      sliderValue = MIN_AMOUNT.toNumber();
    }

    this.setState({
      selectedWalletId,
      sliderValue: Math.max(sliderValue, MIN_AMOUNT.toNumber()),
    });
  };

  onSliderChange = (sliderValue: number) => {
    this.props.onRank(sliderValue);
    this.setState({ sliderValue, selectedWalletId: null });
  };

  generateInfo = () => {
    const { intl } = this.context;
    const { wallets, currentLocale } = this.props;
    const { selectedWalletId, sliderValue } = this.state;
    const allWalletsItem = {
      label: intl.formatMessage(messages.rankingAllWallets),
      value: '0',
    };
    const walletSelectorOptions = [
      allWalletsItem,
      ...wallets.map((w: Wallet) => ({
        label: w.name,
        value: w.id,
      })),
    ];
    const walletSelectorClasses = classnames([
      styles.walletSelector,
      walletSelectorLanguageMap[currentLocale],
      selectedWalletId === null ? 'noValueSelected' : null,
    ]);
    const learnMoreUrl = intl.formatMessage(messages.rankingLearnMoreUrl);

    let walletSelectionStart = null;
    let walletSelectionEnd = null;

    if (selectedWalletId === null) {
      walletSelectionStart = intl.formatMessage(
        messages.rankingSelectWalletStart
      );
      walletSelectionEnd = intl.formatMessage(messages.rankingSelectWalletEnd);
    } else if (selectedWalletId === '0') {
      walletSelectionStart = intl.formatMessage(
        messages.rankingAllWalletsStart
      );
      walletSelectionEnd = intl.formatMessage(messages.rankingAllWalletsEnd);
    } else {
      walletSelectionStart = intl.formatMessage(messages.rankingOneWalletStart);
      walletSelectionEnd = intl.formatMessage(messages.rankingOneWalletEnd);
    }

    return {
      selectedWalletId,
      walletSelectorOptions,
      walletSelectorClasses,
      walletSelectionStart,
      walletSelectionEnd,
      sliderValue,
      learnMoreUrl,
    };
  };

  render() {
    const { intl } = this.context;
    const { onOpenExternalLink, isLoading } = this.props;
    const rankingDescription = intl.formatMessage(messages.rankingDescription);
    const learnMoreButtonClasses = classnames(['flat', styles.actionLearnMore]);
    const {
      selectedWalletId,
      walletSelectorOptions,
      walletSelectorClasses,
      walletSelectionStart,
      walletSelectionEnd,
      sliderValue,
      learnMoreUrl,
    } = this.generateInfo();

    return (
      <div className={styles.component}>
        <div className={styles.upper}>
          <div className={styles.selectWallet}>
            <div className={styles.row}>
              <div className={styles.col}>{rankingDescription}</div>
            </div>
            <div className={styles.row}>
              <div className={styles.col}>{walletSelectionStart}</div>
              <div className={styles.col}>
                <Select
                  className={walletSelectorClasses}
                  options={walletSelectorOptions}
                  value={selectedWalletId}
                  placeholder={intl.formatMessage(messages.rankingSelectWallet)}
                  onChange={this.onSelectedWalletChange}
                  skin={SelectSkin}
                  selectionRenderer={option => (
                    <div className="customValue">{option.label}</div>
                  )}
                  optionHeight={50}
                />
              </div>
              <div className={styles.col}>{walletSelectionEnd}</div>
            </div>
          </div>
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
        </div>
        <div className={styles.lower}>
          <div className={styles.row}>
            <div className={styles.col}>
              <div className={styles.outOfSliderRangeStart} />
            </div>
            <div className={styles.slider}>
              <Slider
                min={MIN_AMOUNT.toNumber()}
                max={MAX_AMOUNT.toNumber()}
                value={sliderValue}
                onChange={this.onSliderChange}
                disabled={isLoading}
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
                  {shortNumber(OUT_OF_RANGE_MAX_AMOUNT)}
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
