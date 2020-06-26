// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import BigNumber from 'bignumber.js';
import classnames from 'classnames';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import { Select } from 'react-polymorph/lib/components/Select';
import { SelectSkin } from 'react-polymorph/lib/skins/simple/SelectSkin';
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
    defaultMessage: '!!!Or choose',
    description: 'Select wallet description before dropdown.',
  },
  actionLearnMore: {
    id: 'staking.stakePools.learnMore',
    defaultMessage: '!!!Learn more',
    description: 'Learn more action of ranking panel.',
  },
});

type Props = {
  wallets: Array<Wallet>,
  onLearnMore: Function,
  onRank: Function,
  isLoading: boolean,
};

type State = {
  selectedWalletId: string,
  sliderValue: number,
};

const OUT_OF_RANGE_MAX_AMOUNT = new BigNumber('31000000000');
const MIN_AMOUNT = new BigNumber('10');

@observer
export default class StakePoolsRanking extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    selectedWalletId: '-1',
    sliderValue: MIN_AMOUNT.toNumber(),
  };

  getAllAvailableAmount = () =>
    this.props.wallets
      .map((w: Wallet) => w.availableAmount)
      .reduce(
        (acc: BigNumber, cur: BigNumber) => acc.plus(cur),
        new BigNumber(0)
      );

  onSelectedWalletChange = (selectedWalletId: string) => {
    const { wallets } = this.props;
    const selectedWallet = wallets.find(
      wallet => wallet.id === selectedWalletId
    );
    let sliderValue = null;

    if (selectedWalletId === '-1') {
      sliderValue = MIN_AMOUNT.toNumber();
    } else if (selectedWalletId === '0') {
      sliderValue = this.getAllAvailableAmount().toNumber();
    } else {
      sliderValue = selectedWallet.availableAmount.toNumber();
    }
    this.setState({ selectedWalletId, sliderValue });
  };

  onSliderChange = (sliderValue: number) => {
    this.props.onRank(sliderValue);
    this.setState({ sliderValue, selectedWalletId: '-1' });
  };

  generateInfo = () => {
    const { intl } = this.context;
    const { wallets } = this.props;
    const { selectedWalletId, sliderValue } = this.state;
    const selectWalletItem = {
      label: intl.formatMessage(messages.rankingSelectWallet),
      value: '-1',
    };
    const allWalletsItem = {
      label: intl.formatMessage(messages.rankingAllWallets),
      value: '0',
    };
    const walletSelectorOptions = [
      selectWalletItem,
      allWalletsItem,
      ...wallets.map((w: Wallet) => ({
        label: w.name,
        value: w.id,
      })),
    ];
    const selectionRendererClasses = classnames([
      'custom-value',
      selectedWalletId === '-1' ? 'no-select' : null,
    ]);

    let walletSelectionStart = null;
    let walletSelectionEnd = null;

    if (selectedWalletId === '-1') {
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
      selectionRendererClasses,
      walletSelectionStart,
      walletSelectionEnd,
      sliderValue,
    };
  };

  render() {
    const { intl } = this.context;
    const { onLearnMore, isLoading } = this.props;
    const rankingDescription = intl.formatMessage(messages.rankingDescription);
    const learnMoreButtonClasses = classnames(['flat', styles.actionLearnMore]);
    const allAvailableAmount: BigNumber = this.getAllAvailableAmount();
    const {
      selectedWalletId,
      walletSelectorOptions,
      selectionRendererClasses,
      walletSelectionStart,
      walletSelectionEnd,
      sliderValue,
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
                  className={styles.walletSelector}
                  options={walletSelectorOptions}
                  value={selectedWalletId}
                  onChange={this.onSelectedWalletChange}
                  skin={SelectSkin}
                  selectionRenderer={option => (
                    <div className={selectionRendererClasses}>
                      {option.label}
                    </div>
                  )}
                  optionHeight={50}
                />
              </div>
              <div className={styles.col}>{walletSelectionEnd}</div>
            </div>
          </div>
          <ButtonLink
            className={learnMoreButtonClasses}
            onClick={onLearnMore}
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
                max={allAvailableAmount.toNumber()}
                value={sliderValue}
                onChange={this.onSliderChange}
                disabled={isLoading}
              />
            </div>
            <div className={styles.col}>
              <div className={styles.outOfRangeMaxAmount}>
                {shortNumber(OUT_OF_RANGE_MAX_AMOUNT)}
              </div>
              <div className={styles.outOfSliderRangeEnd} />
            </div>
          </div>
        </div>
      </div>
    );
  }
}
