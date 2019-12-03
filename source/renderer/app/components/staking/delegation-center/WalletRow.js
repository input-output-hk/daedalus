// @flow
import React, { Component, Fragment } from 'react';
import { observer } from 'mobx-react';
import {
  defineMessages,
  intlShape,
  FormattedMessage,
  FormattedHTMLMessage,
} from 'react-intl';
import SVGInline from 'react-svg-inline';
import isNil from 'lodash/isNil';
import Wallet from '../../../domains/Wallet';
import { getColorFromRange } from '../../../utils/colors';
import settingsIcon from '../../../assets/images/settings-ic.inline.svg';
import { SIMPLE_DECIMAL_PLACES_IN_ADA } from '../../../config/numbersConfig';
import DropdownMenu from './DropdownMenu';
import DonutRing from './DonutRing';
import styles from './WalletRow.scss';

export const DELEGATION_ACTIONS = {
  CHANGE_DELEGATION: 'changeDelegation',
  REMOVE_DELEGATION: 'removeDelegation',
  DELEGATE: 'delegate',
};

const messages = defineMessages({
  walletAmount: {
    id: 'staking.delegationCenter.walletAmount',
    defaultMessage: '!!!{amount} ADA',
    description:
      'Amount of each wallet for the Delegation center body section.',
  },
  inactiveStakePercentageActivate: {
    id: 'staking.delegationCenter.inactiveStakePercentageActivate',
    defaultMessage:
      '!!!<b>activate {inactiveStakePercentage}% of inactive stake</b>',
    description:
      'Inactive stake percentage of each wallet for the Delegation center body section.',
  },
  delegated: {
    id: 'staking.delegationCenter.delegated',
    defaultMessage: '!!!Delegated',
    description: 'Delegated label for the Delegation center body section.',
  },
  notDelegated: {
    id: 'staking.delegationCenter.notDelegated',
    defaultMessage: '!!!Not-delegated',
    description: 'Not-delegated label for the Delegation center body section.',
  },
  changeDelegation: {
    id: 'staking.delegationCenter.changeDelegation',
    defaultMessage: '!!!Change delegation',
    description:
      'Change delegation label for the Delegation center body section.',
  },
  removeDelegation: {
    id: 'staking.delegationCenter.removeDelegation',
    defaultMessage: '!!!Remove delegation',
    description:
      'Remove delegation label for the Delegation center body section.',
  },
  toStakePoolSlug: {
    id: 'staking.delegationCenter.toStakePoolSlug',
    defaultMessage: '!!!To <b>[{delegatedStakePoolSlug}]</b> stake pool',
    description:
      'Delegated stake pool slug for the Delegation center body section.',
  },
  delegate: {
    id: 'staking.delegationCenter.delegate',
    defaultMessage: '!!!Delegate',
    description: 'Delegate label for the Delegation center body section.',
  },
  yourStake: {
    id: 'staking.delegationCenter.yourStake',
    defaultMessage: '!!!your stake',
    description: 'Your stake label for the Delegation center body section.',
  },
});

type Props = {
  wallet: Wallet,
  numberOfStakePools: number,
  onDelegate: Function,
  onUndelegate: Function,
};

@observer
export default class WalletRow extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  onDelegate = () => {
    const { wallet, onDelegate } = this.props;
    onDelegate(wallet.id);
  };

  onUndelegate = () => {
    const { wallet, onUndelegate } = this.props;
    onUndelegate(wallet.id);
  };

  handleWalletAction = (value: string) => {
    if (value === DELEGATION_ACTIONS.CHANGE_DELEGATION) {
      // @TODO: Change delegation action
    } else if (value === DELEGATION_ACTIONS.REMOVE_DELEGATION) {
      this.onUndelegate();
    }
  };

  render() {
    const { intl } = this.context;
    const {
      wallet: {
        name,
        amount,
        inactiveStakePercentage,
        isDelegated,
        delegatedStakePool,
      },
      numberOfStakePools,
    } = this.props;

    const { ranking } = delegatedStakePool || {};

    const inactiveStakePercentageValue = inactiveStakePercentage || 0;
    const color =
      isDelegated && !isNil(ranking)
        ? getColorFromRange(ranking, numberOfStakePools)
        : 'transparent';

    const delegated = intl.formatMessage(messages.delegated);
    const notDelegated = intl.formatMessage(messages.notDelegated);
    const changeDelegation = intl.formatMessage(messages.changeDelegation);
    const removeDelegation = intl.formatMessage(messages.removeDelegation);
    const delegate = intl.formatMessage(messages.delegate);
    const yourStake = intl.formatMessage(messages.yourStake);

    const delegatedWalletActionOptions = [
      {
        label: changeDelegation,
        value: DELEGATION_ACTIONS.CHANGE_DELEGATION,
        className: styles.normalOption,
      },
      {
        label: removeDelegation,
        value: DELEGATION_ACTIONS.REMOVE_DELEGATION,
        className: styles.removeOption,
      },
    ];

    return (
      <div className={styles.component}>
        <div className={styles.left}>
          <div className={styles.title}>{name}</div>
          <div className={styles.description}>
            <FormattedMessage
              {...messages.walletAmount}
              values={{
                amount: amount.toFormat(SIMPLE_DECIMAL_PLACES_IN_ADA),
              }}
            />
            {inactiveStakePercentageValue > 0 && (
              <Fragment>
                <span className={styles.donutRing}>
                  <DonutRing
                    percentage={100 - inactiveStakePercentageValue}
                    sqSize={11}
                    strokeWidth={3}
                  />
                </span>
                <FormattedHTMLMessage
                  {...messages.inactiveStakePercentageActivate}
                  values={{
                    inactiveStakePercentage: inactiveStakePercentageValue,
                  }}
                />
              </Fragment>
            )}
          </div>
        </div>
        <div className={styles.right}>
          <div>
            <div className={styles.status}>
              <span>{isDelegated ? delegated : notDelegated}</span>
              {isDelegated && (
                <DropdownMenu
                  label={
                    <SVGInline svg={settingsIcon} className={styles.gearIcon} />
                  }
                  menuItems={delegatedWalletActionOptions}
                  onMenuItemClick={this.handleWalletAction}
                />
              )}
            </div>
            <div className={styles.action}>
              {isDelegated && delegatedStakePool ? (
                <FormattedHTMLMessage
                  {...messages.toStakePoolSlug}
                  values={{
                    delegatedStakePoolSlug: delegatedStakePool.slug,
                  }}
                />
              ) : (
                <span>
                  <span
                    className={styles.actionLink}
                    role="presentation"
                    onClick={this.onDelegate}
                  >
                    {delegate}
                  </span>
                  {` ${yourStake}`}
                </span>
              )}
            </div>
          </div>
          <div>
            <div
              className={styles.stakePoolRankingIndicator}
              style={{ background: color }}
            />
          </div>
        </div>
      </div>
    );
  }
}
