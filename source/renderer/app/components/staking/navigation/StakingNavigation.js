// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import styles from './StakingNavigation.scss';
// import StakingNavButton from './StakingNavButton';
// import StakingNavDropdown from './StakingNavDropdown';
// import summaryIcon from '../../../assets/images/staking-nav/summary-ic.inline.svg';
// import sendIcon from '../../../assets/images/staking-nav/send-ic.inline.svg';
// import receiveIcon from '../../../assets/images/staking-nav/receive-ic.inline.svg';
// import transactionsIcon from '../../../assets/images/staking-nav/transactions-ic.inline.svg';
// import settingsIcon from '../../../assets/images/staking-nav/staking-settings-2-ic.inline.svg';

const messages = defineMessages({
  summary: {
    id: 'staking.navigation.summary',
    defaultMessage: '!!!Summary',
    description:
      'Label for the "Summary" nav button in the staking navigation.',
  },
  send: {
    id: 'staking.navigation.send',
    defaultMessage: '!!!Send',
    description: 'Label for the "Send" nav button in the staking navigation.',
  },
  receive: {
    id: 'staking.navigation.receive',
    defaultMessage: '!!!Receive',
    description:
      'Label for the "Receive" nav button in the staking navigation.',
  },
  transactions: {
    id: 'staking.navigation.transactions',
    defaultMessage: '!!!Transactions',
    description:
      'Label for the "Transactions" nav button in the staking navigation.',
  },
  settings: {
    id: 'staking.navigation.settings',
    defaultMessage: '!!!Settings',
    description:
      'Label for the "Settings" nav button in the staking navigation.',
  },
  utxo: {
    id: 'staking.navigation.utxo',
    defaultMessage: '!!!Staking UTXO distribution',
    description:
      'Label for the "Staking UTXO distribution" nav button in the staking navigation.',
  },
  more: {
    id: 'staking.navigation.more',
    defaultMessage: '!!!More',
    description: 'Label for the "More" nav button in the staking navigation.',
  },
});

type Props = {
  activeItem: string,
  isActiveNavItem: Function,
  onNavItemClick: Function,
};

@observer
export default class StakingNavigation extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { isActiveNavItem, onNavItemClick, activeItem } = this.props;
    const { intl } = this.context;
    return (
      <div className={styles.component}>
        <div className={styles.navItem}>
          <StakingNavButton
            className="summary"
            label={intl.formatMessage(messages.summary)}
            icon={summaryIcon}
            isActive={isActiveNavItem('summary')}
            onClick={() => onNavItemClick('summary')}
          />
        </div>

        <div className={styles.navItem}>
          <StakingNavButton
            className="send"
            label={intl.formatMessage(messages.send)}
            icon={sendIcon}
            isActive={isActiveNavItem('send')}
            onClick={() => onNavItemClick('send')}
          />
        </div>

        <div className={styles.navItem}>
          <StakingNavButton
            className="receive"
            label={intl.formatMessage(messages.receive)}
            icon={receiveIcon}
            isActive={isActiveNavItem('receive')}
            onClick={() => onNavItemClick('receive')}
          />
        </div>

        <div className={styles.navItem}>
          <StakingNavButton
            label={intl.formatMessage(messages.transactions)}
            icon={transactionsIcon}
            isActive={isActiveNavItem('transactions')}
            onClick={() => onNavItemClick('transactions')}
          />
        </div>

        <div className={styles.navItem}>
          <StakingNavDropdown
            label={intl.formatMessage(messages.more)}
            icon={settingsIcon}
            isActive={isActiveNavItem('settings') || isActiveNavItem('utxo')}
            onChange={item => onNavItemClick(item)}
            activeItem={activeItem}
            options={[
              {
                label: intl.formatMessage(messages.settings),
                value: 'settings',
              },
              {
                label: intl.formatMessage(messages.utxo),
                value: 'utxo',
              },
            ]}
          />
        </div>
      </div>
    );
  }
}
