// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import styles from './DelegationProgressNavigation.scss';
import DelegationProgressNavButton from './DelegationProgressNavButton';

const messages = defineMessages({
  center: {
    id: 'delegation.progress.navigation.center',
    defaultMessage: '!!!Delegation center',
    description:
      'Label for the "Delegation center" nav button in the delegation progress navigation.',
  },
  stakePools: {
    id: 'delegation.progress.navigation.stakePools',
    defaultMessage: '!!!Stake pools',
    description:
      'Label for the "Stake pools" nav button in the delegation progress navigation.',
  },
  rewards: {
    id: 'delegation.progress.navigation.rewards',
    defaultMessage: '!!!Rewards',
    description:
      'Label for the "Rewards" nav button in the delegation progress navigation.',
  },
  epochs: {
    id: 'delegation.progress.navigation.epochs',
    defaultMessage: '!!!Epochs',
    description:
      'Label for the "Epochs" nav button in the delegation progress navigation.',
  },
  info: {
    id: 'delegation.progress.navigation.info',
    defaultMessage: '!!!Info',
    description:
      'Label for the "Info" nav button in the delegation progress navigation.',
  },
});

type Props = {
  isActiveNavItem: Function,
  onNavItemClick: Function,
};

@observer
export default class DelegationProgressNavigation extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { isActiveNavItem, onNavItemClick } = this.props;
    const { intl } = this.context;
    return (
      <div className={styles.component}>
        <div className={styles.navItem}>
          <DelegationProgressNavButton
            className="center"
            label={intl.formatMessage(messages.center)}
            isActive={isActiveNavItem('center')}
            onClick={() => onNavItemClick('center')}
          />
        </div>
        <div className={styles.navItem}>
          <DelegationProgressNavButton
            className="stakePools"
            label={intl.formatMessage(messages.stakePools)}
            isActive={isActiveNavItem('stakePools')}
            onClick={() => onNavItemClick('stakePools')}
          />
        </div>
        <div className={styles.navItem}>
          <DelegationProgressNavButton
            className="rewards"
            label={intl.formatMessage(messages.rewards)}
            isActive={isActiveNavItem('rewards')}
            onClick={() => onNavItemClick('rewards')}
          />
        </div>
        <div className={styles.navItem}>
          <DelegationProgressNavButton
            className="epochs"
            label={intl.formatMessage(messages.epochs)}
            isActive={isActiveNavItem('epochs')}
            onClick={() => onNavItemClick('epochs')}
          />
        </div>
        <div className={styles.navItem}>
          <DelegationProgressNavButton
            className="info"
            label={intl.formatMessage(messages.info)}
            isActive={isActiveNavItem('info')}
            onClick={() => onNavItemClick('info')}
          />
        </div>
      </div>
    );
  }
}
