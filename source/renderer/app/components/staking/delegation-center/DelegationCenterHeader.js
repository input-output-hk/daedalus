// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import styles from './DelegationCenterHeader.scss';

const messages = defineMessages({
  heading: {
    id: 'staking.delegationCenter.heading',
    defaultMessage: '!!!New Cardano epoch starts in',
    description: 'Headline for the Delegation center.',
  },
  description: {
    id: 'staking.delegationCenter.description',
    defaultMessage:
      'Changes to delegation preferences will take effect from the next epoch.',
    description: 'Delegation description for the Delegation center.',
  },
});

@observer
export default class DelegationCenterHeader extends Component {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const heading = intl.formatMessage(messages.heading);
    const description = intl.formatMessage(
      messages.description
    );

    return (
      <div className={styles.component}>
        <div className={styles.mainContent}>
          <div className={styles.heading}>{heading}</div>
          <div className={styles.description}>
            {description}
          </div>
        </div>
      </div>
    );
  }
}
