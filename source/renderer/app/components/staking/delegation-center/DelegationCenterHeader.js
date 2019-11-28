// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import styles from './DelegationCenterHeader.scss';
import CountdownWidget from '../../widgets/CountdownWidget';

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

type Props = {
  redirectToStakingInfo?: Function,
  startDateTime: string,
};

@observer
export default class DelegationCenterHeader extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { redirectToStakingInfo, startDateTime } = this.props;
    const heading = intl.formatMessage(messages.heading);
    const description = intl.formatMessage(messages.description);

    return (
      <div className={styles.component}>
        <div className={styles.mainContent}>
          <div className={styles.heading}>{heading}</div>
          <CountdownWidget
            redirectToStakingInfo={redirectToStakingInfo}
            startDateTime={startDateTime}
          />
          <div className={styles.description}>{description}</div>
        </div>
      </div>
    );
  }
}
