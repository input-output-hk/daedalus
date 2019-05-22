// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape, FormattedMessage } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import styles from './StakingInfo.scss';

const messages = defineMessages({
  heading: {
    id: 'staking.info.heading',
    defaultMessage: '!!!Cardano is transitioning to a decentralized system',
    description: 'Headline for the Decentralization progress notification.',
  },
  description: {
    id: 'staking.info.description',
    defaultMessage:
      '!!!Cardano is transitioning from a federated to a decentralized system. During this transition, some new blocks will be produced by nodes operated in a federated way and some by nodes run by ada stakeholders. All rewards will be distributed to ada stakeholders and none to operators of federated nodes.',
    description:
      'Info description for the Decentralization progress notification.',
  },
  percentage: {
    id: 'staking.info.percentage',
    defaultMessage:
      '!!!Currently, {percentage}% of the system is decentralized and operated by stake pools.',
    description:
      'Percentage info description for the Decentralization progress notification.',
  },
  buttonLabel: {
    id: 'staking.info.buttonLabel',
    defaultMessage: '!!!Learn more',
    description: 'Button Label for the Decentralization progress notification.',
  },
});

type Props = { percentage: number };

@observer
export default class DelegationInfo extends Component<Props> {
  static defaultProps = {
    percentage: 0,
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { percentage } = this.props;
    const heading = intl.formatMessage(messages.heading);
    const description = intl.formatMessage(messages.description);
    const buttonLabel = intl.formatMessage(messages.buttonLabel);
    const progressLabelLeftPosition =
      percentage / 2 >= 5 ? 50 : 500 / percentage;
    const progressBackLabelLeftPosition = Math.max(percentage / 2, 5);

    return (
      <div className={styles.component}>
        <div className={styles.mainContent}>
          <div className={styles.heading}>{heading}</div>
          <div className={styles.description}>{description}</div>
          <div className={styles.description}>
            <FormattedMessage
              {...messages.percentage}
              values={{ percentage }}
            />
          </div>
          <div className={styles.progressBar}>
            <div className={styles.progressBarContainer}>
              <div
                className={styles.progress}
                style={{ width: `${percentage}%` }}
              >
                <div
                  className={styles.progressLabel}
                  style={{ left: `${progressLabelLeftPosition}%` }}
                >
                  {percentage}%
                </div>
              </div>
              <div
                className={styles.progressBackLabel}
                style={{ left: `${progressBackLabelLeftPosition}%` }}
              >
                {percentage}%
              </div>
            </div>
          </div>
          <Button label={buttonLabel} skin={ButtonSkin} />
        </div>
      </div>
    );
  }
}
