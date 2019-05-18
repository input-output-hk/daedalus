// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import styles from './DelegationProgressInfo.scss';

const messages = defineMessages({
  heading: {
    id: 'paper.delegation.progress.notification.heading',
    defaultMessage: '!!!Cardano is transitioning to a decentralized system',
    description: 'Headline for the Decentralization progress notification.',
  },
  infoDesc: {
    id: 'paper.delegation.progress.notification.infoDesc',
    defaultMessage:
      '!!!Cardano is transitioning from a federated to a decentralized system. During this transition, some new blocks will be produced by nodes operated in a federated way and some by nodes run by ada stakeholders. All rewards will be distributed to ada stakeholders and none to operators of federated nodes.',
    description:
      'Info description for the Decentralization progress notification.',
  },
  infoPercentageStart: {
    id: 'paper.delegation.progress.notification.infoPercentageStart',
    defaultMessage: '!!!Currently,',
    description:
      'Percentage info start word for the Decentralization progress notification.',
  },
  infoPercentageDesc: {
    id: 'paper.delegation.progress.notification.infoPercentageDesc',
    defaultMessage:
      '!!!of the system is decentralized and operated by stake pools.',
    description:
      'Percentage info description for the Decentralization progress notification.',
  },
  buttonLabel: {
    id: 'paper.delegation.progress.notification.buttonLabel',
    defaultMessage: '!!!Learn more',
    description: 'Button Label for the Decentralization progress notification.',
  },
});

type Props = { percentage: number };

@observer
export default class DelegationProgressInfo extends Component<Props> {
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
    const infoDesc = intl.formatMessage(messages.infoDesc);
    const infoPercentageStart = intl.formatMessage(
      messages.infoPercentageStart
    );
    const infoPercentageDesc = intl.formatMessage(messages.infoPercentageDesc);
    const progressLabelLeftPosition = Math.max(percentage / 2, 5);
    const buttonLabel = intl.formatMessage(messages.buttonLabel);

    return (
      <div className={styles.component}>
        <div className={styles.heading}>{heading}</div>
        <div className={styles.info}>{infoDesc}</div>
        <div className={styles.info}>
          {infoPercentageStart} {percentage}% {infoPercentageDesc}
        </div>
        <div className={styles.progressBar}>
          <div className={styles.progressBarContainer}>
            <div
              className={styles.progress}
              style={{ width: `${percentage}%` }}
            />
            <div
              className={styles.progressLabel}
              style={{ left: `calc(${progressLabelLeftPosition}% - 10px)` }}
            >
              {percentage}%
            </div>
          </div>
        </div>
        <Button label={buttonLabel} skin={ButtonSkin} />
      </div>
    );
  }
}
