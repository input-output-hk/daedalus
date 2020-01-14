// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import SVGInline from 'react-svg-inline';
import CountdownWidget from '../../widgets/CountdownWidget';
import styles from './StakingCountdown.scss';
import externalLinkIcon from '../../../assets/images/link-ic.inline.svg';

const messages = defineMessages({
  heading: {
    id: 'staking.delegationCountdown.heading',
    defaultMessage: '!!!Cardano decentralization',
    description: 'Headline for the Decentralisation notification.',
  },
  description: {
    id: 'staking.delegationCountdown.description',
    defaultMessage:
      '!!!Cardano will soon start its transition from a federated to a decentralized system. This will mark the start of stakeholders being able to earn rewards for participating in the running of the network. They will be able to participate directly in the process of staking or can delegate their stake to stake pools to earn rewards in ada.',
    description: 'Info for the Decentralisation notification.',
  },
  timeLeftDesc: {
    id: 'staking.delegationCountdown.timeLeftDesc',
    defaultMessage: '!!!Rewards begin in',
    description: 'Description for the Decentralisation notification.',
  },
  buttonLabel: {
    id: 'staking.delegationCountdown.buttonLabel',
    defaultMessage: '!!!Learn more',
    description: 'Button Label for the Decentralisation notification.',
  },
});

type Props = {
  redirectToStakingInfo?: Function,
  startDateTime: string,
  onLearnMoreClick: Function,
};

@observer
export default class StakingCountdown extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const {
      redirectToStakingInfo,
      startDateTime,
      onLearnMoreClick,
    } = this.props;
    const { intl } = this.context;
    const heading = intl.formatMessage(messages.heading);
    const description = intl.formatMessage(messages.description);
    const timeLeftDesc = intl.formatMessage(messages.timeLeftDesc);
    const buttonLabel = intl.formatMessage(messages.buttonLabel);

    return (
      <div className={styles.component}>
        <div className={styles.mainContent}>
          <div className={styles.heading}>{heading}</div>
          <div className={styles.description}>{description}</div>
          <div className={styles.timeLeftDesc}>{timeLeftDesc}</div>
          <CountdownWidget
            startDateTime={startDateTime}
            redirectOnEnd={redirectToStakingInfo}
          />
          <Button
            className={styles.learnMoreButton}
            label={
              <p>
                <SVGInline
                  svg={externalLinkIcon}
                  className={styles.externalLinkIcon}
                />
                {buttonLabel}
              </p>
            }
            skin={ButtonSkin}
            onClick={onLearnMoreClick}
          />
        </div>
      </div>
    );
  }
}
