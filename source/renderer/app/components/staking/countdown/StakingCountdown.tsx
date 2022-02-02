import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import CountdownWidget from '../../widgets/CountdownWidget';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './StakingCountdown.scss' or it... Remove this comment to see the full error message
import styles from './StakingCountdown.scss';
import ButtonLink from '../../widgets/ButtonLink';

const messages = defineMessages({
  heading: {
    id: 'staking.delegationCountdown.heading',
    defaultMessage: '!!!Shelley upgrade',
    description: 'Headline for the Decentralisation notification.',
  },
  description: {
    id: 'staking.delegationCountdown.description',
    defaultMessage:
      '!!!Cardano will soon start transitioning from a federated to a decentralized system. The first step is the activation of the Shelley upgrade. Once the upgrade is complete, stake pools will start registering and users will be able to delegate their wallets. Two epochs (10 days) later, stake pools will begin producing blocks and users could start earning rewards from delegating their stakes. The first rewards, where due, will be distributed two more epochs later (10 days).',
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
  redirectToStakingInfo?: (...args: Array<any>) => any;
  startDateTime: string;
  onLearnMoreClick: (...args: Array<any>) => any;
};

@observer
class StakingCountdown extends Component<Props> {
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
    const showLearnMoreButton = false;
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
          {showLearnMoreButton && (
            <ButtonLink
              // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
              className={styles.learnMoreButton}
              onClick={onLearnMoreClick}
              skin={ButtonSkin}
              label={buttonLabel}
              linkProps={{
                className: styles.externalLinkIcon,
              }}
            />
          )}
        </div>
      </div>
    );
  }
}

export default StakingCountdown;
