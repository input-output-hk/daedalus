import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import ButtonLink from '../../widgets/ButtonLink';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './StakingInfoCountdown.scss' o... Remove this comment to see the full error message
import styles from './StakingInfoCountdown.scss';
import FullyDecentralizedEffect from '../../widgets/FullyDecentralizedEffect';
import CountdownWidget from '../../widgets/CountdownWidget';

const messages = defineMessages({
  heading: {
    id: 'staking.infoCountdown.heading',
    defaultMessage: '!!!Alonzo upgrade',
    description: 'Headline for the "Staking Info" page screen.',
  },
  descriptionBefore: {
    id: 'staking.infoCountdown.description.before',
    defaultMessage:
      '!!!The ‘Alonzo’ protocol upgrade will bring highly-anticipated new smart contract capabilities to Cardano, by integrating Plutus scripts onto the blockchain. This important milestone will open up a whole new world of smart contracts, DeFi capabilities, and dApp development on Cardano.',
    description: 'Info description for the "Staking Info" page screen.',
  },
  descriptionAfter: {
    id: 'staking.infoCountdown.description.after',
    defaultMessage:
      '!!!The ‘Alonzo’ protocol upgrade is now live on Cardano, enabling highly-anticipated new smart contract capabilities, by integrating Plutus scripts onto the blockchain. This important milestone opens up a whole new world of smart contracts, DeFi capabilities, and dApp development on Cardano.',
    description: 'Info description for the "Staking Info" page screen.',
  },
  countdownTitle: {
    id: 'staking.infoCountdown.countdownTitle',
    defaultMessage: '!!!Alonzo upgrade in',
    description: 'Countdown Title for the "Staking Info" page screen.',
  },
  buttonLabel: {
    id: 'staking.infoCountdown.buttonLabel',
    defaultMessage: '!!!Learn more',
    description: 'Button Label for the "Staking Info" page screen.',
  },
  learnMoreLinkUrl: {
    id: 'staking.infoCountdown.learnMore.linkUrl',
    defaultMessage:
      '!!!https://iohk.io/en/blog/posts/2021/04/08/smart-contracts-%E2%80%93-here-we-come/',
    description: '"Learn more" link URL in the "Staking Info" screen.',
  },
});
type Props = {
  startDateTime: string;
  isAlonzoActivated: boolean;
  stakingInfoWasOpen: boolean;
  onSetStakingInfoWasOpen: (...args: Array<any>) => any;
  onLearnMoreClick: (...args: Array<any>) => any;
};

@observer
class StakingInfoCountdown extends Component<Props> {
  static defaultProps = {
    percentage: 0,
  };
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  componentDidMount() {
    this.checkIfShouldSetStakingInfoWasOpen();
  }

  componentDidUpdate() {
    this.checkIfShouldSetStakingInfoWasOpen();
  }

  checkIfShouldSetStakingInfoWasOpen = () => {
    const {
      onSetStakingInfoWasOpen,
      isAlonzoActivated,
      stakingInfoWasOpen,
    } = this.props;

    if (isAlonzoActivated && !stakingInfoWasOpen) {
      onSetStakingInfoWasOpen();
    }
  };

  render() {
    const { intl } = this.context;
    const { startDateTime, onLearnMoreClick, isAlonzoActivated } = this.props;
    const heading = intl.formatMessage(messages.heading);
    const description = isAlonzoActivated
      ? messages.descriptionAfter
      : messages.descriptionBefore;
    const buttonLabel = intl.formatMessage(messages.buttonLabel);
    return (
      <div className={styles.component}>
        <div className={styles.mainContent}>
          <div className={styles.content}>
            <div className={styles.heading}>{heading}</div>
            <div className={styles.description}>
              {intl.formatMessage(description)}
            </div>
            <div className={styles.countdownTitle}>
              {intl.formatMessage(messages.countdownTitle)}
            </div>
            <CountdownWidget
              startDateTime={isAlonzoActivated ? '0' : startDateTime}
              format="DD-HH-mm-ss"
            />
            <ButtonLink
              // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
              className={styles.learnMoreButton}
              onClick={() =>
                onLearnMoreClick(intl.formatMessage(messages.learnMoreLinkUrl))
              }
              skin={ButtonSkin}
              label={buttonLabel}
              linkProps={{
                className: styles.externalLinkIcon,
              }}
            />
          </div>
          <FullyDecentralizedEffect isActive={isAlonzoActivated} />
        </div>
      </div>
    );
  }
}

export default StakingInfoCountdown;
