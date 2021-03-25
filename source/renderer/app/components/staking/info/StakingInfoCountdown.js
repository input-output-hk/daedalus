// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import ButtonLink from '../../widgets/ButtonLink';
import styles from './StakingInfoCountdown.scss';
import FullyDecentralizedEffect from '../../widgets/FullyDecentralizedEffect';
import CountdownWidget from '../../widgets/CountdownWidget';
import { DECENTRALIZATED_ANIMATION_DURATION } from '../../../config/stakingConfig';
import type { NextEpoch } from '../../../api/network/types';

const messages = defineMessages({
  headingBefore: {
    id: 'staking.infoCountdown.before.heading',
    defaultMessage: '!!!Fully decentralized block production',
    description: 'Headline for the Decentralization progress notification.',
  },
  descriptionBefore: {
    id: 'staking.infoCountdown.before.description',
    defaultMessage:
      '!!!<p>Cardano is fast approaching full decentralization for block production. Soon, all blocks will be produced by Cardano’s network of stake pools. </p> <p>Currently, stake pools are producing {percentageDecentralized}% of blocks, while federated nodes are just producing {percentageFederated}%.</p> <p>At the boundary of Cardano epoch #{epochNumber}, stake pools will start producing 100% of blocks and full block decentralization will be achieved.</p>',
    description:
      'Info description for the Decentralization progress notification.',
  },
  headingAfter: {
    id: 'staking.infoCountdown.after.heading',
    defaultMessage: '!!!Cardano is transitioning into a decentralized system',
    description: 'Headline for the Decentralization progress notification.',
  },
  descriptionAfter: {
    id: 'staking.infoCountdown.after.description',
    defaultMessage:
      '!!!Cardano is transitioning from a federated system operated by its creators to a decentralized system operated by a community of stake pool operators. During this transition, blocks will be produced both by the federated nodes and by stake pools. The percentage of blocks produced by stake pools will increase every epoch until block production in the Cardano network becomes fully decentralized.',
    description:
      'Info description for the Decentralization progress notification.',
  },
  countdownTitle: {
    id: 'staking.infoCountdown.countdownTitle',
    defaultMessage: '!!!Fully decentralized block production in',
    description:
      'Countdown Title for the Decentralization progress notification.',
  },
  buttonLabel: {
    id: 'staking.infoCountdown.buttonLabel',
    defaultMessage: '!!!Learn more',
    description: 'Button Label for the Decentralization progress notification.',
  },
  learnMoreLinkUrl: {
    id: 'staking.infoCountdown.learnMore.linkUrl',
    defaultMessage: '!!!https://iohk.zendesk.com/hc',
    description: '"Learn more" link URL in the staking info page',
  },
});

type Props = {
  percentage: number,
  onLearnMoreClick: Function,
  epoch: NextEpoch,
  onSetStakingInfoWasOpen: Function,
  isAnimating: boolean,
  isFullyDecentralized: boolean,
  onStartStakingInfoAnimation: Function,
  onStopStakingInfoAnimation: Function,
};

type State = {
  wasAnimated: boolean,
};

@observer
export default class StakingInfoCountdown extends Component<Props, State> {
  static defaultProps = {
    percentage: 0,
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    wasAnimated: false,
  };

  componentDidMount() {
    this.checkIfShouldAnimate();
  }

  componentDidUpdate() {
    this.checkIfShouldAnimate();
  }

  componentWillUnmount() {
    this.props.onStopStakingInfoAnimation();
  }

  checkIfShouldAnimate = () => {
    const {
      isFullyDecentralized,
      onSetStakingInfoWasOpen,
      isAnimating,
    } = this.props;
    const { wasAnimated } = this.state;
    if (!isAnimating && !wasAnimated && isFullyDecentralized) {
      this.startAnimation();
      onSetStakingInfoWasOpen();
    }
  };

  startAnimation = () => {
    this.setState({
      wasAnimated: true,
    });
    this.props.onStartStakingInfoAnimation();
    setTimeout(this.stopAnimation, DECENTRALIZATED_ANIMATION_DURATION);
  };

  stopAnimation = () => {
    this.props.onStopStakingInfoAnimation();
  };

  render() {
    const { intl } = this.context;
    const {
      percentage,
      onLearnMoreClick,
      epoch,
      isAnimating,
      isFullyDecentralized,
    } = this.props;
    const heading = isFullyDecentralized
      ? intl.formatMessage(messages.headingAfter)
      : intl.formatMessage(messages.headingBefore);
    const description = isFullyDecentralized
      ? messages.descriptionAfter
      : messages.descriptionBefore;
    const buttonLabel = intl.formatMessage(messages.buttonLabel);
    const { epochNumber, epochStart } = epoch;
    return (
      <div className={styles.component}>
        <div className={styles.mainContent}>
          <div className={styles.content}>
            <div className={styles.heading}>{heading}</div>
            <div className={styles.description}>
              <FormattedHTMLMessage
                {...description}
                values={{
                  percentageDecentralized: percentage,
                  percentageFederated: 100 - percentage,
                  epochNumber,
                }}
              />
            </div>
            <div className={styles.countdownTitle}>
              {intl.formatMessage(messages.countdownTitle)}
            </div>
            <CountdownWidget startDateTime={epochStart} format="DD-HH-mm-ss" />
            <ButtonLink
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
          <FullyDecentralizedEffect isActive={isAnimating} />
        </div>
      </div>
    );
  }
}
