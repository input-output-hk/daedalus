// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import {
  defineMessages,
  intlShape,
  FormattedMessage,
  FormattedHTMLMessage,
} from 'react-intl';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import ButtonLink from '../../widgets/ButtonLink';
import styles from './StakingInfo.scss';
import FullyDecentralizedEffect from '../../widgets/FullyDecentralizedEffect';
import CountdownWidget from '../../widgets/CountdownWidget';

const messages = defineMessages({
  headingBefore: {
    id: 'staking.info.before.heading',
    defaultMessage: '!!!Fully decentralized block production',
    description: 'Headline for the Decentralization progress notification.',
  },
  descriptionBefore: {
    id: 'staking.info.before.description',
    defaultMessage:
      '!!!<p>Cardano is fast approaching full decentralization for block production. Soon, all blocks will be produced by Cardano’s network of stake pools. </p> <p>Currently, stake pools are producing {percentageDecentralized}% of blocks, while federated nodes are just producing {percentageFederated}%.</p> <p>At the boundary of Cardano epoch #{epochNumber}, stake pools will start producing 100% of blocks and full block decentralization will be achieved.</p>',
    description:
      'Info description for the Decentralization progress notification.',
  },
  headingAfter: {
    id: 'staking.info.after.heading',
    defaultMessage: '!!!Cardano is transitioning into a decentralized system',
    description: 'Headline for the Decentralization progress notification.',
  },
  descriptionAfter: {
    id: 'staking.info.after.description',
    defaultMessage:
      '!!!Cardano is transitioning from a federated system operated by its creators to a decentralized system operated by a community of stake pool operators. During this transition, blocks will be produced both by the federated nodes and by stake pools. The percentage of blocks produced by stake pools will increase every epoch until block production in the Cardano network becomes fully decentralized.',
    description:
      'Info description for the Decentralization progress notification.',
  },
  countdownTitle: {
    id: 'staking.info.countdownTitle',
    defaultMessage: '!!!Fully decentralized block production in',
    description:
      'Countdown Title for the Decentralization progress notification.',
  },
  buttonLabel: {
    id: 'staking.info.buttonLabel',
    defaultMessage: '!!!Learn more',
    description: 'Button Label for the Decentralization progress notification.',
  },
});

type Props = {
  percentage: number,
  onLearnMoreClick: Function,
  epochNumber: number,
  date: string,
};

@observer
export default class StakingInfo extends Component<Props> {
  static defaultProps = {
    percentage: 0,
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const {
      percentage,
      onLearnMoreClick,
      epochNumber,
      date,
      date2,
    } = this.props;
    const isFullyDecentralized = percentage === 100;
    const heading = isFullyDecentralized
      ? intl.formatMessage(messages.headingAfter)
      : intl.formatMessage(messages.headingBefore);
    const description = isFullyDecentralized
      ? messages.descriptionAfter
      : messages.descriptionBefore;
    const buttonLabel = intl.formatMessage(messages.buttonLabel);
    const showLearnMoreButton = false;
    return (
      <div className={styles.component}>
        <div className={styles.mainContent}>
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
          <CountdownWidget startDateTime={date} format="DD-HH-mm-ss" />
          <ButtonLink
            className={styles.learnMoreButton}
            onClick={onLearnMoreClick}
            skin={ButtonSkin}
            label={buttonLabel}
            linkProps={{
              className: styles.externalLinkIcon,
            }}
          />
          <FullyDecentralizedEffect
            isActive={false}
            effect="fireworks"
            currentTheme="light-blue"
            isActive1={isFullyDecentralized}
          />
        </div>
      </div>
    );
  }
}
