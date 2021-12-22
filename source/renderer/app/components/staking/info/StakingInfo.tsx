import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape, FormattedMessage } from 'react-intl';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import ButtonLink from '../../widgets/ButtonLink';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './StakingInfo.scss' or its cor... Remove this comment to see the full error message
import styles from './StakingInfo.scss';

const messages = defineMessages({
  heading: {
    id: 'staking.info.heading',
    defaultMessage: '!!!Cardano is transitioning into a decentralized system',
    description: 'Headline for the Decentralization progress notification.',
  },
  description: {
    id: 'staking.info.description',
    defaultMessage:
      '!!!Cardano is transitioning from a federated system operated by its creators to a decentralized system operated by a community of stake pool operators. During this transition, blocks will be produced both by the federated nodes and by stake pools. The percentage of blocks produced by stake pools will increase every epoch until block production in the Cardano network becomes fully decentralized.',
    description:
      'Info description for the Decentralization progress notification.',
  },
  percentage: {
    id: 'staking.info.percentage',
    defaultMessage:
      '!!!Currently, {percentage}% of the blocks are produced by the stake pools.',
    description:
      'Percentage info description for the Decentralization progress notification.',
  },
  buttonLabel: {
    id: 'staking.info.buttonLabel',
    defaultMessage: '!!!Learn more',
    description: 'Button Label for the Decentralization progress notification.',
  },
  learnMoreLinkUrl: {
    id: 'staking.info.learnMore.linkUrl',
    defaultMessage: '!!!https://iohk.zendesk.com/hc',
    description: '"Learn more" link URL in the staking info page',
  },
});
type Props = {
  percentage: number;
  onLearnMoreClick: (...args: Array<any>) => any;
};
type State = {
  progressLabelClassName: string;
};

@observer
class StakingInfo extends Component<Props, State> {
  static defaultProps = {
    percentage: 0,
  };
  static contextTypes = {
    intl: intlShape.isRequired,
  };
  progressRef: any;

  constructor(props: Props) {
    super(props);
    this.progressRef = React.createRef();
    this.state = {
      progressLabelClassName: styles.progressLabelWhite,
    };
  }

  componentDidMount() {
    this.handleProgressLabelClassName();
  }

  componentDidUpdate(prevProps: Props) {
    const { percentage: prevPercentage } = prevProps;
    const { percentage: currentPercentage } = this.props;

    if (prevPercentage !== currentPercentage) {
      this.handleProgressLabelClassName();
    }
  }

  handleProgressLabelClassName = () => {
    const { current: progressComponent } = this.progressRef;
    const progressLabelClassName =
      progressComponent.clientWidth >= 50
        ? styles.progressLabelWhite
        : styles.progressLabel;
    this.setState({
      progressLabelClassName,
    });
  };

  render() {
    const { intl } = this.context;
    const { percentage, onLearnMoreClick } = this.props;
    const { progressLabelClassName } = this.state;
    const heading = intl.formatMessage(messages.heading);
    const description = intl.formatMessage(messages.description);
    const buttonLabel = intl.formatMessage(messages.buttonLabel);
    const showLearnMoreButton = false;
    return (
      <div className={styles.component}>
        <div className={styles.mainContent}>
          <div className={styles.heading}>{heading}</div>
          <div className={styles.description}>{description}</div>
          <div className={styles.description}>
            <FormattedMessage
              {...messages.percentage}
              values={{
                percentage,
              }}
            />
          </div>
          <div className={styles.progressBar}>
            <div className={styles.progressBarContainer}>
              <div
                className={styles.progress}
                ref={this.progressRef}
                style={{
                  width: `${percentage}%`,
                }}
              >
                <div className={progressLabelClassName}>{percentage}%</div>
              </div>
            </div>
          </div>
          {showLearnMoreButton && (
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
          )}
        </div>
      </div>
    );
  }
}

export default StakingInfo;
