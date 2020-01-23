// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape, FormattedMessage } from 'react-intl';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import ButtonLink from '../../widgets/ButtonLink';
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

type Props = {
  percentage: number,
  onLearnMoreClick: Function,
};
type State = { progressLabelClassName: string };

@observer
export default class StakingInfo extends Component<Props, State> {
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
    this.state = { progressLabelClassName: styles.progressLabelWhite };
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

    this.setState({ progressLabelClassName });
  };

  render() {
    const { intl } = this.context;
    const { percentage, onLearnMoreClick } = this.props;
    const { progressLabelClassName } = this.state;
    const heading = intl.formatMessage(messages.heading);
    const description = intl.formatMessage(messages.description);
    const buttonLabel = intl.formatMessage(messages.buttonLabel);

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
                ref={this.progressRef}
                style={{ width: `${percentage}%` }}
              >
                <div className={progressLabelClassName}>{percentage}%</div>
              </div>
            </div>
          </div>
          <ButtonLink
            className={styles.learnMoreButton}
            onClick={onLearnMoreClick}
            skin={ButtonSkin}
            label={buttonLabel}
            linkProps={{
              className: styles.externalLinkIcon,
            }}
          />
        </div>
      </div>
    );
  }
}
