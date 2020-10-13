// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import styles from './VotingAdd.scss';
import BorderedBox from '../widgets/BorderedBox';
import type { Node } from 'react';
import { Stepper } from 'react-polymorph/lib/components/Stepper';
import { StepperSkin } from 'react-polymorph/lib/skins/simple/StepperSkin';
import { defineMessages, FormattedMessage, intlShape } from 'react-intl';

const messages = defineMessages({
  heading: {
    id: 'voting.votingAdd.heading',
    defaultMessage: '!!!Register to vote',
    description: 'Headline for Voting add Stepper.',
  },
  subTitle: {
    id: 'voting.votingAdd.subTitle',
    defaultMessage: '!!!Step {step} of {stepCount}',
    description: 'Sub title for Voting add.',
  },
});

type Props = {
  children: Node,
  stepsList: Array<string>,
  activeStep: number,
};

@observer
export default class VotingAdd extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  static defaultProps = {
    children: null,
  };

  render() {
    const { intl } = this.context;
    const { children, activeStep, stepsList } = this.props;

    const heading = intl.formatMessage(messages.heading);

    return (
      <div className={styles.component}>
        <BorderedBox>
          <div className={styles.heading}>{heading}</div>
          <div className={styles.subTitle}>
            <FormattedMessage
              {...messages.subTitle}
              values={{ step: activeStep, stepCount: stepsList.length }}
            />
          </div>
          <div className={styles.votingAddStepsIndicatorWrapper}>
            <Stepper
              steps={stepsList}
              activeStep={activeStep}
              skin={StepperSkin}
              labelDisabled
            />
          </div>
          <div>{children}</div>
        </BorderedBox>
      </div>
    );
  }
}
