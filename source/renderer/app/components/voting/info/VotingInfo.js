// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, FormattedHTMLMessage, intlShape } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import BorderedBox from '../../widgets/BorderedBox';
import styles from './VotingInfo.scss';

const messages = defineMessages({
  heading: {
    id: 'voting.info.heading',
    defaultMessage: '!!!Register to vote',
    description: 'Headline for .',
  },
  description: {
    id: 'voting.info.description',
    defaultMessage:
      '!!!To start yo will need to have the <span>Catalyst Voting App</span> ready on your smart phone! To register to vote you will need a <span>PIN code</span> and a <span>QR code</span> that you will generate here within <span>Daedalus</span>. Before you start',
    description: '!!!Info description for .',
  },
  buttonLabel: {
    id: 'voting.info.buttonLabel',
    defaultMessage: 'Register to vote',
    description: 'Button Label for .',
  },
});

type Props = {
  onRegisterToVoteClick: Function,
};

@observer
export default class VotingInfo extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { onRegisterToVoteClick } = this.props;
    const heading = intl.formatMessage(messages.heading);
    const buttonLabel = intl.formatMessage(messages.buttonLabel);

    return (
      <div className={styles.component}>
        <BorderedBox>
          <div className={styles.heading}>{heading}</div>
          <div className={styles.description}>
            <FormattedHTMLMessage {...messages.description} />
          </div>
          <Button
            onClick={onRegisterToVoteClick}
            skin={ButtonSkin}
            label={buttonLabel}
          />
        </BorderedBox>
      </div>
    );
  }
}
