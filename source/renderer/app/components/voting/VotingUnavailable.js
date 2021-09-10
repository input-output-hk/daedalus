// @flow
import React, { Component, Fragment } from 'react';
import { observer } from 'mobx-react';
import {
  defineMessages,
  intlShape,
  FormattedMessage,
  FormattedHTMLMessage,
} from 'react-intl';
import BigNumber from 'bignumber.js';
import { Link } from 'react-polymorph/lib/components/Link';
import globalMessages from '../../i18n/global-messages';
import LoadingSpinner from '../widgets/LoadingSpinner';
import styles from './VotingUnavailable.scss';

const messages = defineMessages({
  heading: {
    id: 'voting.unavailable.heading',
    defaultMessage: '!!!Project Catalyst voting registration',
    description: 'Headline for the "Voting unavailable" screen',
  },
  paragraph1: {
    id: 'voting.unavailable.paragraph1',
    defaultMessage:
      '!!!Project Catalyst Fund5 has now ended. Fund6 is currently in preparation, voting registration is not available yet.',
    description: 'First paragraph on the "Voting unavailable" screen',
  },
  paragraph2: {
    id: 'voting.unavailable.paragraph2',
    defaultMessage:
      '!!!Join our {link1} and {link2} Telegram channels for the latest updates (English language only).',
    description: 'Second paragraph on the "Voting unavailable" screen',
  },
  link1Text: {
    id: 'voting.unavailable.link1Text',
    defaultMessage: '!!!Catalyst Announcements',
    description: 'First link text on the "Voting unavailable" screen',
  },
  link2Text: {
    id: 'voting.unavailable.link2Text',
    defaultMessage: '!!!Project Catalyst Chat',
    description: 'Second link text on the "Voting unavailable" screen',
  },
  link1Url: {
    id: 'voting.unavailable.link1Url',
    defaultMessage: '!!!https://t.me/cardanocatalyst',
    description: 'First link URL on the "Voting unavailable" screen',
  },
  link2Url: {
    id: 'voting.unavailable.link2Url',
    defaultMessage: '!!!https://t.me/ProjectCatalystChat',
    description: 'Second link URL on the "Voting unavailable" screen',
  },
});

type Props = {
  syncPercentage: number,
  isVotingRegistrationAvailable: boolean,
  onExternalLinkClick: Function,
};

@observer
export default class VotingUnavailable extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const {
      syncPercentage,
      isVotingRegistrationAvailable,
      onExternalLinkClick,
    } = this.props;

    const heading = intl.formatMessage(messages.heading);
    const paragraph1 = intl.formatMessage(messages.paragraph1);
    const link1 = (
      <Link
        className={styles.link}
        label={intl.formatMessage(messages.link1Text)}
        onClick={() =>
          onExternalLinkClick(intl.formatMessage(messages.link1Url))
        }
      />
    );
    const link2 = (
      <Link
        className={styles.link}
        label={intl.formatMessage(messages.link2Text)}
        onClick={() =>
          onExternalLinkClick(intl.formatMessage(messages.link2Url))
        }
      />
    );

    return (
      <div className={styles.component}>
        {isVotingRegistrationAvailable ? (
          <>
            <LoadingSpinner big />
            <div className={styles.description}>
              <FormattedHTMLMessage
                {...globalMessages.featureUnavailableWhileSyncing}
                values={{
                  syncPercentage: new BigNumber(syncPercentage).toFormat(2),
                }}
              />
            </div>
          </>
        ) : (
          <>
            <h1>{heading}</h1>
            <p>{paragraph1}</p>
            <FormattedMessage
              {...messages.paragraph2}
              values={{ link1, link2 }}
              tagName="p"
            />
          </>
        )}
      </div>
    );
  }
}
