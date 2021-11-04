// @flow
import React from 'react';

import { defineMessages, injectIntl } from 'react-intl';

import { ExternalLinkButton } from '../../widgets/ExternalLinkButton';
import type { Intl } from '../../../types/i18nTypes';

import styles from './Headline.scss';

const messages = defineMessages({
  heading: {
    id: 'voting.catalyst.heading',
    defaultMessage: '!!!Project Catalyst',
    description: 'Headline Project Catalyst',
  },
  descriptionRow1: {
    id: 'voting.catalyst.descriptionRow1',
    defaultMessage:
      '!!!Decide which innovative ideas for Cardano will receive funding.',
    description: 'Description Project Catalyst',
  },
  descriptionRow2: {
    id: 'voting.catalyst.descriptionRow2',
    defaultMessage:
      '!!!$1,040,000 worth of ada rewards will be distributed between ada holders who register their vote.',
    description: 'Description Project Catalyst',
  },
  learnMoreLinkLabel: {
    id: 'voting.info.learnMoreLinkLabel',
    defaultMessage: '!!!Learn more',
    description: 'Learn more link label for registration steps',
  },
  learnMoreLinkUrl: {
    id: 'voting.info.learnMoreLinkUrl',
    defaultMessage: '!!!https://cardano.ideascale.com/a/index',
    description: 'Learn more link url for registration steps',
  },
});

type Props = {
  onLearnMoreLinkClick: Function,
  intl: Intl,
};

function Headline({ onLearnMoreLinkClick, intl }: Props) {
  return (
    <section className={styles.component}>
      <h1 className={styles.heading}>{intl.formatMessage(messages.heading)}</h1>
      <div className={styles.descriptionBox}>
        <div>
          <p className={styles.description}>
            {intl.formatMessage(messages.descriptionRow1)}
          </p>
          <p className={styles.description}>
            {intl.formatMessage(messages.descriptionRow2)}
          </p>
        </div>
        <ExternalLinkButton
          label={intl.formatMessage(messages.learnMoreLinkLabel)}
          onClick={() => onLearnMoreLinkClick(messages.learnMoreLinkUrl)}
        />
      </div>
    </section>
  );
}

export default injectIntl(Headline);
