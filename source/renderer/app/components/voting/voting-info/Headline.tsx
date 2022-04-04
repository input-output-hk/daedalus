import React from 'react';
import BigNumber from 'bignumber.js';
import { injectIntl } from 'react-intl';
import { ExternalLinkButton } from '../../widgets/ExternalLinkButton';
import { VOTING_REWARD } from '../../../config/votingConfig';
import type { Intl } from '../../../types/i18nTypes';
import styles from './Headline.scss';
import { messages } from './Headline.messages';

type Props = {
  onExternalLinkClick: (...args: Array<any>) => any;
  intl: Intl;
};

function Headline({ onExternalLinkClick, intl }: Props) {
  return (
    <section className={styles.component}>
      <h1 className={styles.heading}>{intl.formatMessage(messages.heading)}</h1>
      <div className={styles.content}>
        <div className={styles.descriptionBlock}>
          <p className={styles.description}>
            {intl.formatMessage(messages.descriptionRow1)}
          </p>
          <p className={styles.description}>
            {intl.formatMessage(messages.descriptionRow2, {
              reward: new BigNumber(VOTING_REWARD).toFormat(0),
            })}
          </p>
        </div>
        <ExternalLinkButton
          label={intl.formatMessage(messages.learnMoreLinkLabel)}
          onClick={() =>
            onExternalLinkClick(intl.formatMessage(messages.learnMoreLinkUrl))
          }
        />
      </div>
    </section>
  );
}

export default injectIntl(Headline);
