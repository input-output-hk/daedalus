import React from 'react';
import { injectIntl } from 'react-intl';
import type { Intl } from '../../../types/i18nTypes';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './CurrentPhase.scss' or its co... Remove this comment to see the full error message
import styles from './ApiError.scss';
import { messages } from './ApiError.messages';

type Props = {
  intl: Intl;
};

function ApiError({ intl }: Props) {
  return (
    <section className={styles.root}>
      <h1 className={styles.title}>{intl.formatMessage(messages.title)}</h1>
      <span className={styles.description1}>
        {intl.formatMessage(messages.description1)}
      </span>
      <span>{intl.formatMessage(messages.description2)}</span>
    </section>
  );
}

export default injectIntl(ApiError);
