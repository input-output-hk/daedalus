import React from 'react';
import { injectIntl } from 'react-intl';
import type { Intl } from '../../../types/i18nTypes';
import styles from './ApiError.scss';
import { messages } from './ApiError.messages';

type Props = {
  intl: Intl;
};

function ApiError({ intl }: Props) {
  return (
    <section className={styles.root}>
      <h1 className={styles.title}>{intl.formatMessage(messages.title)}</h1>
      <span>{intl.formatMessage(messages.description1)}</span>
      <span className={styles.description2}>
        {intl.formatMessage(messages.description2)}
      </span>
    </section>
  );
}

export default injectIntl(ApiError);
