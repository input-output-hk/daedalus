import React from 'react';
import { FormattedHTMLMessage } from 'react-intl';
import { FormattedHTMLMessageWithLink } from '../../../../components/widgets/FormattedHTMLMessageWithLink';
import LocalizableError from '../../../../i18n/LocalizableError';
import styles from './styles.scss';

interface Props {
  error: LocalizableError | null | undefined;
  onExternalLinkClick: (...args: Array<any>) => any;
}

export function ConfirmationError({ error, onExternalLinkClick }: Props) {
  if (!error) {
    return null;
  }

  const errorHasLink = !!error.values.linkLabel;

  return (
    <p className={styles.error}>
      {errorHasLink ? (
        <FormattedHTMLMessageWithLink
          // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
          message={error}
          onExternalLinkClick={onExternalLinkClick}
        />
      ) : (
        <FormattedHTMLMessage {...error} />
      )}
    </p>
  );
}
