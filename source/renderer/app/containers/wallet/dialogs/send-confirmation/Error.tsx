import React from 'react';
import { FormattedHTMLMessage } from 'react-intl';
import { FormattedHTMLMessageWithLink } from '../../../../components/widgets/FormattedHTMLMessageWithLink';
import LocalizableError from '../../../../i18n/LocalizableError';
import styles from './styles.scss';

interface Props {
  error:
    | (LocalizableError & {
        values: {
          linkPosition?: string;
          linkLabel: string;
          linkURL: string;
        };
      })
    | null
    | undefined;
  onExternalLinkClick: (...args: Array<any>) => any;
}

export const ConfirmationError = ({ error, onExternalLinkClick }: Props) => {
  if (!error) {
    return null;
  }

  const errorHasLink = !!error.values.linkLabel;

  return (
    <p className={styles.error}>
      {errorHasLink ? (
        <FormattedHTMLMessageWithLink
          message={error}
          onExternalLinkClick={onExternalLinkClick}
        />
      ) : (
        <FormattedHTMLMessage {...error} />
      )}
    </p>
  );
};
