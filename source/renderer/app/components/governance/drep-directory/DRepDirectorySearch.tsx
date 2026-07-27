import React from 'react';
import { defineMessages, injectIntl, intlShape } from 'react-intl';
import { Input } from 'react-polymorph/lib/components/Input';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import type { DRepQueryKind } from './helpers';
import styles from './DRepDirectorySearch.scss';

const messages = defineMessages({
  placeholder: {
    id: 'governance.drepDirectory.searchPlaceholder',
    defaultMessage: '!!!Search by DRep ID',
    description: 'Placeholder of the DRep ID search input',
  },
  minLengthHint: {
    id: 'governance.drepDirectory.search.minLengthHint',
    defaultMessage: '!!!Enter at least 8 characters to search by ID',
    description: 'Hint shown while the search query is below the minimum',
  },
  invalidId: {
    id: 'governance.drepDirectory.search.invalidId',
    defaultMessage: '!!!Invalid DRep ID',
    description: 'Error shown for a full-length DRep ID that fails validation',
  },
});

interface Props {
  value: string;
  queryKind: DRepQueryKind;
  onChange: (value: string) => void;
  intl: intlShape.isRequired;
}

function DRepDirectorySearch({ value, queryKind, onChange, intl }: Props) {
  return (
    <div className={styles.container}>
      <Input
        className={styles.input}
        value={value}
        onChange={onChange}
        placeholder={intl.formatMessage(messages.placeholder)}
        skin={InputSkin}
      />
      {queryKind === 'belowMinimum' && (
        <p className={styles.hint}>
          {intl.formatMessage(messages.minLengthHint)}
        </p>
      )}
      {queryKind === 'invalidFullForm' && (
        <p className={styles.error} role="alert">
          {intl.formatMessage(messages.invalidId)}
        </p>
      )}
    </div>
  );
}

export default injectIntl(DRepDirectorySearch);
