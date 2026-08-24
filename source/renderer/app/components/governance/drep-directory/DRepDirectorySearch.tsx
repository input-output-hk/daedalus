import React, { useRef, useState } from 'react';
import SVGInline from 'react-svg-inline';
import classnames from 'classnames';
import { defineMessages, injectIntl, intlShape } from 'react-intl';
import { Input } from 'react-polymorph/lib/components/Input';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
// @ts-ignore inline svg module
import searchIcon from '../../../assets/images/search.inline.svg';
// @ts-ignore inline svg module
import closeIcon from '../../../assets/images/close-cross.inline.svg';
import type { DRepQueryKind } from './helpers';
import styles from './DRepDirectorySearch.scss';

const messages = defineMessages({
  placeholder: {
    id: 'governance.drepDirectory.searchPlaceholder',
    defaultMessage: '!!!Search by DRep ID or name',
    description: 'Placeholder of the DRep ID or name search input',
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
  clearTooltip: {
    id: 'governance.drepDirectory.search.clearTooltip',
    defaultMessage: '!!!Clear',
    description: 'Tooltip of the button that empties the DRep search field',
  },
});

interface Props {
  value: string;
  queryKind: DRepQueryKind;
  onChange: (value: string) => void;
  intl: intlShape.isRequired;
}

function DRepDirectorySearch({ value, queryKind, onChange, intl }: Props) {
  const searchInput = useRef<{
    inputElement: { current: HTMLInputElement | null };
  } | null>(null);
  const [isFocused, setIsFocused] = useState(false);

  // Clearing hands the field back rather than leaving the caret nowhere: the
  // button is pressed to keep typing, not to finish.
  const handleClear = () => {
    onChange('');
    searchInput.current?.inputElement?.current?.focus();
    setIsFocused(true);
  };

  return (
    <div className={styles.container}>
      <div className={styles.field}>
        <SVGInline
          svg={searchIcon}
          className={classnames(
            styles.searchIcon,
            isFocused && styles.searchIconFocused
          )}
        />
        <Input
          className={styles.input}
          value={value}
          onChange={onChange}
          ref={(input) => {
            searchInput.current = input;
          }}
          onFocus={() => setIsFocused(true)}
          onBlur={() => setIsFocused(false)}
          placeholder={intl.formatMessage(messages.placeholder)}
          skin={InputSkin}
        />
        {value.length > 0 && (
          <div className={styles.clearSearch}>
            <PopOver content={intl.formatMessage(messages.clearTooltip)}>
              <button
                type="button"
                className={styles.clearSearchButton}
                aria-label={intl.formatMessage(messages.clearTooltip)}
                onClick={handleClear}
              >
                <SVGInline svg={closeIcon} className={styles.clearSearchIcon} />
              </button>
            </PopOver>
          </div>
        )}
      </div>
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
