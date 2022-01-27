import React, { useState } from 'react';
import { intlShape, injectIntl, defineMessages } from 'react-intl';
import classNames from 'classnames';
import { Input } from 'react-polymorph/lib/components/Input';
import SVGInline from 'react-svg-inline';
import { observer } from 'mobx-react';
import styles from './WalletTokensSearch.scss';
import searchIcon from '../../../../assets/images/search.inline.svg';
import crossIcon from '../../../../assets/images/close-cross.inline.svg';
const messages = defineMessages({
  placeholder: {
    id: 'wallet.tokens.search.placeholder',
    defaultMessage: '!!!Search tokens',
    description: 'Search placeholder for the Wallet Tokens search',
  },
});
type Props = {
  intl: intlShape.isRequired;
  onSearch: (term: string) => string;
  searchValue: string;
};

const WalletTokensSearch = (props: Props) => {
  const { searchValue, onSearch, intl } = props;
  const [focus, setFocus] = useState(false);
  return (
    <div className={styles.component}>
      <SVGInline
        svg={searchIcon}
        className={classNames(
          styles.searchIcon,
          focus && styles.focusSearchIcon
        )}
      />
      <Input
        onFocus={() => setFocus(true)}
        onBlur={() => setFocus(false)}
        onChange={onSearch}
        value={searchValue}
        placeholder={intl.formatMessage(messages.placeholder)}
      />
      {!!searchValue.length && (
        <button
          className={classNames([styles.clearButton, 'flat'])}
          onClick={() => onSearch('')}
        >
          <SVGInline svg={crossIcon} />
        </button>
      )}
    </div>
  );
};

export default injectIntl(observer(WalletTokensSearch));
