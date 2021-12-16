// @flow
import React from 'react';
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
  intl: intlShape.isRequired,
  onSearch: Function,
  searchValue: string,
};

const WalletTokensSearch = (props: Props) => {
  const { searchValue, onSearch, intl } = props;
  return (
    <div className={styles.component}>
      <SVGInline svg={searchIcon} className={styles.searchIcon} />
      <Input
        className={styles.spendingPassword}
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
