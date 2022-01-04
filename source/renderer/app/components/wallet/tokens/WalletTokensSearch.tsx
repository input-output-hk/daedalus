import React from 'react';
import { intlShape, injectIntl, defineMessages } from 'react-intl';
import classNames from 'classnames';
import { Input } from 'react-polymorph/lib/components/Input';
import SVGInline from 'react-svg-inline';
import { observer } from 'mobx-react';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './WalletTokensSearch.scss' or ... Remove this comment to see the full error message
import styles from './WalletTokensSearch.scss';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/search.... Remove this comment to see the full error message
import searchIcon from '../../../assets/images/search.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/close-c... Remove this comment to see the full error message
import crossIcon from '../../../assets/images/close-cross.inline.svg';

const messages = defineMessages({
  placeholder: {
    id: 'wallet.tokens.search.placeholder',
    defaultMessage: '!!!Search tokens',
    description: 'Search placeholder for the Wallet Tokens search',
  },
});
type Props = {
  intl: intlShape.isRequired;
  onSearch: (...args: Array<any>) => any;
  searchValue: string;
};
const WalletTokensSearch = observer((props: Props) => {
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
});
export default injectIntl(WalletTokensSearch);
