import React from 'react';
import { intlShape, injectIntl, defineMessages } from 'react-intl';
import { noop } from 'lodash/fp';
import classNames from 'classnames';
import { Input } from 'react-polymorph/lib/components/Input';
import SVGInline from 'react-svg-inline';
import { observer } from 'mobx-react';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/search.... Remove this comment to see the full error message
import searchIcon from '../../../assets/images/search.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/close-c... Remove this comment to see the full error message
import crossIcon from '../../../assets/images/close-cross.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './WalletSearch.scss' or its co... Remove this comment to see the full error message
import styles from './WalletSearch.scss';

const messages = defineMessages({
  placeholder: {
    id: 'sidebar.wallets.search.placeholder',
    defaultMessage: '!!!Filter',
    description: 'Search placeholder for the sidebar wallet menu',
  },
});
type Props = {
  intl: intlShape.isRequired;
  onSearch?: (...args: Array<any>) => any;
  searchValue?: string;
};

const WalletSearchComponent = ({
  searchValue = '',
  onSearch = noop,
  intl,
}: Props) => {
  const hasValue = !!searchValue.length;
  return (
    <label
      htmlFor="sideBarwalletSearch"
      className={classNames({
        [styles.component]: true,
        [styles.hasValue]: hasValue,
      })}
    >
      <SVGInline svg={searchIcon} className={styles.searchIcon} />
      <Input
        id="sideBarwalletSearch"
        className={styles.input}
        onChange={onSearch}
        spellCheck={false}
        value={searchValue}
        placeholder={intl.formatMessage(messages.placeholder)}
      />
      {hasValue && (
        <button className={styles.clearButton} onClick={() => onSearch('')}>
          <SVGInline svg={crossIcon} />
        </button>
      )}
    </label>
  );
};

export const WalletSearch = injectIntl(observer(WalletSearchComponent));
