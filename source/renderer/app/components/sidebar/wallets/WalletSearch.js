// @flow
import React from 'react';
import { intlShape, injectIntl, defineMessages } from 'react-intl';
import { noop } from 'lodash/fp';
import classNames from 'classnames';
import { Input } from 'react-polymorph/lib/components/Input';
import SVGInline from 'react-svg-inline';
import { observer } from 'mobx-react';
import searchIcon from '../../../assets/images/search.inline.svg';
import crossIcon from '../../../assets/images/close-cross.inline.svg';
import styles from './WalletSearch.scss';

const messages = defineMessages({
  placeholder: {
    id: 'sidebar.wallets.search.placeholder',
    defaultMessage: '!!!Filter',
    description: 'Search placeholder for the sidebar wallet menu',
  },
});

type Props = {
  intl: intlShape.isRequired,
  onSearch?: Function,
  searchValue?: string,
};

const WalletSearchComponent = ({
  searchValue = '',
  onSearch = noop,
  intl,
}: Props) => {
  const hasValue = !!searchValue.length;
  return (
    <label htmlFor="sideBarwalletSearch" className={styles.component}>
      <SVGInline
        svg={searchIcon}
        className={classNames({
          [styles.searchIcon]: true,
          [styles.highlight]: hasValue,
        })}
      />
      <Input
        id="sideBarwalletSearch"
        className={styles.input}
        onChange={onSearch}
        value={searchValue}
        placeholder={intl.formatMessage(messages.placeholder)}
      />
      {hasValue && (
        <button
          className={classNames([styles.clearButton, 'flat'])}
          onClick={() => onSearch('')}
        >
          <SVGInline svg={crossIcon} />
        </button>
      )}
    </label>
  );
};

export const WalletSearch = injectIntl(observer(WalletSearchComponent));
