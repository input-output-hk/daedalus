// @flow
import React from 'react';
import { observer } from 'mobx-react';
import { injectIntl } from 'react-intl';
import classNames from 'classnames';
import { Checkbox } from 'react-polymorph/lib/components/Checkbox';
import Dialog from '../../../widgets/Dialog';
import WalletToken from '../wallet-token/WalletToken';
import WalletTokensSearch from '../wallet-tokens-search/WalletTokensSearch';
import { ScrollPositionEnum, MAX_TOKENS } from './helpers';
import { useSearch, useCheckboxes, useScrollPosition } from './hooks';
import styles from './WalletTokenPicker.scss';
import { messages } from './WalletTokenPicker.messages';
import type { Intl } from '../../../../types/i18nTypes';
import type { AssetToken } from '../../../../api/assets/types';

type Props = {
  intl: Intl,
  assets: Array<AssetToken>,
  tokenFavorites: Object,
};

const WalletTokenPicker = ({ intl, assets, tokenFavorites }: Props) => {
  const { searchValue, setSearchValue } = useSearch();
  const { scrollableRef, scrollPosition } = useScrollPosition();
  const {
    checkedCount,
    checkboxes,
    toggleCheckbox,
    checkFirst30,
  } = useCheckboxes({
    assets,
  });
  const toolbarStyles = classNames(
    styles.toolbar,
    scrollPosition !== ScrollPositionEnum.TOP && styles.scrollTop
  );

  const actions = [
    {
      label: intl.formatMessage(messages.cancelButtonLabel),
      onClick: () => {},
    },
    {
      label: intl.formatMessage(messages.addButtonLabel),
      primary: true,
      disabled: false,
      onClick: () => {},
    },
  ];

  return (
    <Dialog
      className={styles.dialog}
      title={intl.formatMessage(messages.title)}
      closeOnOverlayClick
      actions={actions}
    >
      <div className={styles.root}>
        <WalletTokensSearch
          searchValue={searchValue}
          onSearch={setSearchValue}
        />
        <div className={toolbarStyles}>
          <span className={styles.count}>
            {intl.formatMessage(messages.checkedCountLabel, {
              checkedCount,
              maxTokens: MAX_TOKENS,
            })}
          </span>
          <button className={styles.selectButton} onClick={checkFirst30}>
            {intl.formatMessage(messages.select30label)}
          </button>
        </div>
        <div className={styles.list} ref={scrollableRef}>
          {assets.map((asset) => (
            <div className={styles.listItem} key={asset.uniqueId}>
              <Checkbox
                className={styles.checkbox}
                checked={checkboxes[asset.uniqueId]}
                onChange={() => toggleCheckbox(asset.uniqueId)}
              />
              <WalletToken
                asset={asset}
                className={styles.token}
                headerClassName={styles.tokenHeader}
                fullFingerprint={false}
                isFavorite={tokenFavorites[asset.uniqueId]}
              />
            </div>
          ))}
        </div>
      </div>
    </Dialog>
  );
};

export default injectIntl(observer(WalletTokenPicker));
