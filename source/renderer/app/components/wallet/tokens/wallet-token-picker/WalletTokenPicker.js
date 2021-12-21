// @flow
import React from 'react';
import { observer } from 'mobx-react';
import { injectIntl } from 'react-intl';
import classNames from 'classnames';
import { Checkbox } from 'react-polymorph/lib/components/Checkbox';
import { Select } from 'react-polymorph/lib/components/Select';
import Dialog from '../../../widgets/Dialog';
import WalletToken from '../wallet-token/WalletToken';
import WalletTokensSearch from '../wallet-tokens-search/WalletTokensSearch';
import styles from './WalletTokenPicker.scss';
import { messages } from './WalletTokenPicker.messages';
import { filterSelectOptions } from './helpers';
import { useFilters, useCheckboxes, useScrollPosition } from './hooks';
import { MAX_TOKENS, ScrollPositionEnum } from './const';
import type { Intl } from '../../../../types/i18nTypes';
import type { AssetToken } from '../../../../api/assets/types';

type Props = {
  intl: Intl,
  assets: Array<AssetToken>,
  tokenFavorites: Object,
  previousCheckedIds?: Array<string>,
  onAdd: Function,
  onCancel: Function,
};

const WalletTokenPicker = ({
  intl,
  assets,
  tokenFavorites,
  previousCheckedIds = [],
  onAdd,
  onCancel,
}: Props) => {
  const { scrollableRef, scrollPosition } = useScrollPosition();
  const {
    searchValue,
    setSearchValue,
    currentAssets,
    filterOption,
    setFilterOption,
  } = useFilters({
    assets,
    tokenFavorites,
  });
  const {
    checkboxes,
    checkedCount,
    checkedIds,
    disabledIdsSet,
    toggleCheckbox,
    check30First,
  } = useCheckboxes({
    assets,
    previousCheckedIds,
  });
  const toolbarStyles = classNames(
    styles.toolbar,
    scrollPosition !== ScrollPositionEnum.TOP && styles.scrollTop
  );
  const actions = [
    {
      label: intl.formatMessage(messages.cancelButtonLabel),
      onClick: onCancel,
    },
    {
      label: intl.formatMessage(messages.addButtonLabel),
      primary: true,
      disabled: !checkedIds.length,
      onClick: () => onAdd(checkedIds),
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
          <Select
            value={filterOption}
            onChange={setFilterOption}
            className={styles.filterSelect}
            options={filterSelectOptions(intl)}
            selectionRenderer={(option) => option.label}
          />
          <span className={styles.count}>
            {intl.formatMessage(messages.checkedCountLabel, {
              checkedCount,
              maxTokens: MAX_TOKENS,
            })}
          </span>
          <button className={styles.check30First} onClick={check30First}>
            {intl.formatMessage(messages.check30FirstLabel)}
          </button>
        </div>
        <div className={styles.list} ref={scrollableRef}>
          {currentAssets.map((asset) => (
            <div className={styles.listItem} key={asset.uniqueId}>
              <Checkbox
                className={styles.checkbox}
                checked={
                  checkboxes[asset.uniqueId] ||
                  disabledIdsSet.has(asset.uniqueId)
                }
                onChange={() => toggleCheckbox(asset.uniqueId)}
                disabled={disabledIdsSet.has(asset.uniqueId)}
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
