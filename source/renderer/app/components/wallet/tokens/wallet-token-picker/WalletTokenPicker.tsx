import React, { useMemo } from 'react';
import { observer } from 'mobx-react';
import { injectIntl } from 'react-intl';
import classNames from 'classnames';
import { Select } from 'react-polymorph/lib/components/Select';
import Dialog from '../../../widgets/Dialog';
import WalletToken from '../wallet-token/WalletToken';
import WalletTokensSearch from '../wallet-tokens-search/WalletTokensSearch';
import WalletTokenPickerCheckbox from './WalletTokenPickerCheckbox';
import DialogCloseButton from '../../../widgets/DialogCloseButton';
import styles from './WalletTokenPicker.scss';
import { messages } from './WalletTokenPicker.messages';
import {
  filterSelectOptions,
  getToggleAllLabel,
  getTokenCounterText,
} from './helpers';
import { useFilters, useCheckboxes, useScrollPosition } from './hooks';
import { MAX_TOKENS, ScrollPositionEnum } from './const';
import type { Props } from './types';

function WalletTokenPicker({
  intl,
  assets,
  walletName,
  tokenFavorites,
  previouslyCheckedIds = [],
  onAdd,
  onCancel,
}: Props) {
  const { onScroll, scrollPosition } = useScrollPosition();
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
    totalCheckedCount,
    checkedIds,
    previouslyCheckedIdsSet,
    isMaxTotalCount,
    isToggleAllDisabled,
    isClearAllMode,
    toggleAllFn,
    toggleCheckbox,
  } = useCheckboxes({
    assets,
    currentAssets,
    previouslyCheckedIds,
  });
  const scrollNotTop = scrollPosition !== ScrollPositionEnum.TOP;
  const toolbarStyles = classNames(
    styles.toolbar,
    scrollNotTop && styles.scrollNotTop
  );
  const toolbarContainerStyles = classNames(
    scrollNotTop && styles.toolbarContainer
  );
  const actions = useMemo(
    () => [
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
    ],
    [intl, checkedIds, onAdd]
  );
  return (
    <Dialog
      className={styles.dialog}
      title={intl.formatMessage(messages.title)}
      subtitle={walletName}
      closeOnOverlayClick
      actions={actions}
      onClose={onCancel}
      closeButton={<DialogCloseButton />}
    >
      <div className={styles.root} data-testid="WalletTokenPicker">
        <div className={styles.search}>
          <WalletTokensSearch
            searchValue={searchValue}
            onSearch={setSearchValue}
          />
        </div>

        <div className={toolbarContainerStyles}>
          <div className={toolbarStyles}>
            <Select
              value={filterOption}
              onChange={setFilterOption}
              className={styles.filterSelect}
              options={filterSelectOptions(intl)}
              selectionRenderer={(option: { label: string }) => (
                <span>
                  {option.label}
                  <span className={styles.filterCounter}>
                    {getTokenCounterText({
                      assets,
                      currentAssets,
                    })}
                  </span>
                </span>
              )}
              optionRenderer={(option: { label: string }) => (
                <span className={styles.filterOption}>{option.label}</span>
              )}
              optionHeight={33}
            />
            <span className={styles.count}>
              {intl.formatMessage(messages.checkedCountLabel, {
                checkedCount: totalCheckedCount,
                maxTokens: Math.min(MAX_TOKENS, assets.length),
              })}
            </span>
            <button
              className={styles.toggleAllButton}
              onClick={toggleAllFn}
              disabled={isToggleAllDisabled}
            >
              {intl.formatMessage(messages[getToggleAllLabel(isClearAllMode)], {
                maxTokens: MAX_TOKENS,
              })}
            </button>
          </div>
        </div>
        {/* @ts-ignore ts-migrate(2322) FIXME: Type '(evt: React.MouseEvent<HTMLElement>) => void... Remove this comment to see the full error message */}
        <div className={styles.list} onScroll={onScroll}>
          {currentAssets?.length === 0 && (
            <span className={styles.noResults}>
              {intl.formatMessage(messages.noResults)}
            </span>
          )}
          {currentAssets?.map((asset) => (
            <div key={asset.uniqueId} className={styles.listItem}>
              <WalletTokenPickerCheckbox
                className={styles.checkbox}
                isChecked={checkboxes[asset.uniqueId]}
                isMaxCount={isMaxTotalCount}
                isPreviouslyChecked={previouslyCheckedIdsSet.has(
                  asset.uniqueId
                )}
                uniqueId={asset.uniqueId}
                toggleCheckbox={toggleCheckbox}
              />
              {/* @ts-ignore ts-migrate(2739) FIXME: Type '{ asset: AssetToken; className: any; headerC... Remove this comment to see the full error message */}
              <WalletToken
                asset={asset}
                className={styles.token}
                headerClassName={styles.tokenHeader}
                footerClassName={styles.tokenFooter}
                fullFingerprint={false}
                isFavorite={tokenFavorites[asset.uniqueId]}
              />
            </div>
          ))}
        </div>
      </div>
    </Dialog>
  );
}

export default injectIntl(observer(WalletTokenPicker));
