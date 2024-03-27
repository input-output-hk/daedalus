// @ts-nocheck

import React, { useRef, useState } from 'react';
import SVGInline from 'react-svg-inline';
import { injectIntl } from 'react-intl';
import { Input } from '@react-polymorph/components/Input';
import { InputSkin } from '@react-polymorph/skins/simple/InputSkin';
import { PopOver } from '@react-polymorph/components/PopOver';
import classnames from 'classnames';
import styles from './StakePoolsSearch.scss';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/search.... Remove this comment to see the full error message
import searchIcon from '../../../assets/images/search.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/close-c... Remove this comment to see the full error message
import closeIcon from '../../../assets/images/close-cross.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/grid-ic... Remove this comment to see the full error message
import gridIcon from '../../../assets/images/grid-ic.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/grid-re... Remove this comment to see the full error message
import gridRewardsIcon from '../../../assets/images/grid-rewards.inline.svg';
import { IS_GRID_REWARDS_VIEW_AVAILABLE } from '../../../config/stakingConfig';
import type { Intl } from '../../../types/i18nTypes';
import { messages } from './StakePoolsSearch.messages';
import { StakePoolsSearchListViewButton } from './StakePoolsSearchListViewButton';
import { StakingPageScrollContext } from '../layouts/StakingWithNavigation';

type Props = {
  label?: string;
  placeholder?: string;
  isListView?: boolean;
  isListViewTooltipVisible?: boolean;
  isGridView?: boolean;
  isGridRewardsView?: boolean;
  onSearch: (...args: Array<any>) => any;
  onClearSearch: (...args: Array<any>) => any;
  onGridView?: (...args: Array<any>) => any;
  onGridRewardsView?: (...args: Array<any>) => any;
  onListView?: (...args: Array<any>) => any;
  onListViewVisited?: () => void;
  search: string;
  intl: Intl;
};

function StakePoolsSearchComponent({
  label,
  onClearSearch,
  onSearch,
  onGridView,
  onGridRewardsView,
  onListView,
  onListViewVisited,
  placeholder,
  search,
  isListView,
  isListViewTooltipVisible,
  isGridView,
  isGridRewardsView,
  intl,
}: Props) {
  const searchInput = useRef<{ inputElement: { current: HTMLInputElement } }>(
    null
  );

  const [isSearchInputFocused, setSearchInputFocused] = useState(false);

  const autoSelectOnFocus = () => {
    searchInput?.current
      ? searchInput.current.inputElement?.current?.select()
      : false;

    setSearchInputFocused(true);
  };

  const handleClearSearch = () => {
    onClearSearch();
    searchInput?.current?.inputElement.current.focus();
    setSearchInputFocused(true);
  };

  const hasSearchClearButton = () => {
    return search.length > 0;
  };

  const gridButtonClasses = classnames([
    styles.gridView,
    isGridView ? styles.selected : null,
  ]);
  const gridRewardsButtonClasses = classnames([
    styles.gridRewardsView,
    isGridRewardsView ? styles.selected : null,
  ]);
  const isBigSearchComponent = isListView || isGridView || isGridRewardsView;
  const searchInputClasses = classnames([
    styles.searchInput,
    isBigSearchComponent ? styles.inputExtrasSearch : null,
    IS_GRID_REWARDS_VIEW_AVAILABLE ? styles.withGridRewardsView : null,
  ]);
  const clearSearchClasses = classnames([
    styles.inputExtras,
    isBigSearchComponent ? styles.inputExtrasSearch : null,
    IS_GRID_REWARDS_VIEW_AVAILABLE ? styles.withGridRewardsView : null,
  ]);
  return (
    <StakingPageScrollContext.Consumer>
      {({ scrollElementRef }) => (
        <div className={styles.component}>
          <div className={styles.container}>
            <SVGInline
              svg={searchIcon}
              className={classnames(
                styles.searchIcon,
                isSearchInputFocused && styles.searchIconFocus
              )}
            />
            <Input
              autoFocus
              label={label || null}
              className={searchInputClasses}
              onChange={onSearch}
              ref={(input) => {
                searchInput.current = input;
              }}
              onFocus={autoSelectOnFocus}
              onBlur={() => setSearchInputFocused(false)}
              placeholder={
                placeholder ||
                intl.formatMessage(messages.searchInputPlaceholder)
              }
              skin={InputSkin}
              value={search}
              maxLength={150}
            />
            {hasSearchClearButton && (
              <div className={clearSearchClasses}>
                <PopOver content={intl.formatMessage(messages.clearTooltip)}>
                  <button
                    onClick={handleClearSearch}
                    className={styles.clearSearchButton}
                  >
                    <SVGInline
                      svg={closeIcon}
                      className={styles.clearSearchIcon}
                    />
                  </button>
                </PopOver>
              </div>
            )}
          </div>

          {isBigSearchComponent && (
            <div className={styles.viewButtons}>
              <PopOver
                content={intl.formatMessage(messages.gridIconTooltip)}
                appendTo={() => scrollElementRef.current}
                popperOptions={{
                  placement: 'top',
                  modifiers: [
                    {
                      name: 'flip',
                      options: {
                        fallbackPlacements: ['bottom'],
                      },
                    },
                  ],
                }}
              >
                <button className={gridButtonClasses} onClick={onGridView}>
                  <SVGInline svg={gridIcon} />
                </button>
              </PopOver>
              {IS_GRID_REWARDS_VIEW_AVAILABLE && (
                <PopOver
                  content={intl.formatMessage(messages.gridRewardsIconTooltip)}
                  appendTo={() => scrollElementRef.current}
                  popperOptions={{
                    placement: 'top',
                    modifiers: [
                      {
                        name: 'flip',
                        options: {
                          fallbackPlacements: ['bottom', 'left'],
                        },
                      },
                    ],
                  }}
                >
                  <button
                    className={gridRewardsButtonClasses}
                    onClick={onGridRewardsView}
                  >
                    <SVGInline svg={gridRewardsIcon} />
                  </button>
                </PopOver>
              )}
              <StakePoolsSearchListViewButton
                isListViewTooltipVisible={isListViewTooltipVisible}
                isListView={isListView}
                onClick={onListView}
                onListViewVisited={onListViewVisited}
                tooltipTarget={scrollElementRef.current}
              />
            </div>
          )}
        </div>
      )}
    </StakingPageScrollContext.Consumer>
  );
}

export const StakePoolsSearch = injectIntl(StakePoolsSearchComponent);
