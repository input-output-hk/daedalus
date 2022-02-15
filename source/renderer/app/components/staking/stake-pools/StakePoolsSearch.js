import React, { useRef } from 'react';
import SVGInline from 'react-svg-inline';
import { injectIntl } from 'react-intl';
import { Input } from 'react-polymorph/lib/components/Input';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import classnames from 'classnames';
import styles from './StakePoolsSearch.scss';
import searchIcon from '../../../assets/images/search.inline.svg';
import closeIcon from '../../../assets/images/close-cross.inline.svg';
import gridIcon from '../../../assets/images/grid-ic.inline.svg';
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

  const autoSelectOnFocus = () =>
    searchInput?.current
      ? searchInput?.current?.inputElement.current.select()
      : false;

  const handleClearSearch = () => {
    onClearSearch();

    searchInput?.current?.inputElement.current.focus();
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
  const searchInputClases = classnames([
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
            <SVGInline svg={searchIcon} className={styles.searchIcon} />
            <Input
              autoFocus
              label={label || null}
              className={searchInputClases}
              onChange={onSearch}
              ref={(input) => {
                searchInput.current = input;
              }}
              placeholder={
                placeholder ||
                intl.formatMessage(messages.searchInputPlaceholder)
              }
              skin={InputSkin}
              value={search}
              maxLength={150}
              onFocus={autoSelectOnFocus}
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
