import React, { Component } from 'react';
import SVGInline from 'react-svg-inline';
import { defineMessages, intlShape } from 'react-intl';
import { Input } from 'react-polymorph/lib/components/Input';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import classnames from 'classnames';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './StakePoolsSearch.scss' or it... Remove this comment to see the full error message
import styles from './StakePoolsSearch.scss';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/search.... Remove this comment to see the full error message
import searchIcon from '../../../assets/images/search.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/close-c... Remove this comment to see the full error message
import closeIcon from '../../../assets/images/close-cross.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/grid-ic... Remove this comment to see the full error message
import gridIcon from '../../../assets/images/grid-ic.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/grid-re... Remove this comment to see the full error message
import gridRewardsIcon from '../../../assets/images/grid-rewards.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/list-ic... Remove this comment to see the full error message
import listIcon from '../../../assets/images/list-ic.inline.svg';
import { IS_GRID_REWARDS_VIEW_AVAILABLE } from '../../../config/stakingConfig';

const messages = defineMessages({
  searchInputPlaceholder: {
    id: 'staking.stakePools.search.searchInputPlaceholder',
    defaultMessage: '!!!Search stake pools',
    description: '"Delegating List Title" for the Stake Pools search.',
  },
  delegatingListTitle: {
    id: 'staking.stakePools.search.delegatingListTitle',
    defaultMessage: '!!!Stake pools to which you are delegating',
    description: '"delegatingListTitle" for the Stake Pools search.',
  },
  listTitle: {
    id: 'staking.stakePools.search.listTitle',
    defaultMessage: '!!!Stake pools ({pools})',
    description: '"listTitle" for the Stake Pools search.',
  },
  gridIconTooltip: {
    id: 'staking.stakePools.search.gridIconTooltip',
    defaultMessage: '!!!Grid View',
    description: '"gridIconTooltip" for the Stake Pools search.',
  },
  gridRewardsIconTooltip: {
    id: 'staking.stakePools.search.gridRewardsIconTooltip',
    defaultMessage: '!!!Grid Rewards View',
    description: '"gridRewardsIconTooltip" for the Stake Pools search.',
  },
  listIconTooltip: {
    id: 'staking.stakePools.search.listIconTooltip',
    defaultMessage: '!!!List View',
    description: '"listIconTooltip" for the Stake Pools search.',
  },
  clearTooltip: {
    id: 'staking.stakePools.search.clearTooltip',
    defaultMessage: '!!!Clear',
    description: '"clearTooltip" for the Stake Pools search.',
  },
});
type Props = {
  label?: string;
  placeholder?: string;
  isListView?: boolean;
  isGridView?: boolean;
  isGridRewardsView?: boolean;
  onSearch: (...args: Array<any>) => any;
  onClearSearch: (...args: Array<any>) => any;
  onGridView?: (...args: Array<any>) => any;
  onGridRewardsView?: (...args: Array<any>) => any;
  onListView?: (...args: Array<any>) => any;
  search: string;
};
export class StakePoolsSearch extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };
  searchInput: Record<string, any> | null | undefined = null;
  autoSelectOnFocus = () =>
    this.searchInput ? this.searchInput.inputElement.current.select() : false;

  get hasSearchClearButton() {
    return this.props.search.length > 0;
  }

  handleClearSearch = () => {
    this.props.onClearSearch();

    if (this.searchInput) {
      this.searchInput.focus();
    }
  };

  render() {
    const { intl } = this.context;
    const {
      label,
      onSearch,
      onGridView,
      onGridRewardsView,
      onListView,
      placeholder,
      search,
      isListView,
      isGridView,
      isGridRewardsView,
    } = this.props;
    const gridButtonClasses = classnames([
      styles.gridView,
      isGridView ? styles.selected : null,
    ]);
    const gridRewardsButtonClasses = classnames([
      styles.gridRewardsView,
      isGridRewardsView ? styles.selected : null,
    ]);
    const listButtonClasses = classnames([
      styles.listView,
      isListView ? styles.selected : null,
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
      <div className={styles.component}>
        <div className={styles.container}>
          <SVGInline svg={searchIcon} className={styles.searchIcon} />
          <Input
            autoFocus
            label={label || null}
            className={searchInputClases}
            onChange={onSearch}
            ref={(input) => {
              this.searchInput = input;
            }}
            placeholder={
              placeholder || intl.formatMessage(messages.searchInputPlaceholder)
            }
            skin={InputSkin}
            value={search}
            maxLength={150}
            onFocus={this.autoSelectOnFocus}
          />
          {this.hasSearchClearButton && (
            <div className={clearSearchClasses}>
              <PopOver content={intl.formatMessage(messages.clearTooltip)}>
                <button
                  onClick={this.handleClearSearch}
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
          {isBigSearchComponent && (
            <div className={styles.viewButtons}>
              <span className={styles.separator}>|</span>
              <PopOver content={intl.formatMessage(messages.gridIconTooltip)}>
                <button className={gridButtonClasses} onClick={onGridView}>
                  <SVGInline svg={gridIcon} />
                </button>
              </PopOver>
              {IS_GRID_REWARDS_VIEW_AVAILABLE && (
                <PopOver
                  content={intl.formatMessage(messages.gridRewardsIconTooltip)}
                >
                  <button
                    className={gridRewardsButtonClasses}
                    onClick={onGridRewardsView}
                  >
                    <SVGInline svg={gridRewardsIcon} />
                  </button>
                </PopOver>
              )}
              <PopOver content={intl.formatMessage(messages.listIconTooltip)}>
                <button className={listButtonClasses} onClick={onListView}>
                  <SVGInline svg={listIcon} />
                </button>
              </PopOver>
            </div>
          )}
        </div>
      </div>
    );
  }
}
