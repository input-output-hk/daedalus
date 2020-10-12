// @flow
import React, { Component } from 'react';
import SVGInline from 'react-svg-inline';
import { defineMessages, intlShape } from 'react-intl';
import { Input } from 'react-polymorph/lib/components/Input';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import { Tooltip } from 'react-polymorph/lib/components/Tooltip';
import { TooltipSkin } from 'react-polymorph/lib/skins/simple/TooltipSkin';
import classnames from 'classnames';
import styles from './StakePoolsSearch.scss';
import searchIcon from '../../../assets/images/search.inline.svg';
import closeIcon from '../../../assets/images/close-cross.inline.svg';
import gridIcon from '../../../assets/images/grid-ic.inline.svg';
import listIcon from '../../../assets/images/list-ic.inline.svg';

const messages = defineMessages({
  searchInputPlaceholder: {
    id: 'staking.stakePools.search.searchInputPlaceholder',
    defaultMessage: '!!!Search stake pools',
    description: '"Delegating List Title" for the Stake Pools search.',
  },
  delegatingListTitle: {
    id: 'staking.stakePools.search.delegatingListTitle',
    defaultMessage: '!!!Staking pools you are delegating to',
    description: '"delegatingListTitlee" for the Stake Pools search.',
  },
  listTitle: {
    id: 'staking.stakePools.search.listTitle',
    defaultMessage: '!!!Stake pools ({pools})',
    description: '"listTitle" for the Stake Pools search.',
  },
});

type Props = {
  label?: string,
  placeholder?: string,
  isClearTooltipOpeningDownward?: boolean,
  isListView?: boolean,
  isGridView?: boolean,
  onSearch: Function,
  onClearSearch: Function,
  onGridView?: Function,
  onListView?: Function,
  search: string,
  isFixed?: boolean,
  isScrolled?: boolean,
};

export class StakePoolsSearch extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  searchInput: ?Object = null;

  autoSelectOnFocus = () =>
    this.searchInput ? this.searchInput.inputElement.current.select() : false;

  get hasSearchClearButton() {
    return this.props.search.length > 0;
  }

  render() {
    const { intl } = this.context;
    const {
      label,
      onSearch,
      onClearSearch,
      onGridView,
      onListView,
      placeholder,
      search,
      isListView,
      isGridView,
      isClearTooltipOpeningDownward,
      isFixed,
      isScrolled,
    } = this.props;

    let componentClasses: string = '';
    if (isScrolled) {
      componentClasses = classnames([
        styles.component,
        isListView && isScrolled ? styles.componentFixedPosition : null,
      ]);
    } else {
      componentClasses = classnames([
        styles.component,
        isScrolled && (isListView && isFixed) ? styles.componentFixedPosition : null,
      ]);
    }

    const gridButtonClasses = classnames([
      styles.gridView,
      isGridView ? styles.selected : null,
    ]);

    const listButtonClasses = classnames([
      styles.listView,
      isListView ? styles.selected : null,
    ]);

    const isBigSearchComponent = isListView || isGridView;

    const clearSearchClasses = classnames([
      styles.inputExtras,
      isBigSearchComponent ? styles.inputExtrasSearch : null,
    ]);

    return (
      <div className={componentClasses}>
        <div className={styles.container}>
          <SVGInline svg={searchIcon} className={styles.searchIcon} />
          <Input
            autoFocus
            label={label || null}
            className={styles.searchInput}
            onChange={onSearch}
            ref={input => {
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
              {this.hasSearchClearButton && (
                <Tooltip
                  skin={TooltipSkin}
                  tip="Clear"
                  className={styles.clearSearch}
                  isOpeningUpward={!isClearTooltipOpeningDownward}
                >
                  <button onClick={onClearSearch}>
                    <SVGInline svg={closeIcon} />
                  </button>
                </Tooltip>
              )}
            </div>
          )}
          {isBigSearchComponent && (<div className={styles.viewButtons}>
            <span className={styles.separator}>|</span>
            <button className={gridButtonClasses} onClick={onGridView}>
              <SVGInline svg={gridIcon} />
            </button>
            <button className={listButtonClasses} onClick={onListView}>
              <SVGInline svg={listIcon} />
            </button>
          </div>)}
        </div>
      </div>
    );
  }
}
