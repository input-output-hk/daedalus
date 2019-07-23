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

const messages = defineMessages({
  searchInputPlaceholder: {
    id: 'staking.stakePools.search.searchInputPlaceholder',
    defaultMessage: '!!!Search stake pools',
    description: '"Delegating List Title" for the Stake Pools search.',
  },
  delegatingListTitle: {
    id: 'staking.stakePools.search.delegatingListTitle',
    defaultMessage: '!!!Stake pools you are currently delegating to',
    description: '"delegatingListTitlee" for the Stake Pools search.',
  },
  listTitle: {
    id: 'staking.stakePools.search.listTitle',
    defaultMessage: '!!!Stake pools ({pools})',
    description: '"listTitle" for the Stake Pools search.',
  },
});

export type Filters = Array<Filter>;
export type Filter = 'all' | 'charity' | 'new';

type Props = {
  filters?: Filters,
  label?: string,
  placeholder?: string,
  isClearTooltipOpeningDownward?: boolean,
  onSearch: Function,
  onClearSearch: Function,
  search: string,
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
      filters,
      onSearch,
      onClearSearch,
      placeholder,
      search,
      isClearTooltipOpeningDownward,
    } = this.props;

    const clearSearchStyles = classnames(styles.clearSearch, {
      [styles.clearSearchSeparator]: !!filters,
    });

    return (
      <div className={styles.component}>
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
            <div className={styles.inputExtras}>
              {this.hasSearchClearButton && (
                <Tooltip
                  skin={TooltipSkin}
                  tip="Clear"
                  className={clearSearchStyles}
                  isOpeningUpward={!isClearTooltipOpeningDownward}
                >
                  <button onClick={onClearSearch}>
                    <SVGInline svg={closeIcon} />
                  </button>
                </Tooltip>
              )}
            </div>
          )}
        </div>
      </div>
    );
  }
}
