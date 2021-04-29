// @flow
import React, { Component } from 'react';
import type { Element } from 'react';
import { Select } from 'react-polymorph/lib/components/Select';
import { SelectSkin } from 'react-polymorph/lib/skins/simple/SelectSkin';
import { omit } from 'lodash';
import ItemsDropdownOption from './ItemsDropdownOption';
import styles from './ItemsDropdown.scss';
import type { ItemDropdown } from './ItemsDropdownOption';

type SelectProps = {
  allowBlank: boolean,
  autoFocus: boolean,
  className?: string,
  context: ThemeContextProp,
  error?: string | Element<any>,
  // hasSearch?: boolean, // In this case, `onSearch` is mandatory to activate the search, so `hasSearch` is removed
  hideSearchClearButton?: boolean,
  highlightSearch?: boolean,
  isOpeningUpward: boolean,
  label?: string | Element<any>,
  noResultsMessage?: string,
  onBlur?: Function,
  onChange?: Function,
  onFocus?: Function,
  onSearch?: Function,
  optionHeight?: number,
  optionRenderer?: Function,
  options: Array<any>,
  placeholder?: string,
  selectionRenderer?: Function,
  searchHeight?: number,
  skin?: ComponentType<any>,
  theme: ?Object, // will take precedence over theme in context if passed
  themeId: string,
  themeOverrides: Object,
  value: string,
};

type Props = {
  ...$Shape<SelectProps>,
  options: Array<ItemDropdown>,
  isSyncing?: boolean,
};

export default class ItemsDropdown extends Component<Props> {
  static defaultProps = {
    optionRenderer: (optionProps) => <ItemsDropdownOption {...optionProps} />,
    selectionRenderer: (optionProps) => (
      <ItemsDropdownOption selected {...optionProps} />
    ),
    skin: SelectSkin,
    errorPosition: 'top',
  };

  handleSearch;

  render() {
    const { error, errorPosition, onSearch, ...props } = this.props;
    let topError;
    let bottomError;
    if (errorPosition === 'bottom') bottomError = error;
    else topError = error;
    const selectOptions = omit({ ...props, topError }, 'hasSearch');
    return (
      <>
        <Select
          {...selectOptions}
          optionHeight={50}
          hasSearch={!!onSearch}
          onSearch={onSearch}
        />
        {bottomError && <div className={styles.error}>{bottomError}</div>}
      </>
    );
  }
}
