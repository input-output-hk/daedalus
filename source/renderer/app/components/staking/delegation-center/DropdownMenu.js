// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { Dropdown } from 'react-polymorph/lib/components/Dropdown';
import styles from './DropdownMenu.scss';

type Props = {
  label: any,
  menuItems: Array<{ value: number | string, label: any }>,
  onMenuItemClick: Function,
};

@observer
export default class DropdownMenu extends Component<Props> {
  optionRenderer = (option: any) => (
    <span className={option.className}>{option.label}</span>
  );

  selectionRenderer = () => (
    <div className={styles.dropdownToggle}>{this.props.label}</div>
  );

  render() {
    const { menuItems, onMenuItemClick } = this.props;

    return (
      <div className={styles.component}>
        <Dropdown
          label={this.selectionRenderer()}
          onItemSelected={onMenuItemClick}
          optionRenderer={this.optionRenderer}
          items={menuItems}
          activeItem={menuItems[0]}
          clickToOpen
          noArrow
        />
      </div>
    );
  }
}
