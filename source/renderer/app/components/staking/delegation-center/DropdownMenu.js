// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { Select } from 'react-polymorph/lib/components/Select';
import { SelectSkin } from 'react-polymorph/lib/skins/simple/SelectSkin';
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
        <Select
          allowBlank={false}
          onChange={onMenuItemClick}
          options={menuItems}
          skin={SelectSkin}
          optionRenderer={this.optionRenderer}
          optionsMaxHeight={null}
          selectionRenderer={this.selectionRenderer}
        />
      </div>
    );
  }
}
