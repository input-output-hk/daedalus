import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { Dropdown } from 'react-polymorph/lib/components/Dropdown';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './DropdownMenu.scss' or its co... Remove this comment to see the full error message
import styles from './DropdownMenu.scss';

type Props = {
  label: any;
  menuItems: Array<{
    value: number | string;
    label: any;
  }>;
  onMenuItemClick: (...args: Array<any>) => any;
};

@observer
class DropdownMenu extends Component<Props> {
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

export default DropdownMenu;
