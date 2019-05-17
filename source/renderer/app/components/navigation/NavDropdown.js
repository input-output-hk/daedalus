// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';

import { Select } from 'react-polymorph/lib/components/Select';
import { SelectSkin } from './NavSelectSkin';
import selectStyles from './NavSelectStyles.scss';

import NavButton from './NavButton';
import styles from './NavDropdown.scss';

export type NavDropdownProps = {
  label: string,
  activeItem: string,
  icon: string,
  isActive: boolean,
  options: Array<{ value: number | string, label: string }>,
  onChange: Function,
};

@observer
export default class NavDropdown extends Component<NavDropdownProps> {
  render() {
    const { label, icon, isActive, onChange, options, activeItem } = this.props;
    return (
      <div className={styles.component}>
        <Select
          label={
            <NavButton
              label={label}
              icon={icon}
              isActive={isActive}
              onClick={() => {}}
            />
          }
          onChange={({ value }) => onChange(value)}
          options={options}
          skin={SelectSkin}
          themeOverrides={selectStyles}
          value={activeItem}
        />
      </div>
    );
  }
}
