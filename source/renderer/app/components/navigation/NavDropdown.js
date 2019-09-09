// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';

import { Dropdown } from 'react-polymorph/lib/components/Dropdown';

import NavButton from './NavButton';
import styles from './NavDropdown.scss';

type Props = {
  label: string,
  activeItem: string,
  icon?: string,
  isActive: boolean,
  options?: Array<{ value: number | string, label: string }>,
  onChange: Function,
};

@observer
export default class NavDropdown extends Component<Props> {
  render() {
    const { label, icon, isActive, onChange, options, activeItem } = this.props;
    return (
      <div className={styles.component}>
        <Dropdown
          label={
            <NavButton
              label={label}
              icon={icon}
              isActive={isActive}
              onClick={() => {}}
            />
          }
          onChange={({ value }) => onChange(value)}
          items={options}
          activeItem={activeItem}
          noArrow
        />
      </div>
    );
  }
}
