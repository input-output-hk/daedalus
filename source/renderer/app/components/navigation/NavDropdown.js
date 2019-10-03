// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';

import { Select } from 'react-polymorph/lib/components/Select';
import { SelectSkin } from './NavSelectSkin';
import selectStyles from './NavSelectStyles.scss';

import NavButton from './NavButton';
import styles from './NavDropdown.scss';

type Props = {
  label: string,
  activeItem: string,
  icon?: string,
  isActive: boolean,
  options?: Array<{ value: number | string, label: string }>,
  onChange: Function,
  hasNotification?: boolean,
};

@observer
export default class NavDropdown extends Component<Props> {
  render() {
    const {
      label,
      icon,
      isActive,
      onChange,
      options,
      activeItem,
      hasNotification,
    } = this.props;
    const componentStyles = classnames([
      styles.component,
      hasNotification ? styles.hasNotification : null,
    ]);
    return (
      <div className={componentStyles}>
        <Select
          label={
            <NavButton
              label={label}
              icon={icon}
              isActive={isActive}
              onClick={() => {}}
              hasNotification={hasNotification}
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
