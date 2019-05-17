// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import styles from './Navigation.scss';
import NavButton from './NavButton';
import NavDropdown from './NavDropdown';
// import summaryIcon from '../../../assets/images/-nav/summary-ic.inline.svg';

// import type { NavButtonProps } from './NavButton';
// import type { NavDropdownProps } from './NavDropdown';

type NavButtonProps = {
  id: string,
  label: string,
  icon: string,
};

type NavDropdownProps = {
  ...$Exact<NavButtonProps>,
  options: Array<{ value: number | string, label: string }>,
};

type Props = {
  activeItem: string,
  isActiveNavItem?: Function,
  onNavItemClick: Function,
  items: Array<{
    type?: 'button' | 'dropdown',
    id: string,
    ...$Exact<NavButtonProps | NavDropdownProps>,
  }>,
};

@observer
export default class Navigation extends Component<Props> {
  isActiveNavItem = (id: string) => id === this.props.activeItem;

  render() {
    const {
      isActiveNavItem = this.isActiveNavItem,
      onNavItemClick,
      activeItem,
      items,
    } = this.props;
    return (
      <div className={styles.component}>
        {items.map(({ type, id, icon, label, options }) =>
          type === 'dropdown' ? (
            <NavDropdown
              label={label}
              icon={icon}
              isActive={isActiveNavItem(id)}
              onChange={item => onNavItemClick(item)}
              activeItem={activeItem}
              options={options}
            />
          ) : (
            <NavButton
              className={id}
              label={label}
              icon={icon}
              isActive={isActiveNavItem(id)}
              onClick={() => onNavItemClick(id)}
            />
          )
        )}
      </div>
    );
  }
}
