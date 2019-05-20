// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import styles from './Navigation.scss';
import NavButton from './NavButton';
import NavDropdown from './NavDropdown';

type NavButtonProps = {
  type?: 'button',
  id: string,
  label: string,
  icon?: string,
};

type NavDropdownProps = {
  ...$Exact<NavButtonProps>,
  type: 'dropdown',
  options: Array<{ value: number | string, label: string }>,
};

type Props = {
  activeItem: string,
  isActiveNavItem?: Function,
  onNavItemClick: Function,
  items: Array<NavButtonProps | NavDropdownProps>,
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
        {items.map(({ id, icon, label, ...item }) =>
          item.type === 'dropdown' ? (
            <NavDropdown
              key={id}
              label={label}
              icon={icon}
              isActive={isActiveNavItem(id)}
              onChange={i => onNavItemClick(i)}
              activeItem={activeItem}
              options={item.options}
            />
          ) : (
            <NavButton
              key={id}
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
