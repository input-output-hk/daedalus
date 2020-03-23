// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { includes } from 'lodash';
import { ITN_LEGACY_WALLET_EXCLUDED_NAV_ITEMS } from '../../config/walletNavigationConfig';
import styles from './Navigation.scss';
import NavButton from './NavButton';
import NavDropdown from './NavDropdown';

export type NavButtonProps = {
  type?: 'button',
  id: string,
  label: string,
  icon?: string,
  isLegacy?: boolean,
  hasNotification?: boolean,
};

export type NavDropdownProps = {
  ...$Exact<NavButtonProps>,
  type: 'dropdown',
  options: Array<{ value: number | string, label: string, isLegacy?: boolean }>,
  hasNotification?: boolean,
};

type Props = {
  activeItem: string,
  isActiveNavItem?: Function,
  onNavItemClick: Function,
  items: Array<NavButtonProps | NavDropdownProps>,
};

@observer
export default class Navigation extends Component<Props> {
  isActiveNavItem = (
    id: string,
    item: NavButtonProps | NavDropdownProps | {}
  ) => {
    let result = false;
    if (!item) {
      result = id === this.props.activeItem;
    }
    return result;
  };

  render() {
    const {
      isActiveNavItem = this.isActiveNavItem,
      onNavItemClick,
      activeItem,
      items,
    } = this.props;

    return (
      <div className={styles.component}>
        {items.map(
          ({ id, icon, label, isLegacy, hasNotification, ...item }) => {
            if (includes(ITN_LEGACY_WALLET_EXCLUDED_NAV_ITEMS, id) && isLegacy)
              return null;
            return item.type === 'dropdown'
              ? (!isLegacy && (
                  <NavDropdown
                    key={id}
                    label={label}
                    icon={icon}
                    isActive={isActiveNavItem(id, item)}
                    onChange={i => onNavItemClick(i)}
                    isLegacy={isLegacy}
                    activeItem={activeItem}
                    options={item.options}
                    hasNotification={hasNotification}
                  />
                )) ||
                  (isLegacy && (
                    <NavButton
                      key={id}
                      className={id}
                      label={label}
                      icon={icon}
                      isActive={isActiveNavItem(id, item)}
                      onClick={() => onNavItemClick(id)}
                      hasNotification={hasNotification}
                    />
                  ))
              : !isLegacy && (
                  <NavButton
                    key={id}
                    className={id}
                    label={label}
                    icon={icon}
                    isActive={isActiveNavItem(id, item)}
                    onClick={() => onNavItemClick(id)}
                    hasNotification={hasNotification}
                  />
                );
          }
        )}
      </div>
    );
  }
}
