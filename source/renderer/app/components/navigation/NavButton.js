// @flow
import React, { Component } from 'react';
import SVGInline from 'react-svg-inline';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import styles from './NavButton.scss';

export type NavButtonProps = {
  label: string,
  icon: string,
  isActive: boolean,
  onClick: Function,
  className?: string,
};

@observer
export default class NavButton extends Component<NavButtonProps> {
  render() {
    const { isActive, icon, onClick, className } = this.props;
    const componentClasses = classnames([
      className,
      styles.component,
      isActive ? styles.active : styles.normal,
    ]);
    const iconClasses = classnames([
      styles.icon,
      isActive ? styles.activeIcon : styles.normalIcon,
    ]);
    return (
      <button className={componentClasses} onClick={onClick}>
        <div className={styles.container}>
          <SVGInline svg={icon} className={iconClasses} />
          <span className={styles.label}>{this.props.label}</span>
        </div>
      </button>
    );
  }
}
