// @flow
import React, { Component } from 'react';
import SvgInline from 'react-svg-inline';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import styles from './WalletNavButton.scss';

type Props = {
  label: string,
  icon: string,
  isActive: boolean,
  onClick: Function,
  className?: string,
};

@observer
export default class WalletNavButton extends Component<Props> {

  render() {
    const { isActive, icon, onClick, className } = this.props;
    const componentClasses = classnames([
      className,
      styles.component,
      isActive ? styles.active : styles.normal
    ]);
    const iconClasses = classnames([
      styles.icon,
      isActive ? styles.activeIcon : styles.normalIcon
    ]);
    return (
      <button className={componentClasses} onClick={onClick}>
        <div className={styles.container}>
          <SvgInline svg={icon} className={iconClasses} />
          <span className={styles.label}>{this.props.label}</span>
        </div>
      </button>
    );
  }
}
