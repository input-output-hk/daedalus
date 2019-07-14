// @flow
import React, { Component } from 'react';
import SVGInline from 'react-svg-inline';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import styles from './NavButton.scss';

type Props = {
  label: string,
  icon?: string,
  isActive: boolean,
  onClick: Function,
  className?: string,
};

@observer
export default class NavButton extends Component<Props> {
  render() {
    const { isActive, icon, onClick, className } = this.props;
    const componentClasses = classnames(className, styles.component, {
      [styles.active]: isActive,
      [styles.normal]: !isActive,
    });
    const iconClasses = classnames(styles.icon, {
      [styles.activeIcon]: isActive,
      [styles.normalIcon]: !isActive,
    });
    return (
      <button className={componentClasses} onClick={onClick}>
        <div className={styles.container}>
          {icon && <SVGInline svg={icon} className={iconClasses} />}
          <span className={styles.label}>{this.props.label}</span>
        </div>
      </button>
    );
  }
}
