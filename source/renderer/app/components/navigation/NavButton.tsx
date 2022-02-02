import React, { Component } from 'react';
import SVGInline from 'react-svg-inline';
import { observer } from 'mobx-react';
import classnames from 'classnames';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './NavButton.scss' or its corre... Remove this comment to see the full error message
import styles from './NavButton.scss';

type Props = {
  label: string;
  icon?: string;
  isActive: boolean;
  onClick: (...args: Array<any>) => any;
  className?: string;
  hasNotification?: boolean;
};

@observer
class NavButton extends Component<Props> {
  render() {
    const { isActive, icon, onClick, className, hasNotification } = this.props;
    const componentClasses = classnames([
      className,
      styles.component,
      isActive ? styles.active : styles.normal,
      hasNotification ? styles.hasNotification : null,
    ]);
    const iconClasses = classnames([
      styles.icon,
      isActive ? styles.activeIcon : styles.normalIcon,
    ]);
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

export default NavButton;
