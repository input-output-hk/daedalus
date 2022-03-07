import React, { Component } from 'react';
import SVGInline from 'react-svg-inline';
import classnames from 'classnames';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './BigButtonForDialogs.scss' or... Remove this comment to see the full error message
import styles from './BigButtonForDialogs.scss';

type Props = {
  label: string;
  description: string;
  icon: string;
  onClick?: (...args: Array<any>) => any;
  isDisabled?: boolean;
  className?: string;
};
export default class BigButtonForDialogs extends Component<Props> {
  render() {
    const {
      label,
      description,
      icon,
      onClick,
      isDisabled = false,
      className,
    } = this.props;
    const componentClasses = classnames([
      className,
      styles.component,
      isDisabled ? styles.disabled : null,
    ]);
    return (
      <button
        className={componentClasses}
        onClick={() => !isDisabled && onClick && onClick()}
      >
        <SVGInline svg={icon} className={styles.icon} />
        <div className={styles.label}>{label}</div>
        <div className={styles.description}>{description}</div>
      </button>
    );
  }
}
