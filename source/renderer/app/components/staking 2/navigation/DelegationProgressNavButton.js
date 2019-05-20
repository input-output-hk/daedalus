// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import styles from './DelegationProgressNavButton.scss';

type Props = {
  label: string,
  isActive: boolean,
  onClick: Function,
  className?: string,
};

@observer
export default class DelegationProgressNavButton extends Component<Props> {
  render() {
    const { label, isActive, onClick, className } = this.props;
    const componentClasses = classnames([
      className,
      styles.component,
      isActive ? styles.active : styles.normal,
    ]);
    return (
      <button className={componentClasses} onClick={onClick}>
        <div className={styles.container}>
          <span className={styles.label}>{label}</span>
        </div>
      </button>
    );
  }
}
