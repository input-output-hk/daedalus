// @flow
import classNames from 'classnames';
import { observer } from 'mobx-react';
import React, { Component } from 'react';
import styles from './SidebarWalletMenuItem.scss';

type Props = {
  total: string,
  className: string,
};

@observer
export default class SidebarWalletMenuItemTotal extends Component<Props> {
  render() {
    const { total, className } = this.props;

    const componentStyles = classNames([
      styles.component,
      styles.walletTotal,
      className,
    ]);

    return (
      <div className={componentStyles} title={`${total} ADA`}>
        <div className={styles.meta}>
          <div className={styles.topContainer}>
            <div className={styles.title}>{total}</div>
          </div>
          <div className={styles.info}>Total ADA</div>
        </div>
      </div>
    );
  }
}
