// @flow
import React, { Component } from 'react';
import type { Children, Element } from 'react';
import { observer } from 'mobx-react';
import styles from './TopBarLayout.scss';

@observer
export default class TopBarLayout extends Component {

  props: {
    topbar: Element<any>,
    children?: ?Children,
    notification?: ?Element<any>,
  };

  render() {
    const { children, topbar, notification } = this.props;
    return (
      <div className={styles.component}>
        <div className={styles.topbar}>
          {topbar}
        </div>
        {notification}
        <div className={styles.content}>
          {children}
        </div>
      </div>
    );
  }
}
