// @flow
import React, { Component } from 'react';
import type { Children, Element } from 'react';
import { observer } from 'mobx-react';
import styles from './SidebarLayout.scss';

@observer
export default class SidebarLayout extends Component {

  static defaultProps = { children: null };

  props: {
    children: any | Children,
    sidebar: Element<any>,
    topbar: Element<any>,
    notification?: ?Element<any>,
    contentDialog?: ?Element<any>,
  };

  render() {
    const { children, sidebar, topbar, notification, contentDialog } = this.props;
    return (
      <div className={styles.component}>
        <div className={styles.sidebar}>
          {sidebar}
        </div>
        <div className={styles.main}>
          <div className={styles.topbar}>
            {topbar}
          </div>
          {notification}
          <div className={styles.contentWrapper}>
            <div className={styles.content}>
              {children}
            </div>
            {contentDialog}
          </div>
        </div>
      </div>
    );
  }
}
