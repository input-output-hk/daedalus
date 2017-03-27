// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import styles from './SidebarLayout.scss';
import { oneOrManyChildElements } from '../../propTypes';

@observer
export default class SidebarLayout extends Component {

  static propTypes = {
    children: oneOrManyChildElements,
    sidebar: PropTypes.element.isRequired,
    topbar: PropTypes.element.isRequired,
    notification: PropTypes.element,
    contentDialog: PropTypes.element,
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
