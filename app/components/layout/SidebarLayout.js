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
    notification: PropTypes.element,
    topbar: PropTypes.element.isRequired,
  };

  render() {
    const { children, sidebar, topbar, notification } = this.props;
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
          <div className={styles.content}>
            {children}
          </div>
        </div>
      </div>
    );
  }
}
