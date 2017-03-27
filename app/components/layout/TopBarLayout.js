// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import styles from './TopBarLayout.scss';
import { oneOrManyChildElements } from '../../propTypes';

@observer
export default class TopBarLayout extends Component {

  static propTypes = {
    children: oneOrManyChildElements,
    notification: PropTypes.element,
    topbar: PropTypes.element.isRequired,
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
