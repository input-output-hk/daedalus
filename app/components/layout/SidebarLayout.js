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
    appbar: PropTypes.element.isRequired,
  };

  render() {
    const { children, sidebar, appbar } = this.props;
    return (
      <div className={styles.component}>
        {sidebar}
        <div className={styles.main}>
          {appbar}
          {children}
        </div>
      </div>
    );
  }
}
