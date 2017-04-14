// @flow
import React, { Component } from 'react';
import type { Children } from 'react';
import { observer } from 'mobx-react';
import classNames from 'classnames';
import styles from './SidebarMenu.scss';

@observer
export default class SidebarMenu extends Component {

  props: {
    children?: Children,
    visible: boolean,
  };

  render() {
    const { children, visible } = this.props;
    const componentStyles = classNames([
      styles.component,
      visible ? styles.visible : null
    ]);
    return (
      <div className={componentStyles}>
        {children}
      </div>
    );
  }

}
