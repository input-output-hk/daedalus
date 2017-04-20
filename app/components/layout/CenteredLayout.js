// @flow
import React, { Component } from 'react';
import type { Children } from 'react';
import { observer } from 'mobx-react';
import styles from './CenteredLayout.scss';

@observer
export default class CenteredLayout extends Component {

  static defaultProps = { children: null };

  props: {
    children: Children,
  };

  render() {
    const { children } = this.props;
    return (
      <div className={styles.component}>
        {children}
      </div>
    );
  }
}
