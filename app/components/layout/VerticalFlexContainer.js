// @flow
import React, { Component } from 'react';
import type { Children } from 'react';
import { observer } from 'mobx-react';
import styles from './VerticalFlexContainer.scss';

@observer
export default class VerticalFlexContainer extends Component {

  props: {
    children?: ?Children,
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
