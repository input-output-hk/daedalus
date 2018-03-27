// @flow
import React, { Component } from 'react';
import styles from './LoadingSpinner.scss';

export default class LoadingSpinner extends Component<any> {

  root: ?HTMLElement;

  render() {
    return <div className={styles.component} ref={(div) => { this.root = div; }} />;
  }
}
