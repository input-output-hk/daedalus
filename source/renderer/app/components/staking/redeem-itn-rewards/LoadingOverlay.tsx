// @flow
import React, { Component } from 'react';
import styles from './LoadingOverlay.scss';

type Props = {};
export default class LoadingOverlay extends Component<Props> {
  render() {
    return <div className={styles.component} />;
  }
}
