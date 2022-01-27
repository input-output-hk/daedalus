import React, { Component } from 'react';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './LoadingOverlay.scss' or its ... Remove this comment to see the full error message
import styles from './LoadingOverlay.scss';

type Props = {};
export default class LoadingOverlay extends Component<Props> {
  render() {
    return <div className={styles.component} />;
  }
}
