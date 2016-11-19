// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import styles from './FileUploadWidget.scss';

@observer
export default class FileUploadWidget extends Component {

  render() {
    return (
      <div>
        <div className={styles.label}>Picture</div>
        <div className={styles.uploadBox}>
          <div className={styles.instructions}>
            <div className={styles.title}>Drop file here</div>
            <div className={styles.subtitle}>or click to upload</div>
          </div>
        </div>
      </div>
    );
  }

}
