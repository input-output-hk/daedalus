// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import Dropzone from 'react-dropzone';
import attachIcon from '../../../assets/images/attach-ic.svg';
import styles from './FileUploadWidget.scss';

@observer
export default class FileUploadWidget extends Component {

  props: {
    label: string,
    hint: string,
    onFileSelected: Function,
    selectedFile: File,
    acceptedFileTypes: string,
  };

  onDrop = (files: [File]) => {
    this.props.onFileSelected(files[0]);
  };

  render() {
    const { label, hint, acceptedFileTypes, selectedFile } = this.props;
    return (
      <div className={styles.component}>
        <div className={styles.label}>{label}</div>
        <Dropzone
          className={styles.dropZone}
          onDrop={this.onDrop}
          multiple={false}
          accept={acceptedFileTypes}
        >
          { selectedFile ? (
            <div className={styles.fileName}>{selectedFile.name}</div>
          ) : (
            <div className={styles.hint}>{hint}</div>
          )}
          <img src={attachIcon} className={styles.attachIcon} role="presentation" />
        </Dropzone>
      </div>
    );
  }

}
