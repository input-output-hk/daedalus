// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import SvgInline from 'react-svg-inline';
import Dropzone from 'react-dropzone';
import attachIcon from '../../../assets/images/attach-ic.inline.svg';
import styles from './FileUploadWidget.scss';

type Props = {
  label: string,
  placeholder: string,
  onFileSelected: Function,
  selectedFile: File,
  acceptedFileTypes: string,
};

@observer
export default class FileUploadWidget extends Component<Props> {

  onDrop = (files: [File]) => {
    this.props.onFileSelected(files[0]);
  };

  render() {
    const { label, placeholder, acceptedFileTypes, selectedFile } = this.props;
    return (
      <div className={styles.component}>
        <div className={styles.label}>{label}</div>
        <Dropzone
          className={styles.dropZone}
          onDrop={this.onDrop}
          multiple={false}
          accept={acceptedFileTypes}
        >
          {selectedFile ? (
            <div className={styles.fileName}>{selectedFile.name}</div>
          ) : (
            <div className={styles.placeholder}>{placeholder}</div>
          )}
          <SvgInline svg={attachIcon} className={styles.attachIcon} />
        </Dropzone>
      </div>
    );
  }

}
