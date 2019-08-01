// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import SVGInline from 'react-svg-inline';
import path from 'path';
import attachIcon from '../../../assets/images/attach-ic.inline.svg';
import styles from './FileUploadWidget.scss';

type Props = {
  label: string,
  placeholder: string,
  onFileSelected: Function,
  selectedFile: string,
  acceptedFileTypes: [string],
};

@observer
export default class FileUploadWidget extends Component<Props> {
  onOpen = () => {
    global.dialog.showOpenDialog(
      {
        filters: [
          {
            name: 'file-upload',
            extensions: this.props.acceptedFileTypes,
          },
        ],
        properties: ['openFile'],
      },
      files => {
        if (!files) {
          return;
        }

        const filePath = files[0];
        this.props.onFileSelected(filePath);
      }
    );
  };

  onDragOver = () => false;
  onDragLeave = () => false;
  onDragEnd = () => false;
  onDrop = (e: any) => {
    e.preventDefault();

    const { files } = e.dataTransfer;
    if (!files || files.length === 0) {
      return;
    }

    const filePath = files[0].path;
    this.props.onFileSelected(filePath);
  };

  render() {
    const { label, placeholder, selectedFile } = this.props;
    const fileName = path.basename(selectedFile);

    return (
      <div className={styles.component}>
        <div className={styles.label}>{label}</div>
        <button
          className={styles.dropZone}
          onClick={this.onOpen}
          onDragOver={this.onDragOver}
          onDragLeave={this.onDragLeave}
          onDragEnd={this.onDragEnd}
          onDrop={this.onDrop}
        >
          {selectedFile ? (
            <div className={styles.fileName}>{fileName}</div>
          ) : (
            <div className={styles.placeholder}>{placeholder}</div>
          )}
          <SVGInline svg={attachIcon} className={styles.attachIcon} />
        </button>
      </div>
    );
  }
}
