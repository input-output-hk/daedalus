// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import SVGInline from 'react-svg-inline';
import Dropzone from 'react-dropzone';
import path from 'path';
import { generateFileContent } from '../../../utils/fileContentGenerator';
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

  test = () => {
    global.dialog.showOpenDialog(
      {
        filters: [
          {
            name: 'file-upload',
            extensions: this.props.acceptedFileTypes,
          },
        ],
      },
      async files => {
        if (!files) {
          return;
        }
        const filePath = files[0];
        let fileContent = null;
        try {
          fileContent = await generateFileContent({ filePath });
          const { fileBuffer, fileType } = fileContent;
          const fileBlob = new Blob([fileBuffer]);
          const file = new File([fileBlob], path.basename(filePath), {
            lastModified: new Date().getTime(),
            lastModifiedDate: new Date(),
            path: filePath,
            type: fileType.mime,
          });
          this.props.onFileSelected(file);
        } catch (error) {
          console.log(error);
        }
      }
    );
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
          <SVGInline svg={attachIcon} className={styles.attachIcon} />
        </Dropzone>
        <button onClick={this.test}>Test</button>
      </div>
    );
  }
}
