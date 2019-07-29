// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import SVGInline from 'react-svg-inline';
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
      async files => {
        if (!files) {
          return;
        }

        try {
          const filePath = files[0];
          const fileContent = await generateFileContent({ filePath });
          const {
            fileBuffer,
            fileLastModified,
            fileName,
            fileType,
          } = fileContent;
          const fileBlob = new Blob([fileBuffer]);
          const file = new File([fileBlob], fileName, {
            lastModified: fileLastModified,
            type: fileType,
          });
          this.props.onFileSelected(file);
        } catch (error) {} // eslint-disable-line
      }
    );
  };

  render() {
    const { label, placeholder, selectedFile } = this.props;
    return (
      <div className={styles.component}>
        <div className={styles.label}>{label}</div>
        <button className={styles.dropZone} onClick={this.onOpen}>
          {selectedFile ? (
            <div className={styles.fileName}>{selectedFile.name}</div>
          ) : (
            <div className={styles.placeholder}>{placeholder}</div>
          )}
          <SVGInline svg={attachIcon} className={styles.attachIcon} />
        </button>
      </div>
    );
  }
}
