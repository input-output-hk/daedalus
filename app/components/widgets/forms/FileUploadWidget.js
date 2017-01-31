// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import Dropzone from 'react-dropzone';
import attachIcon from '../../../assets/images/attach-ic.svg';
import styles from './FileUploadWidget.scss';

@observer
export default class FileUploadWidget extends Component {

  static propTypes = {
    label: PropTypes.string.isRequired,
    hint: PropTypes.string.isRequired,
    onFileSelected: PropTypes.func.isRequired,
    acceptedFileTypes: PropTypes.string,
    value: PropTypes.instanceOf(File),
    error: PropTypes.string,
  };

  state = {
    selectedFile: null,
  };

  onDrop = (files) => {
    const selectedFile = files[0];
    this.setState({ selectedFile });
    this.props.onFileSelected(selectedFile);
  };

  render() {
    const { label, hint, acceptedFileTypes } = this.props;
    const { selectedFile } = this.state;
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
          <img src={attachIcon} className={styles.attachIcon} />
        </Dropzone>
      </div>
    );
  }

}
