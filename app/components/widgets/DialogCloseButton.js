import React, { Component, PropTypes } from 'react';
import closeCross from '../../assets/images/close-cross.svg';
import styles from './DialogCloseButton.scss';

export default class DialogCloseButton extends Component {

  static propTypes = {
    onClose: PropTypes.func.isRequred
  };

  render() {
    const { onClose } = this.props;
    return (
      <button onClick={onClose} className={styles.component}>
        <img src={closeCross} role="presentation" />
      </button>
    );
  }
}
