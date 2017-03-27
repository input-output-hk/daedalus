import React, { Component, PropTypes } from 'react';
import closeCross from '../../assets/images/close-cross.svg';
import styles from './DialogCloseButton.scss';

export default class DialogCloseButton extends Component {

  static propTypes = {
    onClose: PropTypes.func.isRequired,
    icon: PropTypes.string,
  };

  render() {
    const { onClose, icon } = this.props;
    return (
      <button onClick={onClose} className={styles.component}>
        <img src={icon || closeCross} role="presentation" />
      </button>
    );
  }
}
