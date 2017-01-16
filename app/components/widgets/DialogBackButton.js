import React, { Component, PropTypes } from 'react';
import backArrow from '../../assets/images/back-arrow-ic.svg';
import styles from './DialogBackButton.scss';

export default class DialogCloseButton extends Component {

  static propTypes = {
    onBack: PropTypes.func.isRequred
  };

  render() {
    const { onBack } = this.props;
    return (
      <button onClick={onBack} className={styles.component}>
        <img src={backArrow} role="presentation" />
      </button>
    );
  }
}
