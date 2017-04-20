import React, { Component } from 'react';
import backArrow from '../../assets/images/back-arrow-ic.svg';
import styles from './DialogBackButton.scss';

export default class DialogBackButton extends Component {

  props: {
    onBack: Function
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
