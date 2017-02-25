import React from 'react';
import TextInputSkin from './TextInputSkin';
import styles from './CurrencyTextInputSkin.scss';

export default (props) => (
  <div className={styles.component}>
    <div className={styles.currency}>{props.currency}</div>
    <TextInputSkin {...props} />
  </div>
);
