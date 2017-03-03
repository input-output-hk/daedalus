import React from 'react';
import { omit } from 'lodash';
import InputSkin from 'react-polymorph/lib/skins/simple/InputSkin';
import styles from './CurrencyInputSkin.scss';

export default (props) => (
  <div className={styles.component}>
    <div className={styles.currency}>{props.currency}</div>
    <InputSkin {...omit(props, ['currency'])} />
  </div>
);
