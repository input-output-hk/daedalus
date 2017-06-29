import React from 'react';
import InputSkin from 'react-polymorph/lib/skins/simple/InputSkin';
import styles from './AmountInputSkin.scss';

export default (props) => (
  <div className={styles.root}>
    <InputSkin {...props} />
    {!props.error && (
      <span className={styles.fees}>
        + {props.fees} of fees
      </span>
    )}
    <span className={styles.total}>
      {!props.error && `= ${props.total} `}{props.currency}
    </span>
  </div>
);
