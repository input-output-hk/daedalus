// @flow
import React from 'react';
import { observer } from 'mobx-react';
import styles from './Ellipsis.scss';

type Props = {
  string: string,
  minCharsEnd?: number,
};

const Ellipsis = observer(({ string, minCharsEnd = 10 }: Props) => {
  const splitPosition = string.length - minCharsEnd;
  const initText = string.substr(0, splitPosition);
  const endText = string.substr(splitPosition);
  return (
    <span className={styles.component}>
      <span className={styles.initText}>{initText}</span>
      <span className={styles.endText}>{endText}</span>
    </span>
  );
});

export default Ellipsis;
