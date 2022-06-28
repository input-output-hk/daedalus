import React from 'react';
import classNames from 'classnames';
import styles from './Separator.scss';

export function Separator({ className }: { className?: string }) {
  return <hr className={classNames(styles.hr, className)} />;
}
