import React from 'react';
import classNames from 'classnames';
import styles from './Separator.scss';

export const Separator: React.FC<{ className?: string }> = ({ className }) => (
  <hr className={classNames(styles.hr, className)} />
);
