import React, { FC, ReactNode } from 'react';
import classNames from 'classnames';
import styles from './MonospaceTextBlock.scss';

export function MonospaceTextBlock({
  children,
  className,
}: {
  children: ReactNode;
  className?: string;
}) {
  return <pre className={classNames(styles.pre, className)}>{children}</pre>;
}
