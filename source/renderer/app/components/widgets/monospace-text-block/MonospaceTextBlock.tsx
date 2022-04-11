import React, { FC } from 'react';
import classNames from 'classnames';
import styles from './MonospaceTextBlock.scss';

export const MonospaceTextBlock: FC<{ className?: string }> = ({
  children,
  className,
}) => <pre className={classNames(styles.pre, className)}>{children}</pre>;
