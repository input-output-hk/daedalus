// @flow
import React from 'react';
import classNames from 'classnames';
import type { Node } from 'react';
import styles from './NotificationDot.scss';

type Props = {
  children: Node,
  className?: string,
  enabled: boolean,
  dotClassName?: string,
};

const NotificationDot = ({
  children,
  className,
  dotClassName,
  enabled = false,
}: Props) => {
  return (
    <div className={classNames(styles.root, className)}>
      {enabled && <span className={classNames(styles.dot, dotClassName)} />}
      {children}
    </div>
  );
};

export default NotificationDot;
