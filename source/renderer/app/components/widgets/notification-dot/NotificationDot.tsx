import React from 'react';
import classNames from 'classnames';
// @ts-ignore ts-migrate(2305) FIXME: Module '"react"' has no exported member 'Node'.
import type { Node } from 'react';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './NotificationDot.scss' or its... Remove this comment to see the full error message
import styles from './NotificationDot.scss';

type Props = {
  children: Node;
  className?: string;
  enabled: boolean;
  dotClassName?: string;
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
