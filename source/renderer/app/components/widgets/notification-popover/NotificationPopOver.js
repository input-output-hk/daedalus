// @flow
import React from 'react';
import classNames from 'classnames';
import type { Node } from 'react';
import { Button } from 'react-polymorph/lib/components/Button';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import styles from './NotificationPopOver.scss';

type Props = {
  children: Node,
  className?: string,
  content: Node,
  dismissLabel: string,
  inline?: boolean,
  onDismiss?: Function,
  offset?: [number, number],
};

const NotificationPopOver = ({
  children,
  className,
  content,
  dismissLabel,
  inline = false,
  onDismiss = () => {},
  offset,
  ...props
}: Props) => {
  return (
    <PopOver
      {...props}
      className={classNames(styles.popOverRoot, className)}
      content={
        <div className={styles.content}>
          {content}
          <div className={styles.contentFooter}>
            <Button
              label={dismissLabel}
              className={styles.dismissButton}
              onClick={onDismiss}
            />
          </div>
        </div>
      }
      popperOptions={{
        modifiers: [
          ...(offset
            ? [
                {
                  name: 'offset',
                  options: {
                    offset,
                  },
                },
              ]
            : []),
        ],
      }}
    >
      <div className={classNames(styles.targetRoot, inline && styles.inline)}>
        {children}
      </div>
    </PopOver>
  );
};

export default NotificationPopOver;
