import React, { memo } from 'react';
// @ts-ignore ts-migrate(2305) FIXME: Module '"react"' has no exported member 'Node'.
import type { Node } from 'react';
import styles from './VerticalFlexContainer.scss';

type Props = {
  children?: Node | null | undefined;
};

function VerticalFlexContainer({ children }: Props) {
  return <div className={styles.component}>{children}</div>;
}

export default memo<Props>(VerticalFlexContainer);
