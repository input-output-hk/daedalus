import React, { memo } from 'react';
// @ts-ignore ts-migrate(2305) FIXME: Module '"react"' has no exported member 'Node'.
import type { Node } from 'react';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './VerticalFlexContainer.scss' ... Remove this comment to see the full error message
import styles from './VerticalFlexContainer.scss';

type Props = {
  children?: Node | null | undefined;
};

const VerticalFlexContainer = ({ children }: Props) => (
  <div className={styles.component}>{children}</div>
);

export default memo<Props>(VerticalFlexContainer);
