// @flow
import React, { memo } from 'react';
import type { Node } from 'react';
import styles from './VerticalFlexContainer.scss';

type Props = {
  children?: ?Node,
};

const VerticalFlexContainer = memo(({ children }: Props) => (
  <div className={styles.component}>{children}</div>
));
export default VerticalFlexContainer;
