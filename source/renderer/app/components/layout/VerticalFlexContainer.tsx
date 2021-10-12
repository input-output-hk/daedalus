import React, { memo } from "react";
import type { Node } from "react";
import styles from "./VerticalFlexContainer.scss";
type Props = {
  children?: Node | null | undefined;
};

const VerticalFlexContainer = ({
  children
}: Props) => <div className={styles.component}>{children}</div>;

export default memo<Props>(VerticalFlexContainer);