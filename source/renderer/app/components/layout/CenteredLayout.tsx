import React, { Component } from 'react';
// @ts-ignore ts-migrate(2305) FIXME: Module '"react"' has no exported member 'Node'.
import type { Node } from 'react';
import { observer } from 'mobx-react';
import styles from './CenteredLayout.scss';

type Props = {
  children: Node;
};

class CenteredLayout extends Component<Props> {
  static defaultProps = {
    children: null,
  };

  render() {
    const { children } = this.props;
    return <div className={styles.component}>{children}</div>;
  }
}

export default observer(CenteredLayout);
