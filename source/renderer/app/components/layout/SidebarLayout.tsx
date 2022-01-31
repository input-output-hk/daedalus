import React, { Component } from 'react';
// @ts-ignore ts-migrate(2305) FIXME: Module '"react"' has no exported member 'Node'.
import type { Node } from 'react';
import { observer } from 'mobx-react';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './SidebarLayout.scss' or its c... Remove this comment to see the full error message
import styles from './SidebarLayout.scss';

type Props = {
  children: any | Node;
  sidebar: Node;
  topbar: Node;
  notification?: Node | null | undefined;
  contentDialogs?: Array<Node> | null | undefined;
};

@observer
class SidebarLayout extends Component<Props> {
  static defaultProps = {
    children: null,
  };

  render() {
    const {
      children,
      sidebar,
      topbar,
      notification,
      contentDialogs,
    } = this.props;
    return (
      <div className={styles.component}>
        <div className={styles.sidebar}>{sidebar}</div>
        <div className={styles.main}>
          <div className={styles.topbar}>{topbar}</div>
          {notification}
          <div className={styles.contentWrapper}>
            <div className={styles.content}>{children}</div>
            {contentDialogs}
          </div>
        </div>
      </div>
    );
  }
}

export default SidebarLayout;
