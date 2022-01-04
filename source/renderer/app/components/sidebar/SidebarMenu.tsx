import React, { Component } from 'react';
// @ts-ignore ts-migrate(2305) FIXME: Module '"react"' has no exported member 'Node'.
import type { Node } from 'react';
import { observer } from 'mobx-react';
import classNames from 'classnames';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './SidebarMenu.scss' or its cor... Remove this comment to see the full error message
import styles from './SidebarMenu.scss';

type Props = {
  children: Node | null | undefined;
  visible: boolean;
};

@observer
class SidebarMenu extends Component<Props> {
  render() {
    const { children, visible } = this.props;
    const componentStyles = classNames([
      styles.component,
      visible ? styles.visible : null,
    ]);
    return <div className={componentStyles}>{children}</div>;
  }
}

export default SidebarMenu;
