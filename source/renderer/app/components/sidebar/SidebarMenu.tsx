import React, { Component } from 'react';
import type { Node } from 'react';
import { observer } from 'mobx-react';
import classNames from 'classnames';
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
