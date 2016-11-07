// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import classNames from 'classnames';
import styles from './SidebarSubMenu.scss';

@observer
export default class SidebarSubMenu extends Component {

  static propTypes = {
    children: PropTypes.oneOfType([
      PropTypes.arrayOf(PropTypes.element),
      PropTypes.element
    ]),
    visible: PropTypes.bool
  };

  render() {
    const { children, visible } = this.props;
    const componentStyles = classNames([
      styles.component,
      visible ? styles.visible : null
    ]);
    return (
      <div className={componentStyles}>
        {children}
      </div>
    );
  }

}
