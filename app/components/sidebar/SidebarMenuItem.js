// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import classNames from 'classnames';
import styles from './SidebarMenuItem.scss';

@observer
export default class SidebarMenuItem extends Component {

  static propTypes = {
    title: PropTypes.string.isRequired,
    info: PropTypes.string.isRequired,
    active: PropTypes.bool,
  };

  render() {
    const { title, info, active } = this.props;
    const componentStyles = classNames([
      styles.component,
      active ? styles.active : null
    ]);
    return (
      <button className={componentStyles}>
        <span className={styles.title}>{title}</span>
        <span className={styles.info}>{info}</span>
      </button>
    );
  }

}
