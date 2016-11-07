// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import classNames from 'classnames';
import styles from './SidebarMainItem.scss';

@observer
export default class SidebarMainItem extends Component {

  static propTypes = {
    icon: PropTypes.string.isRequired,
    label: PropTypes.string.isRequired,
    active: PropTypes.bool,
    minimized: PropTypes.bool,
  };

  render() {
    const { icon, label, active, minimized } = this.props;
    const componentStyles = classNames([
      styles.component,
      active ? styles.active : null
    ]);
    return (
      <div className={componentStyles}>
        <img className={styles.icon} src={icon} role="presentation" />
        <span className={minimized ? styles.noLabel : styles.label}>{label}</span>
      </div>
    );
  }

}
