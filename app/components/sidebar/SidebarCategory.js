// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import classNames from 'classnames';
import styles from './SidebarCategory.scss';

@observer
export default class SidebarCategory extends Component {

  static propTypes = {
    icon: PropTypes.string.isRequired,
    label: PropTypes.string.isRequired,
    active: PropTypes.bool,
    minimized: PropTypes.bool,
    onClick: PropTypes.func,
  };

  render() {
    const { icon, label, active, minimized, onClick } = this.props;
    const componentStyles = classNames([
      styles.component,
      active ? styles.active : null
    ]);
    return (
      <button className={componentStyles} onClick={onClick}>
        <img className={styles.icon} src={icon} role="presentation" />
        <span className={minimized ? styles.noLabel : styles.label}>{label}</span>
      </button>
    );
  }

}
