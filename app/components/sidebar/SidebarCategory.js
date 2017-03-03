// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import classNames from 'classnames';
import styles from './SidebarCategory.scss';

@observer
export default class SidebarCategory extends Component {

  static propTypes = {
    icon: PropTypes.string.isRequired,
    active: PropTypes.bool,
    onClick: PropTypes.func,
    className: PropTypes.string
  };

  render() {
    const { icon, active, onClick, className } = this.props;
    const componentStyles = classNames([
      styles.component,
      active ? styles.active : null,
      className
    ]);
    return (
      <button className={componentStyles} onClick={onClick}>
        <img className={styles.icon} src={icon} role="presentation" />
      </button>
    );
  }

}
