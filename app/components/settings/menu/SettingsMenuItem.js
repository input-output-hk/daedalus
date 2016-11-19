// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import classNames from 'classnames';
import styles from './SettingsMenuItem.scss';

@observer
export default class SettingsMenuItem extends Component {

  static propTypes = {
    label: PropTypes.string.isRequired,
    active: PropTypes.bool.isRequired
  };

  render() {
    const { label, active } = this.props;

    const componentClasses = classNames([
      styles.component,
      active ? styles.active : null
    ]);

    return (
      <div className={componentClasses}>{label}</div>
    );
  }

}
