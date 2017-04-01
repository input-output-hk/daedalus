// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import classNames from 'classnames';
import styles from './SettingsMenuItem.scss';

@observer
export default class SettingsMenuItem extends Component {

  static propTypes = {
    label: PropTypes.string.isRequired,
    active: PropTypes.bool,
    disabled: PropTypes.bool,
    onClick: PropTypes.func,
    className: PropTypes.string
  };

  render() {
    const { label, active, disabled, onClick, className } = this.props;
    let state = styles.enabled;
    if (disabled) {
      state = styles.disabled;
    } else if (active) {
      state = styles.active;
    }
    const componentClasses = classNames([styles.component, state, className]);
    return (
      <button className={componentClasses} onClick={onClick}>{label}</button>
    );
  }

}
