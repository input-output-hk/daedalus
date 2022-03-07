import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classNames from 'classnames';
import styles from './SettingsMenuItem.scss';
type Props = {
  label: string;
  active: boolean;
  onClick: (...args: Array<any>) => any;
  className: string;
  disabled?: boolean;
};

@observer
class SettingsMenuItem extends Component<Props> {
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
      <button className={componentClasses} onClick={onClick}>
        {label}
      </button>
    );
  }
}

export default SettingsMenuItem;
