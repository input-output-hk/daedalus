// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import RTSwitch from 'react-toolbox/lib/switch';
import styles from './Switch.scss';

@observer
export default class Switch extends Component {

  props: {
    active: boolean,
    label: string,
    placeholder: string,
    onChange: Function,
  };

  static defaultProps = {
    active: false,
    label: '',
    placeholder: '',
  };

  render() {
    const { active, label, placeholder, onChange } = this.props;
    return (
      <div className={styles.component}>
        <div className={styles.label}>{label}</div>
        <div className={styles.switch}>
          <div className={styles.placeholder}>{placeholder}</div>
          <RTSwitch
            checked={active}
            onChange={(value) => onChange(value)}
          />
        </div>
      </div>
    );
  }

}
