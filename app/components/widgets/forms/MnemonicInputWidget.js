import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import { PropTypes as MobxPropTypes } from 'mobx-react';
import { Checkbox } from 'react-toolbox';
import Input from 'react-toolbox/lib/input/Input';
import styles from './MnemonicInputWidget.scss';

@observer
export default class MnemonicInputWidget extends Component {

  static propTypes = {
    label: PropTypes.string.isRequired,
    tokens: MobxPropTypes.arrayOrObservableArrayOf(PropTypes.string.isRequired).isRequired,
    onTokenChanged: PropTypes.func.isRequired,
    error: PropTypes.string,
  };

  render() {
    const { label, tokens, onTokenChanged } = this.props;
    return (
      <div className={styles.component}>
        <div className={styles.label}>{label}</div>
        <div className={styles.tokens}>
          {tokens.map((token, index) => (
            <Input
              type="text"
              hint="Token"
              key={index}
              className={styles.input}
              value={token}
              onChange={(token) => onTokenChanged(index, token)}
            />
          ))}
        </div>
      </div>
    );
  }
}
