import React, { Component, PropTypes } from 'react';
import { observer, PropTypes as MobxPropTypes } from 'mobx-react';
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
    const { label, tokens, onTokenChanged, error } = this.props;
    return (
      <div className={styles.component}>
        <div className={styles.label}>{label}</div>
        {error && <div className={styles.error}>{error}</div>}
        <div className={styles.tokens}>
          {tokens.map((token, index) => (
            <Input
              type="text"
              hint="Token"
              key={index}
              className={styles.input}
              value={token}
              onChange={(value) => onTokenChanged(index, value)}
            />
          ))}
        </div>
      </div>
    );
  }
}
