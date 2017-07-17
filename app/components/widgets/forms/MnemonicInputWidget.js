import React, { Component } from 'react';
import { observer } from 'mobx-react';
import Input from 'react-polymorph/lib/components/Input';
import SimpleInputSkin from 'react-polymorph/lib/skins/simple/InputSkin';
import styles from './MnemonicInputWidget.scss';

@observer
export default class MnemonicInputWidget extends Component {

  props: {
    label: string,
    tokens: Array<string>,
    onTokenChanged: Function,
    error: string,
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
              placeholder="Token"
              key={index}
              className={styles.input}
              value={token}
              onChange={(value) => onTokenChanged(index, value)}
              skin={<SimpleInputSkin />}
            />
          ))}
        </div>
      </div>
    );
  }
}
