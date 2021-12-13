import React, { Component } from 'react';
import { IObservableArray } from 'mobx';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import { Input } from 'react-polymorph/lib/components/Input';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './MnemonicInputWidget.scss' or... Remove this comment to see the full error message
import styles from './MnemonicInputWidget.scss';

const messages = defineMessages({
  token: {
    id: 'global.labels.token',
    defaultMessage: '!!!Token',
    description: 'Token description.',
  },
});
type Props = {
  label: string;
  tokens: IObservableArray<string>;
  onTokenChanged: (...args: Array<any>) => any;
  error?: string;
};

@observer
class MnemonicInputWidget extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { label, tokens, onTokenChanged, error } = this.props;
    return (
      <div className={styles.component}>
        <div className={styles.label}>{label}</div>
        {error && <div className={styles.error}>{error}</div>}
        <div className={styles.tokens}>
          {tokens.map((token, index) => (
            <Input
              type="text"
              placeholder={intl.formatMessage(messages.token)} // eslint-disable-next-line react/no-array-index-key
              key={index}
              className={styles.input}
              value={token}
              onChange={(value) => onTokenChanged(index, value)}
              skin={InputSkin}
            />
          ))}
        </div>
      </div>
    );
  }
}

export default MnemonicInputWidget;
