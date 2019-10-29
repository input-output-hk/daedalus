// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import { intlShape } from 'react-intl';
import { Input } from 'react-polymorph/lib/components/Input';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import globalMessages from '../../../i18n/global-messages';
import styles from './ReadOnlyInput.scss';

type Props = {
  label: string,
  value: string,
  onClick: Function,
};

@observer
export default class ReadOnlyInput extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { label, value, onClick } = this.props;
    const { intl } = this.context;

    const mainClasses = classnames([
      styles.component,
      'changeLabel',
    ]);

    return (
      <div className={mainClasses}>
        <Input
          themeOverrides={styles}
          type="text"
          label={label}
          value={value}
          disabled
          skin={InputSkin}
        />

        <button className={styles.button} onClick={onClick}>
          {intl.formatMessage(globalMessages.change)}
        </button>
      </div>
    );
  }
}
