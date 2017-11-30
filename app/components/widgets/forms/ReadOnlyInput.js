// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import { intlShape } from 'react-intl';
import Input from 'react-polymorph/lib/components/Input';
import SimpleInputSkin from 'react-polymorph/lib/skins/simple/InputSkin';
import globalMessages from '../../../i18n/global-messages';
import styles from './ReadOnlyInput.scss';

type Props = {
  label: string,
  value: string,
  isSet: boolean,
  onClick: Function,
};

@observer
export default class ReadOnlyInput extends Component<Props> {

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const {
      label,
      value,
      isSet,
      onClick,
    } = this.props;
    const { intl } = this.context;
    const buttonLabel = intl.formatMessage(globalMessages[isSet ? 'change' : 'create']);

    const mainClasses = classnames([
      styles.component,
      isSet ? 'changeLabel' : 'createLabel',
    ]);

    return (
      <div className={mainClasses}>

        <Input
          className={styles.input}
          type="text"
          label={label}
          value={value}
          disabled
          skin={<SimpleInputSkin />}
        />

        <button
          className={styles.button}
          onClick={onClick}
        >
          {buttonLabel}
        </button>

      </div>
    );
  }

}
