// @flow
import React, { Component, PropTypes } from 'react';
import { observer, PropTypes as MobxPropTypes } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import Dropdown from 'react-toolbox/lib/dropdown/Dropdown';
import ReactToolboxMobxForm from '../../lib/ReactToolboxMobxForm';
import LocalizableError from '../../i18n/LocalizableError';
import styles from './WalletSettings.scss';

const messages = defineMessages({
  unitsLabel: {
    id: 'wallet.settings.units',
    defaultMessage: '!!!Units',
    description: 'Label for the "Units" input.',
  },
});

@observer
export default class WalletSettings extends Component {

  static propTypes = {
    units: MobxPropTypes.arrayOrObservableArrayOf(PropTypes.shape({
      value: PropTypes.number.isRequired,
      label: PropTypes.object.isRequired,
    })).isRequired,
    walletUnit: PropTypes.number.isRequired,
    onWalletUnitUpdate: PropTypes.func.isRequired,
    error: PropTypes.instanceOf(LocalizableError),
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  form = new ReactToolboxMobxForm({
    fields: {
      walletUnit: {
        label: this.context.intl.formatMessage(messages.unitsLabel),
        bindings: 'ReactToolbox',
      },
    },
  }, {
    options: {
      validateOnChange: false,
    },
  });

  render() {
    const { intl } = this.context;
    const { form } = this;
    const { units, onWalletUnitUpdate, error } = this.props;
    const walletUnit = form.$('walletUnit');
    const unitOptions = units.map(unit => ({
      value: unit.value,
      label: intl.formatMessage(unit.label),
    }));
    return (
      <div className={styles.component}>

        <div className={styles.borderedBox}>

          <Dropdown
            source={unitOptions}
            {...walletUnit.bind()}
            value={this.props.walletUnit}
            onChange={(value) => onWalletUnitUpdate({ unit: value })}
          />

          {error && <p className={styles.error}>{intl.formatMessage(error)}</p>}

        </div>

      </div>
    );
  }

}
