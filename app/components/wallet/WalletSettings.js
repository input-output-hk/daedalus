// @flow
import React, { Component, PropTypes } from 'react';
import { observer, PropTypes as MobxPropTypes } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import Dropdown from 'react-toolbox/lib/dropdown/Dropdown';
import LocalizableError from '../../i18n/LocalizableError';
import styles from './WalletSettings.scss';

const messages = defineMessages({
  assuranceLevelLabel: {
    id: 'wallet.settings.assurance',
    defaultMessage: '!!!Transaction assurance security level',
    description: 'Label for the "Transaction assurance security level" dropdown.',
  },
  unitsLabel: {
    id: 'wallet.settings.units',
    defaultMessage: '!!!Units',
    description: 'Label for the "Units" dropdown.',
  },
});

@observer
export default class WalletSettings extends Component {

  static propTypes = {
    assuranceLevels: MobxPropTypes.arrayOrObservableArrayOf(PropTypes.shape({
      value: PropTypes.string.isRequired,
      label: PropTypes.object.isRequired,
    })).isRequired,
    units: MobxPropTypes.arrayOrObservableArrayOf(PropTypes.shape({
      value: PropTypes.number.isRequired,
      label: PropTypes.object.isRequired,
    })).isRequired,
    walletAssurance: PropTypes.string.isRequired,
    walletUnit: PropTypes.number.isRequired,
    onWalletAssuranceLevelUpdate: PropTypes.func.isRequired,
    onWalletUnitUpdate: PropTypes.func.isRequired,
    error: PropTypes.instanceOf(LocalizableError),
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const {
      assuranceLevels, units,
      walletAssurance, walletUnit,
      onWalletAssuranceLevelUpdate,
      onWalletUnitUpdate, error,
    } = this.props;
    const assuranceLevelOptions = assuranceLevels.map(assurance => ({
      value: assurance.value,
      label: intl.formatMessage(assurance.label),
    }));
    const unitOptions = units.map(unit => ({
      value: unit.value,
      label: intl.formatMessage(unit.label),
    }));
    return (
      <div className={styles.component}>

        <div className={styles.borderedBox}>

          <Dropdown
            label={intl.formatMessage(messages.unitsLabel)}
            source={unitOptions}
            value={walletUnit}
            onChange={(value) => onWalletUnitUpdate({ unit: value })}
          />

          <Dropdown
            label={intl.formatMessage(messages.assuranceLevelLabel)}
            source={assuranceLevelOptions}
            value={walletAssurance}
            onChange={(value) => onWalletAssuranceLevelUpdate({ assurance: value })}
          />

          {error && <p className={styles.error}>{intl.formatMessage(error)}</p>}

        </div>

      </div>
    );
  }

}
