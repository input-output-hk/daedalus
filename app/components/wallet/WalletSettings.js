// @flow
import React, { Component, PropTypes } from 'react';
import { observer, PropTypes as MobxPropTypes } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import InlineEditingDropdown from '../widgets/forms/InlineEditingDropdown';
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
    isWalletUnitUpdated: PropTypes.bool.isRequired,
    error: PropTypes.instanceOf(LocalizableError),
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const {
      units,
      walletUnit,
      onWalletUnitUpdate,
      isWalletUnitUpdated,
      error,
    } = this.props;
    const unitOptions = units.map(unit => ({
      value: unit.value,
      label: intl.formatMessage(unit.label),
    }));
    return (
      <div className={styles.component}>

        <div className={styles.borderedBox}>

          <InlineEditingDropdown
            isActive
            label={intl.formatMessage(messages.unitsLabel)}
            options={unitOptions}
            value={walletUnit}
            onChange={(value) => onWalletUnitUpdate({ unit: value })}
            successfullyUpdated={isWalletUnitUpdated}
          />

          {error && <p className={styles.error}>{intl.formatMessage(error)}</p>}

        </div>

      </div>
    );
  }

}
