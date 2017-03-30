// @flow
import React, { Component, PropTypes } from 'react';
import { observer, PropTypes as MobxPropTypes } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import Dropdown from 'react-toolbox/lib/dropdown/Dropdown';
import LocalizableError from '../../i18n/LocalizableError';
import BorderedBox from '../widgets/BorderedBox';
import styles from './WalletSettings.scss';
import DeleteWalletButton from './settings/DeleteWalletButton';
import DeleteWalletConfirmationDialog from './settings/DeleteWalletConfirmationDialog';
import DeleteWalletDialogContainer from '../../containers/wallet/dialogs/DeleteWalletDialogContainer';

const messages = defineMessages({
  assuranceLevelLabel: {
    id: 'wallet.settings.assurance',
    defaultMessage: '!!!Transaction assurance security level',
    description: 'Label for the "Transaction assurance security level" dropdown.',
  },
});

@observer
export default class WalletSettings extends Component {

  static propTypes = {
    assuranceLevels: MobxPropTypes.arrayOrObservableArrayOf(PropTypes.shape({
      value: PropTypes.string.isRequired,
      label: PropTypes.object.isRequired,
    })).isRequired,
    walletAssurance: PropTypes.string.isRequired,
    onWalletAssuranceLevelUpdate: PropTypes.func.isRequired,
    onWalletUnitUpdate: PropTypes.func.isRequired,
    error: PropTypes.instanceOf(LocalizableError),
    openDialogAction: PropTypes.func.isRequired,
    isDialogOpen: PropTypes.func.isRequired,
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const {
      assuranceLevels, walletAssurance,
      onWalletAssuranceLevelUpdate,
      error, openDialogAction, isDialogOpen,
    } = this.props;
    const assuranceLevelOptions = assuranceLevels.map(assurance => ({
      value: assurance.value,
      label: intl.formatMessage(assurance.label),
    }));
    return (
      <div className={styles.component}>

        <BorderedBox>

          <Dropdown
            label={intl.formatMessage(messages.assuranceLevelLabel)}
            source={assuranceLevelOptions}
            value={walletAssurance}
            onChange={(value) => onWalletAssuranceLevelUpdate({ assurance: value })}
          />

          {error && <p className={styles.error}>{intl.formatMessage(error)}</p>}

          <div className={styles.deleteWalletButton}>
            <DeleteWalletButton
              onClick={() => openDialogAction({
                dialog: DeleteWalletConfirmationDialog,
              })}
            />
          </div>

        </BorderedBox>

        {isDialogOpen(DeleteWalletConfirmationDialog) ? (
          <DeleteWalletDialogContainer />
        ) : null}

      </div>
    );
  }

}
