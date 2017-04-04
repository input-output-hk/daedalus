// @flow
import React, { Component, PropTypes } from 'react';
import { observer, PropTypes as MobxPropTypes } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import moment from 'moment';
import Dropdown from 'react-toolbox/lib/dropdown/Dropdown';
import LocalizableError from '../../i18n/LocalizableError';
import BorderedBox from '../widgets/BorderedBox';
import styles from './WalletSettings.scss';
import ReadOnlyInput from '../widgets/forms/ReadOnlyInput';
import DeleteWalletButton from './settings/DeleteWalletButton';
import DeleteWalletConfirmationDialog from './settings/DeleteWalletConfirmationDialog';
import DeleteWalletDialogContainer from '../../containers/wallet/dialogs/DeleteWalletDialogContainer';
import ChangeWalletPasswordDialog from './settings/ChangeWalletPasswordDialog';
import ChangeWalletPasswordDialogContainer from '../../containers/wallet/dialogs/ChangeWalletPasswordDialogContainer';

const messages = defineMessages({
  assuranceLevelLabel: {
    id: 'wallet.settings.assurance',
    defaultMessage: '!!!Transaction assurance security level',
    description: 'Label for the "Transaction assurance security level" dropdown.',
  },
  passwordLabel: {
    id: 'wallet.settings.password',
    defaultMessage: '!!!Password',
    description: 'Label for the "Password" field.',
  },
  passwordLastUpdated: {
    id: 'wallet.settings.passwordLastUpdated',
    defaultMessage: '!!!Last updated',
    description: 'Last updated X time ago message.',
  },
  passwordNotSet: {
    id: 'wallet.settings.passwordNotSet',
    defaultMessage: '!!!Password',
    description: 'Not set message.',
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
    hasWalletPassword: PropTypes.bool.isRequired,
    walletPasswordUpdateDate: PropTypes.instanceOf(Date).isRequired,
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
      hasWalletPassword,
      walletPasswordUpdateDate, error,
      openDialogAction, isDialogOpen,
    } = this.props;

    const assuranceLevelOptions = assuranceLevels.map(assurance => ({
      value: assurance.value,
      label: intl.formatMessage(assurance.label),
    }));

    const passwordMessage = hasWalletPassword ? (
      intl.formatMessage(messages.passwordLastUpdated, {
        lastUpdated: moment(walletPasswordUpdateDate).fromNow(),
      })
    ) : intl.formatMessage(messages.passwordNotSet);

    return (
      <div className={styles.component}>

        <BorderedBox>

          <Dropdown
            label={intl.formatMessage(messages.assuranceLevelLabel)}
            source={assuranceLevelOptions}
            value={walletAssurance}
            onChange={(value) => onWalletAssuranceLevelUpdate({ assurance: value })}
          />

          <ReadOnlyInput
            label={intl.formatMessage(messages.passwordLabel)}
            value={passwordMessage}
            onClick={() => openDialogAction({
              dialog: ChangeWalletPasswordDialog,
            })}
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

        {isDialogOpen(ChangeWalletPasswordDialog) ? (
          <ChangeWalletPasswordDialogContainer />
        ) : null}

        {isDialogOpen(DeleteWalletConfirmationDialog) ? (
          <DeleteWalletDialogContainer />
        ) : null}

      </div>
    );
  }

}
