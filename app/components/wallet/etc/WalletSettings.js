// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import moment from 'moment';
import LocalizableError from '../../../i18n/LocalizableError';
import BorderedBox from '../../widgets/BorderedBox';
import InlineEditingInput from '../../widgets/forms/InlineEditingInput';
import ReadOnlyInput from '../../widgets/forms/ReadOnlyInput';
import DeleteWalletButton from '../settings/DeleteWalletButton';
import DeleteWalletConfirmationDialog from '../settings/DeleteWalletConfirmationDialog';
import DeleteWalletDialogContainer from '../../../containers/wallet/dialogs/DeleteWalletDialogContainer';
import ChangeWalletPasswordDialog from '../settings/ChangeWalletPasswordDialog';
import ChangeWalletPasswordDialogContainer from '../../../containers/wallet/dialogs/ChangeWalletPasswordDialogContainer';
import globalMessages from '../../../i18n/global-messages';
import styles from '../WalletSettings.scss';

const messages = defineMessages({
  name: {
    id: 'wallet.settings.name.label',
    defaultMessage: '!!!Name',
    description: 'Label for the "Name" text input on the wallet settings page.',
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
    defaultMessage: '!!!You still don\'t have password',
    description: 'You still don\'t have password set message.',
  },
});

type Props = {
  walletName: string,
  isWalletPasswordSet: boolean,
  walletPasswordUpdateDate: ?Date,
  error?: ?LocalizableError,
  openDialogAction: Function,
  isDialogOpen: Function,
  onFieldValueChange: Function,
  onStartEditing: Function,
  onStopEditing: Function,
  onCancelEditing: Function,
  nameValidator: Function,
  activeField: ?string,
  isSubmitting: boolean,
  isInvalid: boolean,
  lastUpdatedField: ?string,
};

@observer
export default class WalletSettings extends Component<Props> {

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  componentWillUnmount() {
    // This call is used to prevent display of old successfully-updated messages
    this.props.onCancelEditing();
  }

  render() {
    const { intl } = this.context;
    const {
      walletName, isWalletPasswordSet,
      walletPasswordUpdateDate, error,
      openDialogAction, isDialogOpen,
      onFieldValueChange, onStartEditing,
      onStopEditing, onCancelEditing,
      nameValidator, activeField,
      isSubmitting, isInvalid,
      lastUpdatedField,
    } = this.props;

    const passwordMessage = isWalletPasswordSet ? (
      intl.formatMessage(messages.passwordLastUpdated, {
        lastUpdated: moment(walletPasswordUpdateDate).fromNow(),
      })
    ) : intl.formatMessage(messages.passwordNotSet);

    return (
      <div className={styles.component}>

        <BorderedBox>

          <InlineEditingInput
            className="walletName"
            inputFieldLabel={intl.formatMessage(messages.name)}
            inputFieldValue={walletName}
            isActive={activeField === 'name'}
            onStartEditing={() => onStartEditing('name')}
            onStopEditing={onStopEditing}
            onCancelEditing={onCancelEditing}
            onSubmit={(value) => onFieldValueChange('name', value)}
            isValid={nameValidator}
            validationErrorMessage={intl.formatMessage(globalMessages.invalidWalletName)}
            successfullyUpdated={!isSubmitting && lastUpdatedField === 'name' && !isInvalid}
          />

          <ReadOnlyInput
            label={intl.formatMessage(messages.passwordLabel)}
            value={passwordMessage}
            isSet={isWalletPasswordSet}
            onClick={() => openDialogAction({
              dialog: ChangeWalletPasswordDialog,
            })}
          />

          {error && <p className={styles.error}>{intl.formatMessage(error)}</p>}

          <div className={styles.actionButtons}>
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
