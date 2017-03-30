// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import BorderedBox from '../widgets/BorderedBox';
import styles from './WalletSettings.scss';
import DeleteWalletButton from './settings/DeleteWalletButton';
import DeleteWalletConfirmationDialog from './settings/DeleteWalletConfirmationDialog';
import DeleteWalletDialogContainer from '../../containers/wallet/dialogs/DeleteWalletDialogContainer';

@observer
export default class WalletSettings extends Component {

  static propTypes = {
    openDialogAction: PropTypes.func.isRequired,
    isDialogOpen: PropTypes.func.isRequired,
  };

  render() {
    const { openDialogAction, isDialogOpen } = this.props;
    return (
      <div className={styles.component}>

        <BorderedBox>

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
