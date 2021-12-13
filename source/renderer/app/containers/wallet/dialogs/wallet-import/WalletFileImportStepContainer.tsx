import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import type { InjectedDialogContainerStepProps } from '../../../../types/injectedPropsType';
import { InjectedDialogContainerStepDefaultProps } from '../../../../types/injectedPropsType';
import WalletImportFileDialog from '../../../../components/wallet/wallet-import/WalletImportFileDialog';
import type { ImportFromOption } from '../../../../types/walletExportTypes';

type Props = InjectedDialogContainerStepProps;
const DefaultProps = InjectedDialogContainerStepDefaultProps;

@inject('stores', 'actions')
@observer
class WalletFileImportStepContainer extends Component<Props> {
  static defaultProps = DefaultProps;
  onSelectExportSourcePath = (params: { importFrom: ImportFromOption }) => {
    this.props.actions.walletMigration.selectExportSourcePath.trigger(params);
  };
  onResetExportSourcePath = () => {
    this.props.actions.walletMigration.resetExportSourcePath.trigger();
  };
  onOpen = () => {
    this.props.actions.walletMigration.resetMigration.trigger();
    this.props.actions.walletMigration.initiateMigration.trigger();
  };

  render() {
    const { onClose, onContinue, stores } = this.props;
    const { walletMigration, app } = stores;
    const {
      exportErrors,
      exportSourcePath,
      defaultExportSourcePath,
      pendingImportWalletsCount,
      isExportRunning,
    } = walletMigration;
    const { openExternalLink } = app;
    return (
      <WalletImportFileDialog
        isSubmitting={isExportRunning}
        exportSourcePath={exportSourcePath}
        defaultExportSourcePath={defaultExportSourcePath}
        exportErrors={exportErrors}
        // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
        pendingImportWalletsCount={pendingImportWalletsCount}
        onOpen={this.onOpen}
        onOpenExternalLink={openExternalLink}
        onSelectExportSourcePath={this.onSelectExportSourcePath}
        onResetExportSourcePath={this.onResetExportSourcePath}
        onClose={onClose}
        onContinue={onContinue}
      />
    );
  }
}

export default WalletFileImportStepContainer;
