import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import { dappCatalogPresentation } from '../../../../common/config/dappCatalog';
import DappCatalog from '../../components/dapp/DappCatalog';
import CollateralPanel from '../../components/dapp/collateral/CollateralPanel';
import type { InjectedProps } from '../../types/injectedPropsType';

export const dappCatalogMessages = defineMessages({
  liqwidName: {
    id: 'dapp.catalog.liqwid.name',
    defaultMessage: '!!!Liqwid Finance',
    description: 'Name of the Liqwid Finance dApp in the curated catalog.',
  },
  liqwidDescription: {
    id: 'dapp.catalog.liqwid.description',
    defaultMessage: '!!!Borrow, lend, and earn yield on Cardano.',
    description: 'Description of Liqwid Finance in the curated dApp catalog.',
  },
});

type Props = { stores?: InjectedProps['stores'] };

@inject('stores')
@observer
export default class DappCatalogPage extends Component<Props> {
  static contextTypes = { intl: intlShape.isRequired };
  private collateralContext?: string;

  componentDidMount(): void {
    this.refreshCollateral();
  }

  componentDidUpdate(): void {
    this.refreshCollateral();
  }

  private refreshCollateral = (): void => {
    const { collateral, networkStatus, wallets } = this.props.stores!;
    const walletId = wallets.activeDappWallet?.id;
    const context = `${walletId ?? ''}:${networkStatus.isConnected}:${
      networkStatus.isSynced
    }`;
    if (context === this.collateralContext) return;
    this.collateralContext = context;
    if (walletId && networkStatus.isConnected && networkStatus.isSynced)
      collateral.refresh();
  };

  launch = (id: string): void => {
    const entry = dappCatalogPresentation.find(
      (candidate) => candidate.id === id
    );
    if (entry)
      this.props.stores!.dapp.launch(
        id,
        this.context.intl.formatMessage({ id: entry.nameMessageId })
      );
  };

  render() {
    const { dapp, networkStatus, wallets } = this.props.stores!;
    const walletReady =
      !!wallets.activeDappWallet &&
      networkStatus.isConnected &&
      networkStatus.isSynced;
    const { intl } = this.context;
    return (
      <DappCatalog
        entries={dappCatalogPresentation.map((entry) => ({
          id: entry.id,
          name: intl.formatMessage({ id: entry.nameMessageId }),
          description: intl.formatMessage({
            id: entry.descriptionMessageId,
          }),
          iconAsset: entry.iconAsset,
        }))}
        available={dapp.catalogAvailable}
        ready={dapp.ready}
        isOpen={dapp.guestOpen}
        isLaunching={dapp.isLaunching}
        beforeEntries={
          <CollateralPanel
            preference={this.props.stores!.collateral.snapshot?.preference}
            corrupt={this.props.stores!.collateral.snapshot?.corrupt ?? false}
            busy={this.props.stores!.collateral.isLoading}
            failed={this.props.stores!.collateral.actionFailed || !walletReady}
            onPrepare={this.props.stores!.collateral.prepare}
            onCancelPreparation={
              this.props.stores!.collateral.cancelPreparation
            }
            onClear={this.props.stores!.collateral.clear}
            onRepair={this.props.stores!.collateral.repair}
          />
        }
        onLaunch={this.launch}
        onClose={dapp.close}
      />
    );
  }
}
