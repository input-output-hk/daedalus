import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import { getDappCatalogPresentation } from '../../../../common/config/dappCatalog';
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
  fluidtokensName: {
    id: 'dapp.catalog.fluidtokens.name',
    defaultMessage: '!!!FluidTokens',
    description: 'Name of the FluidTokens dApp in the curated catalog.',
  },
  fluidtokensDescription: {
    id: 'dapp.catalog.fluidtokens.description',
    defaultMessage: '!!!Use FluidTokens on Cardano.',
    description: 'Description of FluidTokens in the curated dApp catalog.',
  },
  steelswapName: {
    id: 'dapp.catalog.steelswap.name',
    defaultMessage: '!!!SteelSwap',
    description: 'Name of the SteelSwap dApp in the curated catalog.',
  },
  steelswapDescription: {
    id: 'dapp.catalog.steelswap.description',
    defaultMessage: '!!!Swap tokens on Cardano.',
    description: 'Description of SteelSwap in the curated dApp catalog.',
  },
  strikeName: {
    id: 'dapp.catalog.strike.name',
    defaultMessage: '!!!Strike Finance',
    description: 'Name of the Strike Finance dApp in the curated catalog.',
  },
  strikeDescription: {
    id: 'dapp.catalog.strike.description',
    defaultMessage: '!!!Use Strike Finance on Cardano.',
    description: 'Description of Strike Finance in the curated dApp catalog.',
  },
  unfrackName: {
    id: 'dapp.catalog.unfrack.name',
    defaultMessage: '!!!unfrack.it',
    description: 'Name of unfrack.it in the curated dApp catalog.',
  },
  unfrackDescription: {
    id: 'dapp.catalog.unfrack.description',
    defaultMessage: '!!!Optimize your Cardano wallet’s UTxOs.',
    description: 'Description of unfrack.it in the curated dApp catalog.',
  },
});

type Props = { stores?: InjectedProps['stores'] };

@inject('stores')
@observer
export default class DappCatalogPage extends Component<Props> {
  static contextTypes = { intl: intlShape.isRequired };
  private readonly entries = getDappCatalogPresentation(
    global.environment.network,
    global.isFlight
  );
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
    const entry = this.entries.find((candidate) => candidate.id === id);
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
        entries={this.entries.map((entry) => ({
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
