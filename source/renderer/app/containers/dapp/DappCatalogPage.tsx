import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import { intlShape } from 'react-intl';
import { dappCatalogPresentation } from '../../../../common/config/dappCatalog';
import DappCatalog from '../../components/dapp/DappCatalog';
import CollateralPanel from '../../components/dapp/collateral/CollateralPanel';
import type { InjectedProps } from '../../types/injectedPropsType';

type Props = { stores?: InjectedProps['stores'] };

@inject('stores')
@observer
export default class DappCatalogPage extends Component<Props> {
  static contextTypes = { intl: intlShape.isRequired };
  componentDidMount(): void {
    this.props.stores!.collateral.refresh();
  }

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
    const { dapp } = this.props.stores!;
    const { intl } = this.context;
    return (
      <DappCatalog
        entries={dappCatalogPresentation.map((entry) => ({
          id: entry.id,
          name: intl.formatMessage({ id: entry.nameMessageId }),
          description: intl.formatMessage({ id: entry.descriptionMessageId }),
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
            failed={this.props.stores!.collateral.actionFailed}
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
