import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import { intlShape } from 'react-intl';
import { dappCatalogPresentation } from '../../../../common/config/dappCatalog';
import DappCatalog from '../../components/dapp/DappCatalog';
import type { InjectedProps } from '../../types/injectedPropsType';

type Props = { stores?: InjectedProps['stores'] };

@inject('stores')
@observer
export default class DappCatalogPage extends Component<Props> {
  static contextTypes = { intl: intlShape.isRequired };

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
        onLaunch={this.launch}
        onClose={dapp.close}
      />
    );
  }
}
