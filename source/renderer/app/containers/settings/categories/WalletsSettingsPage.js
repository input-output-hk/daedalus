// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import WalletsSettings from '../../../components/settings/categories/WalletsSettings';
import type { InjectedProps } from '../../../types/injectedPropsType';

@inject('stores', 'actions')
@observer
export default class WalletsSettingsPage extends Component<InjectedProps> {
  static defaultProps = { actions: null, stores: null };

  handleSelectCurrency = (currencyCode: string) => {
    this.props.actions.wallets.setCurrencySelected.trigger({ currencyCode });
  };

  handleToggleCurrencyIsActive = () =>
    this.props.actions.wallets.toggleCurrencyIsActive.trigger();

  render() {
    const { stores } = this.props;
    const {
      localizedCurrency: currencySelected,
      localizedCurrencyList: currencyList,
      currencyRate,
      currencyIsActive,
    } = stores.wallets;
    const { currentLocale } = stores.profile;
    const { openExternalLink } = stores.app;
    return (
      <WalletsSettings
        currencySelected={currencySelected}
        currencyRate={currencyRate}
        currencyList={currencyList}
        currencyIsActive={currencyIsActive}
        currentLocale={currentLocale}
        onSelectCurrency={this.handleSelectCurrency}
        onToggleCurrencyIsActive={this.handleToggleCurrencyIsActive}
        onOpenExternalLink={openExternalLink}
      />
    );
  }
}
