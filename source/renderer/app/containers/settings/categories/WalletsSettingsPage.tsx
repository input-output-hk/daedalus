import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import WalletsSettings from '../../../components/settings/categories/WalletsSettings';
import type { InjectedProps } from '../../../types/injectedPropsType';

@inject('stores', 'actions')
@observer
class WalletsSettingsPage extends Component<InjectedProps> {
  static defaultProps = {
    actions: null,
    stores: null,
  };
  handleSelectCurrency = (code: string) => {
    this.props.actions.currency.setCurrencySelected.trigger({
      code,
    });
  };
  handleToggleCurrencyIsActive = () =>
    this.props.actions.currency.toggleCurrencyIsActive.trigger();

  handleToggleWithdrawRewardsDefault = (value: boolean) =>
    this.props.actions.walletSettings.setWithdrawRewardsDefault.trigger({
      value,
    });

  render() {
    const { stores } = this.props;
    const {
      localizedCurrency: selected,
      localizedCurrencyList: currencyList,
      rate,
      isActive,
    } = stores.currency;
    const { currentLocale } = stores.profile;
    const { openExternalLink } = stores.app;
    return (
      <WalletsSettings
        currencySelected={selected}
        // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
        currencyRate={rate}
        currencyList={currencyList}
        currencyIsActive={isActive}
        currentLocale={currentLocale}
        onSelectCurrency={this.handleSelectCurrency}
        onToggleCurrencyIsActive={this.handleToggleCurrencyIsActive}
        onOpenExternalLink={openExternalLink}
        withdrawRewardsDefault={stores.walletSettings.withdrawRewardsDefault}
        onToggleWithdrawRewardsDefault={this.handleToggleWithdrawRewardsDefault}
      />
    );
  }
}

export default WalletsSettingsPage;
