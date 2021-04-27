// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { map } from 'lodash';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import { Select } from 'react-polymorph/lib/components/Select';
import { Link } from 'react-polymorph/lib/components/Link';
import NormalSwitch from '../../widgets/forms/NormalSwitch';
import styles from './WalletsSettings.scss';
import { currencyConfig } from '../../../config/currencyConfig';
import type { LocalizedCurrency } from '../../../types/currencyTypes';

const messages = defineMessages({
  currencyTitleLabel: {
    id: 'settings.wallets.currency.titleLabel',
    defaultMessage: '!!!Display ada balances in other currency',
    description:
      'titleLabel for the Currency settings in the Wallets settings page.',
  },
  currencyDescription: {
    id: 'settings.wallets.currency.description',
    defaultMessage:
      '!!!Select a conversion currency for displaying your ada balances.',
    description:
      'currencyDescription for the Currency settings in the Wallets settings page.',
  },
  currencySelectLabel: {
    id: 'settings.wallets.currency.selectLabel',
    defaultMessage: '!!!Select currency',
    description:
      'currencySelectLabel for the Currency settings in the Wallets settings page.',
  },
  currencyDisclaimer: {
    id: 'settings.wallets.currency.disclaimer',
    defaultMessage:
      '!!!Conversion rates are provided by CoinGecko without any warranty. Please use the calculated conversion value only as a reference. Converted balances reflect the current global average price of ada on active cryptocurrency exchanges, as tracked by CoinGecko. Ada conversion is available only to fiat and cryptocurrencies that are supported by CoinGecko, other local currency conversions may not be available.',
    description:
      'currencyDisclaimer for the Currency settings in the Wallets settings page.',
  },
  currencyPoweredByLabel: {
    id: 'settings.wallets.currency.poweredBy.label',
    defaultMessage: '!!!Powered by ',
    description:
      'currencyPoweredByLabel for the Currency settings in the Wallets settings page.',
  },
});

type Props = {
  currencySelected: ?LocalizedCurrency,
  currencyList: Array<any>,
  currencyIsActive: boolean,
  onSelectCurrency: Function,
  onToggleCurrencyIsActive: Function,
  onOpenExternalLink: Function,
  hasSearch: boolean,
};

@observer
export default class WalletSettings extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const {
      currencySelected,
      currencyList,
      currencyIsActive,
      onSelectCurrency,
      onToggleCurrencyIsActive,
      onOpenExternalLink,
      hasSearch,
    } = this.props;

    const currencyOptions = map(currencyList, ({ code, name }) => ({
      label: `${code.toUpperCase()} - ${name}`,
      value: code,
    }));

    return (
      <div className={styles.component}>
        <div className={styles.label}>
          {intl.formatMessage(messages.currencyTitleLabel)}
        </div>
        <div className={styles.description}>
          <p>{intl.formatMessage(messages.currencyDescription)}</p>
          <NormalSwitch
            checked={currencyIsActive}
            onChange={onToggleCurrencyIsActive}
            className={styles.switch}
          />
        </div>
        {currencyIsActive && (
          <div className={styles.content}>
            <hr className={styles.separator} />
            <div className={styles.currencyPoweredBy}>
              {intl.formatMessage(messages.currencyPoweredByLabel)}
              <Link
                className={styles.currencyPoweredByLink}
                onClick={() => onOpenExternalLink(currencyConfig.website)}
                label={currencyConfig.name}
              />
            </div>
            <Select
              label={intl.formatMessage(messages.currencySelectLabel)}
              value={currencySelected ? currencySelected.code : null}
              options={currencyOptions}
              onChange={onSelectCurrency}
              hasSearch={hasSearch}
              optionHeight={50}
            />
            <div className={styles.disclaimer}>
              <FormattedHTMLMessage
                {...messages.currencyDisclaimer}
                values={{ currencyApiName: currencyConfig.name }}
              />
            </div>
          </div>
        )}
      </div>
    );
  }
}
