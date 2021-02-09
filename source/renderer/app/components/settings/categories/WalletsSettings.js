// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { map } from 'lodash';
import { defineMessages, intlShape } from 'react-intl';
import { Select } from 'react-polymorph/lib/components/Select';
import { Link } from 'react-polymorph/lib/components/Link';
import NormalSwitch from '../../widgets/forms/NormalSwitch';
import styles from './WalletsSettings.scss';
import { currencyConfig } from '../../../config/currencyConfig';
import type { Currency } from '../../../types/currencyTypes';

const messages = defineMessages({
  currencyTitleLabel: {
    id: 'settings.wallets.currency.titleLabel',
    defaultMessage: '!!!Show ada price in other currency',
    description:
      'titleLabel for the Currency settings in the Wallets settings page.',
  },
  currencyDescription: {
    id: 'settings.wallets.currency.description',
    defaultMessage:
      '!!!This feature does… Lorem ipsum dolor sit amet, consectetur adipiscing elit',
    description:
      'currencyDescription for the Currency settings in the Wallets settings page.',
  },
  currencySelectLabel: {
    id: 'settings.wallets.currency.selectLabel',
    defaultMessage: '!!!Countervalue',
    description:
      'currencySelectLabel for the Currency settings in the Wallets settings page.',
  },
  currencyPoweredByLabel: {
    id: 'settings.wallets.currency.poweredBy.label',
    defaultMessage: '!!!Powered by ',
    description:
      'currencyPoweredByLabel for the Currency settings in the Wallets settings page.',
  },
});

type Props = {
  currencySelected: ?Currency,
  currencyList: Array<any>,
  currencyIsActive: boolean,
  onSelectCurrency: Function,
  onToggleCurrencyIsActive: Function,
  onOpenExternalLink: Function,
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
    } = this.props;

    const currencyOptions = map(currencyList, ({ symbol, name }) => {
      return {
        label: `${symbol.toUpperCase()} - ${name}`,
        value: symbol,
      };
    });

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
        <Select
          label={intl.formatMessage(messages.currencySelectLabel)}
          value={currencySelected ? currencySelected.symbol : null}
          options={currencyOptions}
          onChange={onSelectCurrency}
        />
        <div className={styles.currencyPoweredBy}>
          {intl.formatMessage(messages.currencyPoweredByLabel)}
          <Link
            className={styles.currencyPoweredByLink}
            onClick={() => onOpenExternalLink(currencyConfig.website)}
            label={currencyConfig.name}
          />
        </div>
      </div>
    );
  }
}
