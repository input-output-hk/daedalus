import React from 'react';
import { findKey } from 'lodash';
import addons from '@storybook/addons';
import { boolean, number, CHANGE } from '@storybook/addon-knobs';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import { withState } from '@dump247/storybook-state';
import SettingsWrapper from '../utils/SettingsWrapper';
import {
  DATE_ENGLISH_OPTIONS,
  LANGUAGE_OPTIONS,
  NUMBER_OPTIONS,
  TIME_OPTIONS,
} from '../../../../source/renderer/app/config/profileConfig';
import { updateParam } from '../../../addons/DaedalusMenu';
import { locales, themesIds } from '../../_support/config';
// Screens
import ProfileSettingsForm from '../../../../source/renderer/app/components/widgets/forms/ProfileSettingsForm';
import StakePoolsSettings from '../../../../source/renderer/app/components/settings/categories/StakePoolsSettings';
import DisplaySettings from '../../../../source/renderer/app/components/settings/categories/DisplaySettings';
import SupportSettings from '../../../../source/renderer/app/components/settings/categories/SupportSettings';
import TermsOfUseSettings from '../../../../source/renderer/app/components/settings/categories/TermsOfUseSettings';
import WalletsSettings from '../../../../source/renderer/app/components/settings/categories/WalletsSettings';
import SecuritySettings from '../../../../source/renderer/app/components/settings/categories/SecuritySettings';
// Assets and helpers
import currenciesList from '../../../../source/renderer/app/config/currenciesList.json';
import { getLocalizedCurrenciesList } from '../../../../source/renderer/app/config/currencyConfig';

const changeControl = (name: string, value: boolean) => {
  addons.getChannel().emit(CHANGE, {
    name,
    value,
  });
};

const getParamName = (obj, itemName): any =>
  Object.entries(obj).find((entry: [any, any]) => itemName === entry[1]);

/* eslint-disable consistent-return */
storiesOf('Settings|General', module)
  .addDecorator(SettingsWrapper) // ====== Stories ======
  .add('General', () => (
    <ProfileSettingsForm
      isSubmitting={boolean('isSubmitting', false)}
      onSubmit={action('submit')}
      onChangeItem={(param: string, value: string) => {
        if (param === 'locale') {
          updateParam({
            param: 'localeName',
            value: findKey(locales, (item) => item === value),
          });
        }
      }}
      currentDateFormat={DATE_ENGLISH_OPTIONS[0].value}
      currentLocale={LANGUAGE_OPTIONS[0].value}
      currentNumberFormat={NUMBER_OPTIONS[0].value}
      currentTimeFormat={TIME_OPTIONS[0].value}
    />
  ))
  .add(
    'Wallets',
    withState(
      {
        currencySelected: {
          id: 'uniswap-state-dollar',
          code: 'usd',
          name: 'unified Stable Dollar',
        },
      },
      (store) => (
        <WalletsSettings
          currencySelected={store.state.currencySelected}
          // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
          currencyRate={0.321}
          // @ts-ignore ts-migrate(2345) FIXME: Argument of type '{ aed: { code: string; decimalDi... Remove this comment to see the full error message
          currencyList={getLocalizedCurrenciesList(currenciesList, 'en-US')}
          currencyIsActive
          onSelectCurrency={(code) =>
            store.set({
              currencySelected: currenciesList[code],
            })
          }
          onToggleCurrencyIsActive={action('onToggleCurrencyIsActive')}
          onOpenExternalLink={action('onOpenExternalLink')}
        />
      )
    )
  )
  .add('Stake Pools', () => (
    <StakePoolsSettings
      onSelectSmashServerUrl={action('onSelectSmashServerUrl')}
      onResetSmashServerError={action('onResetSmashServerError')}
      smashServerUrl="https://smash.cardano-mainnet.iohk.io"
      onOpenExternalLink={action('onOpenExternalLink')}
      isSyncing={boolean('isSyncing', false)}
      syncPercentage={number('syncPercentage', 70, {
        range: true,
        min: 0,
        max: 100,
        step: 1,
      })}
      isLoading={boolean('isLoading', false)}
    />
  ))
  .add('Themes', () => (
    <DisplaySettings
      theme="DarkBlue"
      selectTheme={({ theme }) => {
        updateParam({
          param: 'themeName',
          value: getParamName(themesIds, theme)[0],
        });
      }}
    />
  ))
  .add('Terms of Service', (props) => {
    const termsOfUseSource = require(`../../../../source/renderer/app/i18n/locales/terms-of-use/${props.locale}.md`);

    return (
      <TermsOfUseSettings
        localizedTermsOfUse={termsOfUseSource}
        onOpenExternalLink={() => null}
      />
    );
  })
  .add('Support', () => (
    <SupportSettings
      onExternalLinkClick={action('onExternalLinkClick')}
      onSupportRequestClick={action('onSupportRequestClick')}
      onDownloadLogs={action('onDownloadLogs')}
      disableDownloadLogs={boolean('disableDownloadLogs', false)}
    />
  ))
  .add('Security', () => (
    <SecuritySettings
      discreetMode={boolean('discreetMode', false)}
      openDiscreetMode={boolean('openDiscreetMode', false)}
      onDiscreetModeToggle={() =>
        // @ts-ignore ts-migrate(2554) FIXME: Expected 2-3 arguments, but got 1.
        changeControl('discreetMode', !boolean('discreetMode'))
      }
      onOpenDiscreetModeToggle={() =>
        // @ts-ignore ts-migrate(2554) FIXME: Expected 2-3 arguments, but got 1.
        changeControl('openDiscreetMode', !boolean('openDiscreetMode'))
      }
    />
  ));
