import React from 'react';
import { boolean, number } from '@storybook/addon-knobs';
import { action } from '@storybook/addon-actions';
import { useGlobals } from '@storybook/preview-api';
import { withState } from '../../_support/WithLocalState';
import SettingsWrapper from '../utils/SettingsWrapper';
import { themesIds } from '../../_support/config';
// Screens
import ProfileSettingsForm from '../../../../source/renderer/app/components/widgets/forms/ProfileSettingsForm';
import StakePoolsSettings from '../../../../source/renderer/app/components/settings/categories/StakePoolsSettings';
import DisplaySettings from '../../../../source/renderer/app/components/settings/categories/DisplaySettings';
import SupportSettings from '../../../../source/renderer/app/components/settings/categories/SupportSettings';
import TermsOfUseSettings from '../../../../source/renderer/app/components/settings/categories/TermsOfUseSettings';
import WalletsSettings from '../../../../source/renderer/app/components/settings/categories/WalletsSettings';
import SecuritySettings from '../../../../source/renderer/app/components/settings/categories/SecuritySettings';
// Assets and helpers
import { mockedLocaleState, onLocaleValueChange } from '../utils/helpers';
import currenciesList from '../../../../source/renderer/app/config/currenciesList.json';
import { getLocalizedCurrenciesList } from '../../../../source/renderer/app/config/currencyConfig';

const mockedWalletsState = {
  currencyIsActive: true,
  currencySelected: {
    id: 'uniswap-state-dollar',
    code: 'usd',
    name: 'unified Stable Dollar',
  },
};

const mockedSecurityStore = {
  discreetMode: true,
  openDiscreetMode: false,
};

const getParamName = (obj, itemName): any =>
  Object.entries(obj).find((entry: [any, any]) => itemName === entry[1]);

export default {
  title: 'Settings / General',
  decorators: [SettingsWrapper],
};

export const General = withState(mockedLocaleState, (store) => (
  <ProfileSettingsForm
    isSubmitting={boolean('isSubmitting', false)}
    onSubmit={action('submit')}
    onChangeItem={(id, value) => onLocaleValueChange(store, id, value)}
    {...store.state}
  />
));

export const Wallets = withState(mockedWalletsState, (store) => (
  <WalletsSettings
    currencySelected={store.state.currencySelected}
    // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
    currencyRate={0.321}
    // @ts-ignore ts-migrate(2345) FIXME: Argument of type '{ aed: { code: string; decimalDi... Remove this comment to see the full error message
    currencyList={getLocalizedCurrenciesList(currenciesList, 'en-US')}
    onSelectCurrency={(code) =>
      store.set({
        currencySelected: currenciesList[code],
      })
    }
    onToggleCurrencyIsActive={(value) => store.set({ currencyIsActive: value })}
    onOpenExternalLink={action('onOpenExternalLink')}
    {...store.state}
  />
));

export const StakePools = () => (
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
);

export const Themes = () => {
  // The toolbar selection is a Storybook global now, so this writes back
  // through updateGlobals rather than over an addon channel.
  const [, updateGlobals] = useGlobals();
  return (
    <DisplaySettings
      theme="DarkBlue"
      selectTheme={({ theme }) => {
        updateGlobals({ themeName: getParamName(themesIds, theme)[0] });
      }}
    />
  );
};

export const TermsOfService = {
  render: (_, props) => {
    const termsOfUseSource = require(
      `../../../../source/renderer/app/i18n/locales/terms-of-use/${props.locale}.md`
    );

    return (
      <TermsOfUseSettings
        localizedTermsOfUse={termsOfUseSource}
        onOpenExternalLink={() => null}
      />
    );
  },

  name: 'Terms of Service',
};

export const Support = () => (
  <SupportSettings
    onExternalLinkClick={action('onExternalLinkClick')}
    onSupportRequestClick={action('onSupportRequestClick')}
    onDownloadLogs={action('onDownloadLogs')}
    disableDownloadLogs={boolean('disableDownloadLogs', false)}
    analyticsAccepted={boolean('analyticsAccepted', false)}
  />
);

export const Security = withState(mockedSecurityStore, (store) => (
  <SecuritySettings
    onDiscreetModeToggle={(value) => store.set({ discreetMode: value })}
    onOpenDiscreetModeToggle={(value) => store.set({ openDiscreetMode: value })}
    {...store.state}
  />
));
