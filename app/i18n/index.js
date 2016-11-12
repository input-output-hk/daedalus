// @flow
import { IntlProvider } from 'react-intl';
import state from '../state';
import translations from './translations';

const { locale } = state.uiStore.i18n;

export const intlOptions = {
  locale,
  key: locale,
  messages: translations[locale]
};

export const { intl } = new IntlProvider(intlOptions, {}).getChildContext();
