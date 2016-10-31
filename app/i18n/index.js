import { IntlProvider } from 'react-intl';
import store from '../store';
import translations from './translations';

const { locale } = store.i18n;

export const intlOptions = {
  locale,
  key: locale,
  messages: translations[locale]
};

export const { intl } = new IntlProvider(intlOptions, {}).getChildContext();
