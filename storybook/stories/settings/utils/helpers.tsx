import { Store } from '@dump247/storybook-state';
import {
  DATE_ENGLISH_OPTIONS,
  LANGUAGE_OPTIONS,
  NUMBER_OPTIONS,
  TIME_OPTIONS,
} from '../../../../source/renderer/app/config/profileConfig';

export interface LocaleStoryStore {
  currentDateFormat: string;
  currentNumberFormat: string;
  currentTimeFormat: string;
  currentLocale: string;
}

export const mockedLocaleState = {
  currentDateFormat: DATE_ENGLISH_OPTIONS[0].value,
  currentNumberFormat: NUMBER_OPTIONS[0].value,
  currentTimeFormat: TIME_OPTIONS[0].value,
  currentLocale: LANGUAGE_OPTIONS[0].value,
};

export const onLocaleValueChange = (
  store: Store<LocaleStoryStore>,
  id: string,
  value: string
): void => {
  const fieldIdToStoreKeyMap = {
    dateFormat: 'currentDateFormat',
    numberFormat: 'currentNumberFormat',
    timeFormat: 'currentTimeFormat',
    locale: 'currentLocale',
  };

  store.set({
    [fieldIdToStoreKeyMap[id]]: value,
  });

  if (id === 'locale') {
    const currentDateFormat =
      value === mockedLocaleState.currentLocale
        ? mockedLocaleState.currentDateFormat
        : 'YYYY年MM月DD日';
    store.set({
      currentDateFormat,
    });
  }
};
