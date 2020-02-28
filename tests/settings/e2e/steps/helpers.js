// @flow
import { expect } from 'chai';
import {
  clickInputByLabel,
  clickOptionByValue,
  clickOptionByIndex,
  getInputValueByLabel,
} from '../../../common/e2e/steps/helpers';
import type { Daedalus } from '../../../types';

const DATA_LAYER_MIGRATION_ACCEPTANCE_COMPONENT = '.DataLayerMigrationForm_component';
const DEFAULT_LANGUAGE = 'en-US';
const INITIAL_SETTINGS_FORM = '.InitialSettings_component';
const TERMS_OF_USE_FORM = '.TermsOfUseForm_component';

declare var daedalus: Daedalus;

export const i18nHelpers = {
  formatMessage: async (
    client: Object,
    { id, values }: { id: string, values?: Object }
  ) => {
    const translation = await client.execute(
      (translationId, translationValues) => {
        const IntlProvider = require('react-intl').IntlProvider; // eslint-disable-line
        const locale = daedalus.stores.profile.currentLocale;
        const messages = daedalus.translations;
        const intlProvider = new IntlProvider(
          { locale, messages: messages[locale] },
          {}
        );
        return intlProvider
          .getChildContext()
          .intl.formatMessage({ id: translationId }, translationValues);
      },
      id,
      values || {}
    );
    return translation.value;
  },
  setActiveLanguage: async (
    client: Object,
    { language }: { language: string } = {}
  ) =>
    client.execute(value => {
      daedalus.actions.profile.updateUserLocalSetting.trigger({ param: 'locale', value });
    }, language || DEFAULT_LANGUAGE),
};

export const initialSettingsHelpers = {
  waitForVisible: async (
    client: Object,
    { isHidden }: { isHidden: boolean } = {}
  ) =>
    client.waitForVisible(INITIAL_SETTINGS_FORM, null, isHidden),
  ensureLanguageIsSelected: async (
    client: Object,
    { language }: { language: string } = {}
  ) => {
    await i18nHelpers.setActiveLanguage(client, { language });
    client.execute(
      () => daedalus.actions.profile.finishInitialScreenSettings.trigger()
    );
    await initialSettingsHelpers.waitForVisible(client, { isHidden: true });
  },
};

export const migrationHelpers = {
  waitForVisible: async (
    client: Object,
    { isHidden } : { isHidden: boolean } = {}
  ) =>
    client.waitForVisible(
      DATA_LAYER_MIGRATION_ACCEPTANCE_COMPONENT,
      null,
      isHidden
    ),
  acceptMigration: async (client: Object) => {
    await client.execute(() => {
      daedalus.actions.profile.acceptDataLayerMigration.trigger();
    });
    await migrationHelpers.waitForVisible(client, { isHidden: true });
  },
};

export const termsOfUseHelpers = {
  waitForVisible: async (
    client: Object,
    { isHidden } : { isHidden: boolean } = {}
  ) =>
    client.waitForVisible(TERMS_OF_USE_FORM, null, isHidden),
  acceptTerms: async (client: Object) => {
    await client.execute(() => {
      daedalus.actions.profile.acceptTermsOfUse.trigger();
    });
    await termsOfUseHelpers.waitForVisible(client, { isHidden: true });
  },
};

export const chooseCustomOptionsByValue = async function(
  numberValue: string,
  dateValue: string,
  timeValue: string,
) {
  await clickInputByLabel.call(this, 'Number format');
  await clickOptionByValue.call(this, numberValue);
  await clickInputByLabel.call(this, 'Date format');
  await clickOptionByValue.call(this, dateValue);
  await clickInputByLabel.call(this, 'Time format');
  await clickOptionByValue.call(this, timeValue);
}

export const chooseCustomOptionsByIndex = async function(
  numberIndex: number,
  dateIndex: number,
  timeIndex: number
) {
  await clickInputByLabel.call(this, 'Number format');
  await clickOptionByIndex.call(this, numberIndex);
  await clickInputByLabel.call(this, 'Date format');
  await clickOptionByIndex.call(this, dateIndex);
  await clickInputByLabel.call(this, 'Time format');
  await clickOptionByIndex.call(this, timeIndex);
}

export const getSelectedCustomOptions = async function() {
  const selectedValues = await this.client.execute(function() {
    const {
      currentNumberFormat,
      currentDateFormat,
      currentTimeFormat,
    } = daedalus.stores.profile;
    return {
      currentNumberFormat,
      currentDateFormat,
      currentTimeFormat,
    };
  });
  const {
    currentNumberFormat: selectedNumber,
    currentDateFormat: selectedDate,
    currentTimeFormat: selectedTime,
  } = selectedValues.value;
  return {
    selectedNumber,
    selectedDate,
    selectedTime,
  }
}
