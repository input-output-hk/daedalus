import { clickInputByLabel, clickOptionByValue, clickOptionByIndex } from "../../../common/e2e/steps/helpers";

const DATA_LAYER_MIGRATION_ACCEPTANCE_COMPONENT = '.DataLayerMigrationForm_component';
const DEFAULT_LANGUAGE = 'en-US';
const INITIAL_SETTINGS_FORM = '.InitialSettings_component';
const TERMS_OF_USE_FORM = '.TermsOfUseForm_component';
export const screenElementSelectors = {
  alert: {
    date: '.AlertsOverlay_date'
  },
  incident: {
    date: '.IncidentOverlay_date'
  },
  announcement: {
    date: '.NewsItem_newsItemDate'
  },
  info: {
    date: '.NewsItem_newsItemDate'
  },
  transaction: {
    date: '.WalletTransactionsList_groupDate',
    time: '.Transaction_type',
    number: '.Transaction_amount',
    transform: {
      time: (value: string) => value.split(',')[1]
    }
  },
  'transaction filter': {
    date: '.FilterDialog_fromDateInput input'
  },
  'send form': {
    number: '.SimpleInput_input[name="amount"]'
  },
  'Target Wallet': {
    number: '//*[@class="SidebarWalletMenuItem_title" and text()="Target Wallet"]//parent::div//following-sibling::div[@class="SidebarWalletMenuItem_info"]'
  }
};
const paramsMatchersValues = {
  date: (expectedValue: string) => expectedValue.replace('MM', '(0[1-9]|1[0-2])').replace('DD', '(0[1-9]|[12]\\d|3[01])').replace('YYYY', '\\d{4}'),
  time: (expectedValue: string) => expectedValue === 'hh:mm:ss A' ? '[0-1]\\d:[0-5]\\d(:[0-5]\\d)? [AP]M' : '[0-2]\\d:[0-5]\\d:([0-5]\\d)?',
  number: (expectedValue: string) => {
    let [thousandsSeparator, decimalSeparator] = expectedValue.split('');

    if (!decimalSeparator) {
      decimalSeparator = thousandsSeparator;
      thousandsSeparator = ' ';
    }

    return `((${thousandsSeparator})?\\d+)+${decimalSeparator}\\d{6}$`;
  }
};
export const i18nHelpers = {
  formatMessage: async (client: Record<string, any>, {
    id,
    values
  }: {
    id: string;
    values?: Record<string, any>;
  }) => {
    const translation = await client.execute((translationId, translationValues) => {
      const IntlProvider = require('react-intl').IntlProvider; // eslint-disable-line


      // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
      const locale = daedalus.stores.profile.currentLocale;
      // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
      const messages = daedalus.translations;
      const intlProvider = new IntlProvider({
        locale,
        messages: messages[locale]
      }, {});
      return intlProvider.getChildContext().intl.formatMessage({
        id: translationId
      }, translationValues);
    }, id, values || {});
    return translation.value;
  },
  // @ts-ignore ts-migrate(2741) FIXME: Property 'language' is missing in type '{}' but re... Remove this comment to see the full error message
  setActiveLanguage: async (client: Record<string, any>, {
    language
  }: {
    language: string;
  } = {}) => client.execute(value => {
    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
    daedalus.actions.profile.updateUserLocalSetting.trigger({
      param: 'locale',
      value
    });
  }, language || DEFAULT_LANGUAGE)
};
export const initialSettingsHelpers = {
  // @ts-ignore ts-migrate(2741) FIXME: Property 'isHidden' is missing in type '{}' but re... Remove this comment to see the full error message
  waitForVisible: async (client: Record<string, any>, {
    isHidden
  }: {
    isHidden: boolean;
  } = {}) => client.waitForVisible(INITIAL_SETTINGS_FORM, null, isHidden),
  // @ts-ignore ts-migrate(2741) FIXME: Property 'language' is missing in type '{}' but re... Remove this comment to see the full error message
  ensureLanguageIsSelected: async (client: Record<string, any>, {
    language
  }: {
    language: string;
  } = {}) => {
    await i18nHelpers.setActiveLanguage(client, {
      language
    });
    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
    client.execute(() => daedalus.actions.profile.finishInitialScreenSettings.trigger());
    await initialSettingsHelpers.waitForVisible(client, {
      isHidden: true
    });
  }
};
export const migrationHelpers = {
  // @ts-ignore ts-migrate(2741) FIXME: Property 'isHidden' is missing in type '{}' but re... Remove this comment to see the full error message
  waitForVisible: async (client: Record<string, any>, {
    isHidden
  }: {
    isHidden: boolean;
  } = {}) => client.waitForVisible(DATA_LAYER_MIGRATION_ACCEPTANCE_COMPONENT, null, isHidden),
  acceptMigration: async (client: Record<string, any>) => {
    await client.execute(() => {
      // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
      daedalus.actions.profile.acceptDataLayerMigration.trigger();
    });
    await migrationHelpers.waitForVisible(client, {
      isHidden: true
    });
  }
};
export const termsOfUseHelpers = {
  // @ts-ignore ts-migrate(2741) FIXME: Property 'isHidden' is missing in type '{}' but re... Remove this comment to see the full error message
  waitForVisible: async (client: Record<string, any>, {
    isHidden
  }: {
    isHidden: boolean;
  } = {}) => client.waitForVisible(TERMS_OF_USE_FORM, null, isHidden),
  acceptTerms: async (client: Record<string, any>) => {
    await client.execute(() => {
      // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
      daedalus.actions.profile.acceptTermsOfUse.trigger();
    });
    await termsOfUseHelpers.waitForVisible(client, {
      isHidden: true
    });
  }
};
export const chooseCustomOptionsByValue = async function (numberValue: string, dateValue: string, timeValue: string) {
  await clickInputByLabel.call(this, 'Number format');
  await clickOptionByValue.call(this, numberValue);
  await clickInputByLabel.call(this, 'Date format');
  await clickOptionByValue.call(this, dateValue);
  await clickInputByLabel.call(this, 'Time format');
  await clickOptionByValue.call(this, timeValue);
};
export const chooseCustomOptionsByIndex = async function (numberIndex: number, dateIndex: number, timeIndex: number) {
  await clickInputByLabel.call(this, 'Number format');
  await clickOptionByIndex.call(this, numberIndex);
  await clickInputByLabel.call(this, 'Date format');
  await clickOptionByIndex.call(this, dateIndex);
  await clickInputByLabel.call(this, 'Time format');
  await clickOptionByIndex.call(this, timeIndex);
};
export const getSelectedCustomOptions = async function () {
  const selectedValues = await this.client.execute(function () {
    const {
      currentNumberFormat,
      currentDateFormat,
      currentTimeFormat
    // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'daedalus'.
    } = daedalus.stores.profile;
    return {
      currentNumberFormat,
      currentDateFormat,
      currentTimeFormat
    };
  });
  const {
    currentNumberFormat: selectedNumber,
    currentDateFormat: selectedDate,
    currentTimeFormat: selectedTime
  } = selectedValues.value;
  return {
    selectedNumber,
    selectedDate,
    selectedTime
  };
};
export const getValueFromSelector = async function (screenElement: string, expectedParam: string) {
  const selector = screenElementSelectors[screenElement][expectedParam];
  const {transform} = screenElementSelectors[screenElement];
  const tagName = await this.client.getTagName(selector);
  let value;

  if (tagName === 'input') {
    value = await this.client.getValue(selector);
  } else {
    value = await this.client.getText(selector);
    if (Array.isArray(value)) value = value[0];
  }

  if (transform && transform[expectedParam]) value = transform[expectedParam](value);
  return value;
};
export const doesMatchExpectedValue = async function (screenElement: string, expectedParam: string, expectedValue: string) {
  const currentValue = await getValueFromSelector.call(this, screenElement, expectedParam);
  const expectedMatcher = new RegExp(paramsMatchersValues[expectedParam](expectedValue));
  return expectedMatcher.test(currentValue);
};