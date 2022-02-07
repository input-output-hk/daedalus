import humanizeDuration from 'humanize-duration';
import { get } from 'lodash';
import { humanizedDurationLanguages } from '../../../common/types/locales.types';

const generalConfig = {
  round: true,
  // round seconds to prevent e.g. 1 day 3 hours *11,56 seconds*
  units: ['d', 'h', 'm'],
};
const localeConfigs = {
  'en-US': {
    conjunction: ' and ',
    serialComma: false,
  },
  'ja-JP': {
    language: 'customJapanese',
    spacer: '',
    delimiter: '',
    languages: {
      customJapanese: {
        d: () => '日と',
        h: () => '時間',
        m: () => '分',
      },
    },
  },
};
export default (
  time: number,
  currentLocale: string,
  configOverride: Record<string, any> = {}
) => {
  const customLocaleConfig = get(configOverride, [
    'localeConfig',
    currentLocale,
  ]);
  const localeConfig = customLocaleConfig || localeConfigs[currentLocale];
  const language = humanizedDurationLanguages[currentLocale];
  const config = {
    ...generalConfig,
    language,
    ...localeConfig,
    ...configOverride,
  };
  return humanizeDuration(time, config);
};
