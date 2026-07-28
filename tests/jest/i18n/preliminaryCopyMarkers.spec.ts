import enUS from '../../../source/renderer/app/i18n/locales/en-US.json';
import jaJP from '../../../source/renderer/app/i18n/locales/ja-JP.json';

// Copy that is still preliminary carries a leading `!!!` in every locale
// until the release-end review clears it. This one key's ja-JP copy was
// reviewed before the rule existed, so its en-US marker outlives its ja-JP
// one; it is the only permitted asymmetry.
const REVIEWED_JA_JP_EXCEPTIONS = [
  'wallet.settings.recoveryPhraseVerification.timeUntilWarningReplacement',
];

const en: Record<string, string> = enUS;
const ja: Record<string, string> = jaJP;

describe('preliminary copy markers', () => {
  it('keeps the ja-JP !!! marker on every key whose en-US copy is still preliminary', () => {
    const unmarked = Object.keys(en).filter(
      (key) =>
        key in ja &&
        en[key].startsWith('!!!') &&
        !ja[key].startsWith('!!!') &&
        !REVIEWED_JA_JP_EXCEPTIONS.includes(key)
    );
    expect(unmarked).toEqual([]);
  });
});
