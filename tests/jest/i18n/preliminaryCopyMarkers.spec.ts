import enUS from '../../../source/renderer/app/i18n/locales/en-US.json';
import jaJP from '../../../source/renderer/app/i18n/locales/ja-JP.json';

// The English copy on this branch has been reviewed and its markers cleared,
// and the translator's Japanese pass has returned, so every governance string
// carries real Japanese rather than the draft it was written against.
//
// This one key's ja-JP copy was reviewed before the rule existed, so its
// en-US marker outlives its ja-JP one; it is the only permitted asymmetry.
const REVIEWED_JA_JP_EXCEPTIONS = [
  'wallet.settings.recoveryPhraseVerification.timeUntilWarningReplacement',
];

const GOVERNANCE_NAMESPACES = ['governance.', 'voting.governance.'];

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

  it('defines every catalog key in both locales', () => {
    const missingInJa = Object.keys(en).filter((key) => !(key in ja));
    const missingInEn = Object.keys(ja).filter((key) => !(key in en));
    expect({ missingInEn, missingInJa }).toEqual({
      missingInEn: [],
      missingInJa: [],
    });
  });

  it('ships no governance string with a marker left on its English', () => {
    const marked = Object.keys(en)
      .filter((key) => GOVERNANCE_NAMESPACES.some((ns) => key.startsWith(ns)))
      .filter((key) => en[key].startsWith('!!!'));
    expect(marked).toEqual([]);
  });

  it('ships no governance string with a marker left on its Japanese', () => {
    // A marker here means a governance string was added or reworded after the
    // translator's pass and went out with drafted Japanese behind it.
    const marked = Object.keys(en)
      .filter((key) => GOVERNANCE_NAMESPACES.some((ns) => key.startsWith(ns)))
      .filter((key) => key in ja && ja[key].startsWith('!!!'));
    expect(marked).toEqual([]);
  });
});
