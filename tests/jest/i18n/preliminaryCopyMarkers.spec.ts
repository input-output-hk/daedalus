import enUS from '../../../source/renderer/app/i18n/locales/en-US.json';
import jaJP from '../../../source/renderer/app/i18n/locales/ja-JP.json';

// The English copy on this branch has been reviewed and its markers cleared.
// The Japanese has not: every governance string was drafted here rather than
// translated, so it keeps a leading `!!!` until the translator's pass returns.
//
// This one key's ja-JP copy was reviewed before the rule existed, so its
// en-US marker outlives its ja-JP one; it is the only permitted asymmetry.
const REVIEWED_JA_JP_EXCEPTIONS = [
  'wallet.settings.recoveryPhraseVerification.timeUntilWarningReplacement',
];

const GOVERNANCE_NAMESPACES = ['governance.', 'voting.governance.'];

// Governance strings that predate this work and that it did not reword. Their
// Japanese was translated rather than drafted, so it carries no marker.
const REVIEWED_GOVERNANCE_JA_JP = [
  'voting.governance.abstain',
  'voting.governance.confirmationDialog.title',
  'voting.governance.heading',
  'voting.governance.noConfidence',
  'voting.governance.paragraph1',
  'voting.governance.paragraph1LinkText',
  'voting.governance.paragraph1LinkUrl',
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

  it('keeps the marker on governance Japanese until the translator clears it', () => {
    // Drafted here rather than translated. Removing a marker is what says a
    // native speaker has read the string, so it must not happen by accident
    // when the English beside it is reworded.
    const unmarked = Object.keys(en)
      .filter((key) => GOVERNANCE_NAMESPACES.some((ns) => key.startsWith(ns)))
      .filter((key) => !REVIEWED_GOVERNANCE_JA_JP.includes(key))
      .filter((key) => key in ja && !ja[key].startsWith('!!!'));
    expect(unmarked).toEqual([]);
  });
});
