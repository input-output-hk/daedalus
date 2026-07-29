import enUS from '../../../source/renderer/app/i18n/locales/en-US.json';
import jaJP from '../../../source/renderer/app/i18n/locales/ja-JP.json';

// Copy that is still preliminary carries a leading `!!!` in every locale
// until the release-end review clears it. This one key's ja-JP copy was
// reviewed before the rule existed, so its en-US marker outlives its ja-JP
// one; it is the only permitted asymmetry.
const REVIEWED_JA_JP_EXCEPTIONS = [
  'wallet.settings.recoveryPhraseVerification.timeUntilWarningReplacement',
];

const CURRENT_VOTE_NAMESPACE = 'voting.governance.currentVote.';

// Only these two confirmation-dialog keys are preliminary; the rest of that
// namespace predates the feature and is legitimately unmarked.
const PRELIMINARY_CONFIRMATION_KEYS = [
  'voting.governance.confirmationDialog.drepIdCip105',
  'voting.governance.confirmationDialog.signedPayload',
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

  it('keeps the preliminary marker on every current-vote key in both locales', () => {
    const unmarked = Object.keys(en)
      .filter((key) => key.startsWith(CURRENT_VOTE_NAMESPACE))
      .filter(
        (key) => !en[key].startsWith('!!!') || !ja[key].startsWith('!!!')
      );
    expect(unmarked).toEqual([]);
  });

  it('keeps the preliminary marker on the new confirmation-dialog keys in both locales', () => {
    const unmarked = PRELIMINARY_CONFIRMATION_KEYS.filter(
      (key) => !en[key]?.startsWith('!!!') || !ja[key]?.startsWith('!!!')
    );
    expect(unmarked).toEqual([]);
  });
});
