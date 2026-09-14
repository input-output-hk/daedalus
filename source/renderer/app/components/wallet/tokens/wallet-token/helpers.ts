/**
 * How strongly a disagreement between the user's decimal-place setting and the
 * issuer's published value is worth putting.
 *
 * A published value that was cryptographically bound to the token's minting
 * policy is a claim by the issuer that can be checked, and a setting that
 * contradicts it is worth saying plainly. A published value with no such
 * binding is a number nobody can vouch for, and saying the user is "not using
 * the recommended configuration" overstates it.
 */
export enum DecimalSettingDisagreement {
  None = 'none',
  WithVerified = 'withVerified',
  WithUnverified = 'withUnverified',
}

type DecimalSettingDisagreementArgs = {
  decimals: number | null | undefined;
  recommendedDecimals: number | null | undefined;
  /**
   * The verdict for `recommendedDecimals`. Read with `===`, so an argument
   * object built without it reports the weaker disagreement rather than the
   * stronger one.
   */
  recommendedDecimalsVerified?: boolean | null;
};

export const decimalSettingDisagreement = ({
  recommendedDecimals,
  decimals,
  recommendedDecimalsVerified,
}: DecimalSettingDisagreementArgs): DecimalSettingDisagreement => {
  const hasRecommendedDecimals = typeof recommendedDecimals === 'number';
  const hasConfiguredDecimals = typeof decimals === 'number';

  if (!hasRecommendedDecimals) {
    return DecimalSettingDisagreement.None;
  }

  const disagreement =
    recommendedDecimalsVerified === true
      ? DecimalSettingDisagreement.WithVerified
      : DecimalSettingDisagreement.WithUnverified;

  if (hasConfiguredDecimals) {
    return decimals !== recommendedDecimals
      ? disagreement
      : DecimalSettingDisagreement.None;
  }

  if (recommendedDecimals === 0) {
    return DecimalSettingDisagreement.None;
  }

  return disagreement;
};
