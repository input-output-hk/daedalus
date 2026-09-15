/**
 * Where an applied decimal count came from.
 *
 * A user setting is an explicit choice made in the asset settings dialog and
 * stored per subject in browser storage. A verified registry value was published
 * by an issuer and cryptographically bound to the token's minting policy. There
 * is no third source that formats anything: an unverified published value is a
 * number nobody can vouch for, and a wrong one moves the decimal point on an
 * amount the user is about to sign.
 */
export enum AssetDecimalsProvenance {
  UserSetting = 'userSetting',
  VerifiedRegistry = 'verifiedRegistry',
  None = 'none',
}

export type ResolvedAssetDecimals = {
  decimals: number | null;
  provenance: AssetDecimalsProvenance;
};

type ResolvableDecimals = {
  /** The explicit per-subject setting, if the user has made one. */
  userDecimals?: number | null;
  /** The registry's published value, verified or not. */
  registryDecimals?: number | null;
  /** The verification verdict for that published value. */
  registryDecimalsVerified?: boolean;
};

/**
 * Resolves the decimal count that applies to a subject, highest source first:
 * an explicit user setting, then the registry's value if and only if it verified
 * against the minting policy, then none.
 *
 * `none` means raw units: the integers the chain holds, displayed and entered
 * without a separator. It is the honest rendering of a token whose denomination
 * nobody has attested, and it is what the amount field degrades to.
 *
 * An unverified published value is deliberately not returned here. It stays
 * available to the settings dialog as the recommended value, which is where the
 * user is asked to decide about it.
 */
export const resolveAssetDecimals = ({
  userDecimals,
  registryDecimals,
  registryDecimalsVerified,
}: ResolvableDecimals): ResolvedAssetDecimals => {
  if (typeof userDecimals === 'number') {
    return {
      decimals: userDecimals,
      provenance: AssetDecimalsProvenance.UserSetting,
    };
  }

  // `=== true` rather than a truthiness test. The field is optional on a merged
  // row and arrives `undefined` for a subject the cache has no verdict for, and
  // this is the comparison that says which single value unlocks formatting.
  if (
    registryDecimalsVerified === true &&
    typeof registryDecimals === 'number'
  ) {
    return {
      decimals: registryDecimals,
      provenance: AssetDecimalsProvenance.VerifiedRegistry,
    };
  }

  return { decimals: null, provenance: AssetDecimalsProvenance.None };
};
