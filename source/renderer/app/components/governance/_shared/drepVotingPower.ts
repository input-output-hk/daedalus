import BigNumber from 'bignumber.js';

/**
 * The share of delegated voting power above which a single DRep is flagged.
 *
 * The figure is not invented. The Target15 campaign's goal is a CR10, the share
 * held by the ten largest DReps, of 15%, which puts each of the top ten at or
 * below roughly 1.5% of voting power. Measured against mainnet on 2026-08-20,
 * CR10 stood at 51.1% and this threshold marked 16 of 1,062 registered DReps,
 * narrow enough that the warning still means something.
 *
 * The denominator is `total_drep_stake` from the wallet's DRep summary, which
 * counts registered DReps only. Verified on preprod against Koios: the wallet's
 * figure matched the sum over real DReps to the lovelace, with `always_abstain`
 * and `always_no_confidence` excluded from it. No adjustment is needed here, and
 * folding the abstain target in would move the cutoff from roughly 79M to 228M
 * ADA and miss most of the DReps the threshold exists to mark.
 */
export const HIGH_VOTING_POWER_THRESHOLD = 0.015;

/**
 * The smallest share the label states outright. Anything smaller is reported as
 * being under this rather than rounded to zero, because a DRep with voting
 * power does not hold none of it.
 */
export const MINIMUM_DISPLAYED_SHARE = 0.0001;

const LOVELACE_PER_ADA = 1_000_000;

export function getVotingPowerShare(
  votingPower: BigNumber | null | undefined,
  totalDRepStake: BigNumber | null | undefined
): number | null {
  if (votingPower == null || totalDRepStake == null) return null;
  if (totalDRepStake.isLessThanOrEqualTo(0)) return null;
  return votingPower.dividedBy(totalDRepStake).toNumber();
}

export function isHighVotingPower(share: number | null): boolean {
  return share != null && share >= HIGH_VOTING_POWER_THRESHOLD;
}

/**
 * Human-rounded ADA with the currency glyph, for labels rather than totals.
 *
 * The billion step exists for the denominator: the stake delegated to every
 * DRep runs to thousands of millions, and "2,800.0M" is a figure a reader has
 * to convert before it means anything.
 */
export function formatVotingPower(value: BigNumber | null): string {
  if (!value) return '—';
  const ada = value.div(LOVELACE_PER_ADA);
  if (ada.isGreaterThanOrEqualTo(1_000_000_000)) {
    return `₳ ${ada.div(1_000_000_000).toFormat(1)}B`;
  }
  if (ada.isGreaterThanOrEqualTo(1_000_000)) {
    return `₳ ${ada.div(1_000_000).toFormat(1)}M`;
  }
  if (ada.isGreaterThanOrEqualTo(1_000)) {
    return `₳ ${ada.div(1_000).toFormat(1)}K`;
  }
  return `₳ ${ada.toFormat(0)}`;
}
