import React from 'react';
import BigNumber from 'bignumber.js';
import { defineMessages, injectIntl, intlShape } from 'react-intl';
import classNames from 'classnames';
import {
  formatVotingPower,
  getVotingPowerShare,
  isHighVotingPower,
  MINIMUM_DISPLAYED_SHARE,
} from './drepVotingPower';
import DRepInfoIcon from './DRepInfoIcon';
import styles from './DRepVotingPowerShare.scss';

const messages = defineMessages({
  label: {
    id: 'governance.drepDirectory.votingPowerShare.label',
    defaultMessage: '!!!{share}',
    description: 'Share of delegated voting power a single DRep holds',
  },
  tooltip: {
    id: 'governance.drepDirectory.votingPowerShare.tooltip',
    defaultMessage:
      '!!!This DRep controls {share} of active voting power ({total}).',
    description: 'Tooltip stating a DRep share of delegated voting power',
  },
  labelBelowMinimum: {
    id: 'governance.drepDirectory.votingPowerShare.labelBelowMinimum',
    defaultMessage: '!!!<{share}',
    description:
      'Share label for a DRep whose share rounds to zero at the displayed precision',
  },
  tooltipBelowMinimum: {
    id: 'governance.drepDirectory.votingPowerShare.tooltipBelowMinimum',
    defaultMessage:
      '!!!This DRep controls less than {share} of active voting power ({total}).',
    description:
      'Tooltip for a DRep whose share is too small to state at the displayed precision',
  },
});

interface Props {
  votingPower: BigNumber | null;
  totalDRepStake?: BigNumber | null;
  // A badge is a container for a figure that has none of its own, which is the
  // case beside an ADA amount on a card or in a table cell. In a labelled row
  // the label is already that container, and a chip inside it reads as a
  // second, competing one.
  variant?: 'badge' | 'plain';
  intl: intlShape.isRequired;
}

function DRepVotingPowerShare({
  votingPower,
  totalDRepStake,
  variant = 'badge',
  intl,
}: Props) {
  const share = getVotingPowerShare(votingPower, totalDRepStake);
  // Without the totals there is no share to state. The card and the detail view
  // both report the absolute voting power separately, so nothing is lost.
  if (share === null) return null;

  // The badge carries the figure alone. Spelling out "of voting power" beside
  // it doubled the badge's width and wrapped it, and the label it sits next to
  // already says what the figure is a share of. The tooltip still states it in
  // full, along with the absolute amount.
  const isHigh = isHighVotingPower(share);
  const asPercent = (value: number) =>
    intl.formatNumber(value, { style: 'percent', maximumFractionDigits: 2 });
  const formattedShare = asPercent(share);
  // A share small enough to round to zero is not zero, and saying so would
  // misstate it. Report it as under the smallest figure this precision shows.
  const isBelowMinimum = share > 0 && share < MINIMUM_DISPLAYED_SHARE;
  // The symbol rather than the word: mathematical comparison notation reads
  // the same in every script this app ships, so the badge stays a figure with
  // nothing in it to translate. The explanation spells it out in words, which
  // is what a screen reader announces and what the symbol alone cannot
  // guarantee.
  const minimumShare = asPercent(MINIMUM_DISPLAYED_SHARE);
  const label = isBelowMinimum
    ? intl.formatMessage(messages.labelBelowMinimum, { share: minimumShare })
    : intl.formatMessage(messages.label, { share: formattedShare });
  // The parenthetical is the denominator, not the numerator. Repeating this
  // DRep's own amount restated the figure sitting next to it and left the
  // share itself unanchored: a percentage means nothing without knowing what
  // it is a percentage of.
  const total = formatVotingPower(totalDRepStake ?? null);
  // No separate wording above the threshold. The share is the fact; whether
  // it is a lot is the reader's to judge, and the glyph and its colour say
  // that this one is worth a look without arguing a case.
  const resolveTooltip = () => {
    // The same rounding that would have printed 0% on the badge would have
    // printed it here, stating outright that a DRep with voting power holds
    // none of it.
    if (isBelowMinimum) {
      return intl.formatMessage(messages.tooltipBelowMinimum, {
        share: minimumShare,
        total,
      });
    }
    return intl.formatMessage(messages.tooltip, {
      share: formattedShare,
      total,
    });
  };
  const tooltip = resolveTooltip();

  // The figure alone gives a reader no reason to hover it, and now that it can
  // render as ordinary text there is nothing about it that suggests more is
  // available. The icon is that suggestion, and it carries the explanation
  // itself so the tooltip is reachable by keyboard rather than by pointer
  // alone. A share past the threshold gets the warning glyph: same control,
  // same wording, but not the colour of a neutral aside.
  return (
    // The explanation is carried by the whole chip, not only by the icon.
    // Fourteen pixels of glyph is a small thing to find with a pointer, and
    // the figure beside it is the part a reader is already looking at. The
    // icon keeps its own copy so it stays a focusable control with a name.
    <span
      className={classNames(
        variant === 'badge' ? styles.share : styles.plain,
        isHigh ? styles.high : styles.normal
      )}
      title={tooltip}
    >
      <span>{label}</span>
      <DRepInfoIcon
        explanation={tooltip}
        variant={isHigh ? 'warning' : 'info'}
      />
    </span>
  );
}

export default injectIntl(DRepVotingPowerShare);
