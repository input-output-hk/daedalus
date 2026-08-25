import React from 'react';
import { defineMessages, injectIntl, intlShape } from 'react-intl';
import BigNumber from 'bignumber.js';
import { Link } from 'react-polymorph/lib/components/Link';
import { LinkSkin } from 'react-polymorph/lib/skins/simple/LinkSkin';
import globalMessages from '../../../i18n/global-messages';
import { getNetworkExplorerUrlByType } from '../../../utils/network';
import DRepStatusBadge from '../_shared/DRepStatusBadge';
import DRepVotingPowerShare from '../_shared/DRepVotingPowerShare';
import DRepSectionHeading from '../_shared/DRepSectionHeading';
import type { AppDRepDetail } from '../../../stores/GovernanceStore';
import styles from './DRepDetail.scss';

const messages = defineMessages({
  explorerLink: {
    id: 'governance.drepDetail.explorerLink',
    defaultMessage: '!!!View voting history on Cardano Explorer',
    description:
      'Link out to the DRep on the Cardano explorer, which carries the voting record',
  },
  title: {
    id: 'governance.drepDetail.onchain.title',
    defaultMessage: '!!!On-Chain Data',
    description: 'Heading of the on-chain section on the DRep detail view',
  },
  titleExplanation: {
    id: 'governance.drepDetail.onchain.title.explanation',
    defaultMessage:
      '!!!Read directly from the Cardano ledger by your local node.',
    description: 'Explains what the on-chain data section contains',
  },
  inactiveInLabel: {
    id: 'governance.drepDetail.inactiveIn',
    defaultMessage: '!!!Inactive in',
    description:
      'Label of the field naming how long before a DRep stops counting',
  },
  inactiveInValue: {
    id: 'governance.drepDetail.inactiveInEpochs',
    defaultMessage: '!!!{count, plural, one {# epoch} other {# epochs}}',
    description:
      'Epochs before a DRep goes inactive, read beside the Inactive in label',
  },
  votingPowerShareLabel: {
    id: 'governance.drepDetail.votingPowerShare',
    defaultMessage: '!!!Share of voting power',
    description: 'Label for the share of delegated voting power a DRep holds',
  },
  votingPowerShareUnavailable: {
    id: 'governance.drepDetail.votingPowerShare.unavailable',
    defaultMessage:
      '!!!DRep totals are unavailable, so this share cannot be calculated.',
    description:
      'Shown in place of the share when the network total is missing',
  },
  votingPowerLabel: {
    id: 'governance.drepDetail.votingPower',
    defaultMessage: '!!!Voting power',
    description: 'Label for the voting power field on the detail view',
  },
  votingPowerUnavailable: {
    id: 'governance.drepDetail.votingPower.unavailable',
    defaultMessage: '!!!Stake distribution unavailable, try again later.',
    description: 'Shown in place of the voting power when stake failed to load',
  },
});

interface Props {
  entry: AppDRepDetail;
  totalDRepStake?: BigNumber | null;
  network?: string;
  onOpenExternalLink?: (url: string) => void;
  intl: intlShape.isRequired;
}

// Detail-form rendering: full ADA with thousands separators, never
// abbreviated to a suffix the way the directory cards do.
function formatAdaExact(lovelace: BigNumber): string {
  return `₳ ${lovelace.div(1_000_000).toFormat()}`;
}

function DRepDetailOnchainSection({
  entry,
  totalDRepStake = null,
  network,
  onOpenExternalLink,
  intl,
}: Props) {
  const shareOfVotingPower =
    entry.votingPower != null && totalDRepStake != null ? (
      <DRepVotingPowerShare
        votingPower={entry.votingPower}
        totalDRepStake={totalDRepStake}
        variant="plain"
      />
    ) : null;

  return (
    <section
      className={styles.section}
      aria-label={intl.formatMessage(messages.title)}
    >
      <DRepSectionHeading
        title={intl.formatMessage(messages.title)}
        explanation={intl.formatMessage(messages.titleExplanation)}
      />
      <dl className={styles.fieldList}>
        <div className={styles.fieldRow}>
          <dt className={styles.fieldLabel}>
            {intl.formatMessage(globalMessages.status)}
          </dt>
          <dd className={styles.fieldValue}>
            <DRepStatusBadge
              status={entry.status}
              drepActivity={entry.drepActivity}
            />
          </dd>
        </div>
        <div className={styles.fieldRow}>
          <dt className={styles.fieldLabel}>
            {intl.formatMessage(messages.inactiveInLabel)}
          </dt>
          <dd className={styles.fieldValue}>
            {entry.status === 'active' && entry.drepActivity != null
              ? intl.formatMessage(messages.inactiveInValue, {
                  count: entry.drepActivity,
                })
              : '—'}
          </dd>
        </div>
        <div className={styles.fieldRow}>
          <dt className={styles.fieldLabel}>
            {intl.formatMessage(messages.votingPowerLabel)}
          </dt>
          <dd className={styles.fieldValue}>
            {entry.votingPower ? (
              <span className={styles.votingPowerAda}>
                {formatAdaExact(entry.votingPower)}
              </span>
            ) : (
              <span className={styles.mutedValue}>
                {intl.formatMessage(messages.votingPowerUnavailable)}
              </span>
            )}
          </dd>
        </div>
        <div className={styles.fieldRow}>
          <dt className={styles.fieldLabel}>
            {intl.formatMessage(messages.votingPowerShareLabel)}
          </dt>
          <dd className={styles.fieldValue}>
            {/* The share needs both halves of the division. When the DRep's
                own voting power is the missing half, the row above has already
                said so and repeating it here states one failure twice; only a
                missing network total is unaccounted for anywhere else. */}
            {shareOfVotingPower ??
              (entry.votingPower == null ? (
                '—'
              ) : (
                <span className={styles.mutedValue}>
                  {intl.formatMessage(messages.votingPowerShareUnavailable)}
                </span>
              ))}
          </dd>
        </div>
      </dl>
      {/* The wallet API serves a DRep's registration, stake and expiry, but
          nothing about how it has voted. The explorer is the only route to
          that from here, so the page points at it rather than pretending the
          record does not exist.

          A link rather than a button, matching the two other places the app
          sends someone off to a website: the explorer link on the paper wallet
          certificate and the stake pool homepage in its tooltip. Buttons act
          within the wallet and links leave it, and that distinction is worth
          keeping legible now that DRep metadata can carry links a stranger
          wrote. LinkSkin marks it as leaving with its own icon, so the link
          needs nothing added. */}
      {onOpenExternalLink && (
        <Link
          className={styles.explorerLink}
          label={intl.formatMessage(messages.explorerLink)}
          onClick={() =>
            onOpenExternalLink(
              getNetworkExplorerUrlByType('drep', entry.drepId, network)
            )
          }
          skin={LinkSkin}
        />
      )}
    </section>
  );
}

export default injectIntl(DRepDetailOnchainSection);
