import React from 'react';
import { defineMessages, injectIntl, intlShape } from 'react-intl';
import globalMessages from '../../../i18n/global-messages';
import BigNumber from 'bignumber.js';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import DRepIdDisplay from '../_shared/DRepIdDisplay';
import DRepStatusBadge from '../_shared/DRepStatusBadge';
import DRepDirectorySkeleton from '../drep-directory/DRepDirectorySkeleton';
import CurrentDRepSummary from '../../voting/voting-governance/CurrentDRepSummary';
import type { DRepDelegation } from '../../../api/wallets/types';
import type { AppDRepDirectoryEntry } from '../../../stores/GovernanceStore';
import styles from './GovernanceWallets.scss';

const messages = defineMessages({
  columnWallet: {
    id: 'governance.dashboard.column.wallet',
    defaultMessage: '!!!Wallet',
    description: 'Column heading for the wallet name',
  },
  columnDelegatedTo: {
    id: 'governance.dashboard.column.delegatedTo',
    defaultMessage: '!!!Voting power delegated to',
    description: 'Column heading for the current governance delegation',
  },
  columnStatus: {
    id: 'governance.dashboard.column.status',
    defaultMessage: '!!!Status',
    description: 'Column heading for the delegated DRep status',
  },
  notDelegated: {
    id: 'governance.dashboard.notDelegated',
    defaultMessage: '!!!Not delegated',
    description: 'Shown for a wallet with no governance delegation',
  },
  abstainTarget: {
    id: 'governance.dashboard.target.abstain',
    defaultMessage: '!!!Abstain',
    description: 'Shown for a wallet delegated to the abstain target',
  },
  noConfidenceTarget: {
    id: 'governance.dashboard.target.noConfidence',
    defaultMessage: '!!!No confidence',
    description: 'Shown for a wallet delegated to the no-confidence target',
  },
  loadingTarget: {
    id: 'governance.dashboard.target.loading',
    defaultMessage: '!!!Loading DRep…',
    description: 'Shown while the delegated DRep is still being resolved',
  },
  noWallets: {
    id: 'governance.dashboard.noWallets',
    defaultMessage:
      '!!!Create or restore a wallet to participate in Cardano governance.',
    description: 'Empty state message shown when no wallets exist',
  },
  changeDelegation: {
    id: 'governance.dashboard.changeDelegation',
    defaultMessage: '!!!Change delegation',
    description: 'Button label to navigate to the delegation form for a wallet',
  },
  chooseDRepPrompt: {
    id: 'governance.dashboard.chooseDRepPrompt',
    defaultMessage: '!!!Help support Cardano governance by',
    description:
      'Prompt text preceding the "Choosing a DRep" link for abstain/no-confidence wallets',
  },
  chooseDRepLink: {
    id: 'governance.dashboard.chooseDRepLink',
    defaultMessage: '!!!Choosing a DRep',
    description: 'Link text in the choose-a-DRep prompt',
  },
  noDelegationHeading: {
    id: 'governance.dashboard.noDelegation.heading',
    defaultMessage: '!!!No DRep selected',
    description: 'Heading for wallets with no governance delegation',
  },
  noDelegationBody: {
    id: 'governance.dashboard.noDelegation.body',
    defaultMessage:
      '!!!Delegate your voting power to a DRep to participate in Cardano governance.',
    description: 'Body text for wallets with no governance delegation',
  },
  noDelegationButton: {
    id: 'governance.dashboard.noDelegation.button',
    defaultMessage: '!!!Choose a DRep',
    description: 'CTA button for wallets with no governance delegation',
  },
});

export type WalletDelegationSummary = {
  walletId: string;
  walletName: string;
  currentDRep: DRepDelegation | null;
  drepEntry: AppDRepDirectoryEntry | null;
};

type Props = {
  wallets: WalletDelegationSummary[];
  favoriteDRepIds: Set<string>;
  totalDRepStake?: BigNumber | null;
  onToggleFavorite: (drepId: string) => void;
  onChangeDelegation: (walletId: string) => void;
  onChooseDRep: () => void;
  onViewDetails: (drepId: string, walletId: string) => void;
  intl: intlShape.isRequired;
};

/**
 * One row per wallet, so the whole set can be read at a glance.
 *
 * This page previously rendered a full DRep card per wallet, which answered
 * "who is this DRep" well and "which of my wallets are delegated, and to whom"
 * badly. The staking delegation centre answers the same question in rows, and
 * this now does too.
 */
function WalletDelegationRow({
  wallet,
  onChangeDelegation,
  onChooseDRep,
  onViewDetails,
  intl,
}: {
  wallet: WalletDelegationSummary;
  onChangeDelegation: (walletId: string) => void;
  onChooseDRep: () => void;
  onViewDetails: (drepId: string, walletId: string) => void;
  intl: intlShape.isRequired;
}) {
  const { walletId, walletName, currentDRep, drepEntry } = wallet;

  const target = () => {
    if (currentDRep == null) {
      return (
        <span className={styles.notDelegated}>
          {intl.formatMessage(messages.notDelegated)}
        </span>
      );
    }
    if (currentDRep.kind === 'abstain') {
      return <span>{intl.formatMessage(messages.abstainTarget)}</span>;
    }
    if (currentDRep.kind === 'no_confidence') {
      return <span>{intl.formatMessage(messages.noConfidenceTarget)}</span>;
    }
    if (drepEntry == null) {
      return (
        <span className={styles.notDelegated}>
          {intl.formatMessage(messages.loadingTarget)}
        </span>
      );
    }
    return (
      <span className={styles.targetCell}>
        {drepEntry.verifiedName && (
          <span className={styles.targetName}>{drepEntry.verifiedName}</span>
        )}
        <DRepIdDisplay drepId={drepEntry.drepId} variant="single" />
      </span>
    );
  };

  const status = () => {
    if (currentDRep == null || currentDRep.kind !== 'drep' || drepEntry == null)
      return null;
    // One badge, three states. A DRep has to be active to be expiring, so
    // pairing the two restated the first, and pairing expiry with inactive
    // contradicted it.
    return (
      <DRepStatusBadge
        status={drepEntry.status}
        drepActivity={drepEntry.drepActivity}
      />
    );
  };

  return (
    <tr className={styles.row}>
      <td className={styles.cell}>
        <span className={styles.walletName}>{walletName}</span>
      </td>
      <td className={styles.cell}>{target()}</td>
      <td className={styles.cell}>{status()}</td>
      <td className={styles.actionsCell}>
        {currentDRep != null &&
          currentDRep.kind === 'drep' &&
          drepEntry != null && (
            <Button
              className="flat"
              label={intl.formatMessage(globalMessages.viewDetails)}
              onClick={() => onViewDetails(drepEntry.drepId, walletId)}
              skin={ButtonSkin}
            />
          )}
        <Button
          label={intl.formatMessage(
            currentDRep == null
              ? messages.noDelegationButton
              : messages.changeDelegation
          )}
          onClick={() =>
            currentDRep == null ? onChooseDRep() : onChangeDelegation(walletId)
          }
          skin={ButtonSkin}
        />
      </td>
    </tr>
  );
}

function GovernanceWallets({
  wallets,
  favoriteDRepIds,
  totalDRepStake = null,
  onToggleFavorite,
  onChangeDelegation,
  onChooseDRep,
  onViewDetails,
  intl,
}: Props) {
  return (
    <div className={styles.container}>
      {/* No page title here: the governance tab bar already names the page,
          the way the delegation center relies on its own tabs. */}
      {wallets.length === 0 ? (
        <p className={styles.emptyState}>
          {intl.formatMessage(messages.noWallets)}
        </p>
      ) : (
        <div className={styles.scrollContainer}>
          <table className={styles.walletTable}>
            <thead>
              <tr className={styles.headerRow}>
                <th className={styles.headerCell} scope="col">
                  {intl.formatMessage(messages.columnWallet)}
                </th>
                <th className={styles.headerCell} scope="col">
                  {intl.formatMessage(messages.columnDelegatedTo)}
                </th>
                <th className={styles.headerCell} scope="col">
                  {intl.formatMessage(messages.columnStatus)}
                </th>
                <th className={styles.headerCell} scope="col">
                  <span className={styles.visuallyHidden}>
                    {intl.formatMessage(messages.changeDelegation)}
                  </span>
                </th>
              </tr>
            </thead>
            <tbody>
              {wallets.map((wallet) => (
                <WalletDelegationRow
                  key={wallet.walletId}
                  wallet={wallet}
                  onChangeDelegation={onChangeDelegation}
                  onChooseDRep={onChooseDRep}
                  onViewDetails={onViewDetails}
                  intl={intl}
                />
              ))}
            </tbody>
          </table>
        </div>
      )}
    </div>
  );
}

export default injectIntl(GovernanceWallets);
