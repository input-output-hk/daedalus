import React from 'react';
import { defineMessages, injectIntl, intlShape } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import DRepCard from '../drep-directory/DRepCard';
import DRepDirectorySkeleton from '../drep-directory/DRepDirectorySkeleton';
import CurrentDRepSummary from '../../voting/voting-governance/CurrentDRepSummary';
import type { DRepDelegation } from '../../../api/wallets/types';
import type { AppDRepDirectoryEntry } from '../../../stores/GovernanceStore';
import styles from './GovernanceWallets.scss';

const messages = defineMessages({
  pageTitle: {
    id: 'governance.dashboard.pageTitle',
    defaultMessage: '!!!Wallets',
    description: 'Title of the governance wallets page',
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
  onToggleFavorite: (drepId: string) => void;
  onChangeDelegation: (walletId: string) => void;
  onChooseDRep: () => void;
  onViewDetails: (drepId: string, walletId: string) => void;
  intl: intlShape.isRequired;
};

function renderWalletContent(
  wallet: WalletDelegationSummary,
  favoriteDRepIds: Set<string>,
  onToggleFavorite: (drepId: string) => void,
  onChangeDelegation: (walletId: string) => void,
  onChooseDRep: () => void,
  onViewDetails: (drepId: string, walletId: string) => void,
  intl: any
) {
  const { walletId, currentDRep, drepEntry } = wallet;

  if (currentDRep == null) {
    return (
      <div className={styles.noDelegationCta}>
        <p className={styles.noDelegationHeading}>
          {intl.formatMessage(messages.noDelegationHeading)}
        </p>
        <p className={styles.noDelegationBody}>
          {intl.formatMessage(messages.noDelegationBody)}
        </p>
        <Button
          label={intl.formatMessage(messages.noDelegationButton)}
          onClick={onChooseDRep}
          skin={ButtonSkin}
        />
      </div>
    );
  }

  if (currentDRep.kind === 'drep') {
    if (drepEntry == null) {
      return <DRepDirectorySkeleton count={1} />;
    }
    return (
      <>
        <DRepCard
          entry={drepEntry}
          isFavorite={favoriteDRepIds.has(drepEntry.drepId)}
          onToggleFavorite={onToggleFavorite}
          isCurrentDRep
          onViewDetails={(drepId) => onViewDetails(drepId, walletId)}
          onSelectForDelegation={() => onChangeDelegation(walletId)}
        />
        <div className={styles.actions}>
          <Button
            label={intl.formatMessage(messages.changeDelegation)}
            onClick={() => onChangeDelegation(walletId)}
            skin={ButtonSkin}
          />
        </div>
      </>
    );
  }

  const isSentinel =
    currentDRep.kind === 'abstain' || currentDRep.kind === 'no_confidence';
  return (
    <>
      <CurrentDRepSummary currentDRep={currentDRep} />
      {isSentinel && (
        <p className={styles.chooseDRepPrompt}>
          {intl.formatMessage(messages.chooseDRepPrompt)}{' '}
          <button
            type="button"
            className={styles.chooseDRepLink}
            onClick={onChooseDRep}
          >
            {intl.formatMessage(messages.chooseDRepLink)}
          </button>
          .
        </p>
      )}
      <div className={styles.actions}>
        <Button
          label={intl.formatMessage(messages.changeDelegation)}
          onClick={() => onChangeDelegation(walletId)}
          skin={ButtonSkin}
        />
      </div>
    </>
  );
}

function GovernanceWallets({
  wallets,
  favoriteDRepIds,
  onToggleFavorite,
  onChangeDelegation,
  onChooseDRep,
  onViewDetails,
  intl,
}: Props) {
  return (
    <div className={styles.container}>
      <h2 className={styles.pageTitle}>
        {intl.formatMessage(messages.pageTitle)}
      </h2>
      {wallets.length === 0 ? (
        <p className={styles.emptyState}>
          {intl.formatMessage(messages.noWallets)}
        </p>
      ) : (
        <div className={styles.walletList}>
          {wallets.map((wallet) => (
            <div key={wallet.walletId} className={styles.walletSection}>
              <h3 className={styles.walletName}>{wallet.walletName}</h3>
              {renderWalletContent(
                wallet,
                favoriteDRepIds,
                onToggleFavorite,
                onChangeDelegation,
                onChooseDRep,
                onViewDetails,
                intl
              )}
            </div>
          ))}
        </div>
      )}
    </div>
  );
}

export default injectIntl(GovernanceWallets);
