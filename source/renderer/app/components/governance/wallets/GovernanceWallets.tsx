import React from 'react';
import {
  defineMessages,
  FormattedMessage,
  injectIntl,
  intlShape,
} from 'react-intl';
import BigNumber from 'bignumber.js';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import { Link } from 'react-polymorph/lib/components/Link';
import { LinkSkin } from 'react-polymorph/lib/skins/simple/LinkSkin';
import { sharedGovernanceMessages } from '../../voting/voting-governance/shared-messages';
import globalMessages from '../../../i18n/global-messages';
import DRepIdDisplay from '../_shared/DRepIdDisplay';
import DRepStatusBadge from '../_shared/DRepStatusBadge';
import DRepDirectorySkeleton from '../drep-directory/DRepDirectorySkeleton';
import { drepLookupKey } from '../../../utils/governance/voteDelegationState';
import type {
  DRepLookup,
  VoteDelegationState,
} from '../../../utils/governance/voteDelegationState';
import EndedDelegationNotice from './EndedDelegationNotice';
import type { EndedDelegation } from './EndedDelegationNotice';
import styles from './GovernanceWallets.scss';

const messages = defineMessages({
  columnDelegatedTo: {
    id: 'governance.dashboard.column.delegatedTo',
    defaultMessage: '!!!Delegated to',
    description: 'Column heading for the current governance delegation',
  },
  loadingTarget: {
    id: 'governance.dashboard.target.loading',
    defaultMessage: '!!!Loading DRep…',
    description: 'Shown while the delegated DRep is still being resolved',
  },
  // Kept under the voting.governance namespace of the delegation form these
  // moved from: the ids are what carry the reviewed English and Japanese, and
  // renaming them for tidiness would send finished strings back for review.
  intro: {
    id: 'voting.governance.paragraph1',
    defaultMessage:
      '!!!Register your voting power to withdraw rewards. Learn more about {Link}.',
    description: 'Why a wallet needs a governance delegation at all',
  },
  introLinkUrl: {
    id: 'voting.governance.paragraph1LinkUrl',
    defaultMessage:
      '!!!https://docs.intersectmbo.org/cardano/cardano-governance/overview',
    description: 'Documentation URL behind the intro link',
  },
  introLinkText: {
    id: 'voting.governance.paragraph1LinkText',
    defaultMessage: '!!!Cardano Governance',
    description: 'Text of the intro link out to the governance documentation',
  },
  lastDelegatedTo: {
    id: 'governance.dashboard.target.lastDelegatedTo',
    defaultMessage: '!!!Last delegated to',
    description:
      'Wallets table, under "Not delegated", before the DRep ID of a delegation that ended',
  },
  statusRetired: {
    id: 'governance.dashboard.status.retired',
    defaultMessage: '!!!DRep retired',
    description:
      "Status column: the wallet's DRep retired, which ended its delegation",
  },
  statusRetiredRegisteredAgain: {
    id: 'governance.dashboard.status.retiredRegisteredAgain',
    defaultMessage: '!!!DRep retired and registered again',
    description:
      "Status column: the wallet's DRep retired and registered again; the delegation was not restored",
  },
  statusNotInRegistry: {
    id: 'governance.dashboard.status.notInRegistry',
    defaultMessage: '!!!Not in the DRep registry',
    description:
      "Status column: the wallet's DRep is not in the wallet's DRep registry, on a build that cannot read the ledger's delegation",
  },
  statusUnavailable: {
    id: 'governance.dashboard.status.unavailable',
    defaultMessage: '!!!DRep data unavailable',
    description: "Status column: the lookup of the wallet's DRep failed",
  },
  noWallets: {
    id: 'governance.dashboard.noWallets',
    defaultMessage:
      '!!!Create or restore a wallet to participate in Cardano governance.',
    description: 'Empty state message shown when no wallets exist',
  },
});

export type WalletDelegationSummary = {
  walletId: string;
  walletName: string;
  state: VoteDelegationState;
  // The lookup of the DRep the row shows.
  lookup: DRepLookup;
};

type Props = {
  wallets: WalletDelegationSummary[];
  endedDelegations?: Array<EndedDelegation>;
  favoriteDRepIds: Set<string>;
  totalDRepStake?: BigNumber | null;
  onToggleFavorite: (drepId: string) => void;
  onChangeDelegation: (walletId: string) => void;
  onViewDetails: (drepId: string, walletId: string) => void;
  onExternalLinkClick: (url: string, event?: React.MouseEvent) => void;
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
  onViewDetails,
  intl,
}: {
  wallet: WalletDelegationSummary;
  onChangeDelegation: (walletId: string) => void;
  onViewDetails: (drepId: string, walletId: string) => void;
  intl: intlShape.isRequired;
}) {
  const { walletId, walletName, state, lookup } = wallet;
  const entry = lookup.status === 'found' ? lookup.entry : null;
  const current = state.kind === 'current' ? state.voting : null;

  const drepId = (): string | null => {
    if (state.kind === 'ended' || state.kind === 'not_in_registry') {
      return drepLookupKey(state.drep);
    }
    if (current?.kind === 'drep') return drepLookupKey(current.drep);
    return null;
  };

  const target = () => {
    if (state.kind === 'none') {
      return (
        <span className={styles.notDelegated}>
          {intl.formatMessage(globalMessages.undelegated)}
        </span>
      );
    }
    if (current?.kind === 'abstain') {
      return (
        <span>{intl.formatMessage(sharedGovernanceMessages.abstain)}</span>
      );
    }
    if (current?.kind === 'no_confidence') {
      return (
        <span>{intl.formatMessage(sharedGovernanceMessages.noConfidence)}</span>
      );
    }
    if (state.kind === 'ended') {
      return (
        <span className={styles.targetCell}>
          <span className={styles.notDelegated}>
            {intl.formatMessage(globalMessages.undelegated)}
          </span>
          <span className={styles.notDelegated}>
            {intl.formatMessage(messages.lastDelegatedTo)}
          </span>
          <DRepIdDisplay drepId={drepId() ?? ''} variant="single" />
        </span>
      );
    }
    if (entry != null) {
      return (
        <span className={styles.targetCell}>
          {entry.verifiedName && (
            <span className={styles.targetName}>{entry.verifiedName}</span>
          )}
          <DRepIdDisplay drepId={entry.drepId} variant="single" />
        </span>
      );
    }
    if (
      state.kind === 'current' &&
      (lookup.status === 'idle' || lookup.status === 'pending')
    ) {
      return (
        <span className={styles.notDelegated}>
          {intl.formatMessage(messages.loadingTarget)}
        </span>
      );
    }
    return <DRepIdDisplay drepId={drepId() ?? ''} variant="single" />;
  };

  const status = () => {
    if (state.kind === 'ended') {
      return intl.formatMessage(
        state.registeredAgain
          ? messages.statusRetiredRegisteredAgain
          : messages.statusRetired
      );
    }
    if (state.kind === 'not_in_registry') {
      return intl.formatMessage(messages.statusNotInRegistry);
    }
    if (current?.kind !== 'drep') return null;
    if (entry == null) {
      return lookup.status === 'not_found' || lookup.status === 'failed'
        ? intl.formatMessage(messages.statusUnavailable)
        : null;
    }
    // One badge, three states. A DRep has to be active to be expiring, so
    // pairing the two restated the first, and pairing expiry with inactive
    // contradicted it.
    return (
      <DRepStatusBadge
        status={entry.status}
        drepActivity={entry.drepActivity}
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
        {current?.kind === 'drep' && entry != null && (
          <Button
            className="flat"
            label={intl.formatMessage(globalMessages.viewDetails)}
            onClick={() => onViewDetails(entry.drepId, walletId)}
            skin={ButtonSkin}
          />
        )}
        <Button
          label={intl.formatMessage(
            state.kind === 'none' || state.kind === 'ended'
              ? globalMessages.delegate
              : globalMessages.redelegate
          )}
          onClick={() => onChangeDelegation(walletId)}
          skin={ButtonSkin}
        />
      </td>
    </tr>
  );
}

function GovernanceWallets({
  wallets,
  endedDelegations = [],
  favoriteDRepIds,
  totalDRepStake = null,
  onToggleFavorite,
  onChangeDelegation,
  onViewDetails,
  onExternalLinkClick,
  intl,
}: Props) {
  const introLinkUrl = intl.formatMessage(messages.introLinkUrl);

  return (
    <div className={styles.container}>
      {/* No page title here: the governance tab bar already names the page,
          the way the delegation center relies on its own tabs.

          Why a delegation is needed belongs here, on the first governance
          screen anyone sees and the one that lists which wallets have none.
          It used to sit on the delegation form, which is reached only after
          choosing a wallet, opening the directory and picking a DRep: by then
          the reader has decided, and the one fact that might have prompted
          them arrives too late to act on. */}
      <p className={styles.intro}>
        <FormattedMessage
          {...messages.intro}
          values={{
            Link: (
              <Link
                className={styles.introLink}
                href={introLinkUrl}
                label={intl.formatMessage(messages.introLinkText)}
                onClick={(event) => onExternalLinkClick(introLinkUrl, event)}
                skin={LinkSkin}
              />
            ),
          }}
        />
      </p>
      <EndedDelegationNotice
        delegations={endedDelegations}
        onDelegate={onChangeDelegation}
      />
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
                  {intl.formatMessage(globalMessages.wallet)}
                </th>
                <th className={styles.headerCell} scope="col">
                  {intl.formatMessage(messages.columnDelegatedTo)}
                </th>
                <th className={styles.headerCell} scope="col">
                  {intl.formatMessage(globalMessages.status)}
                </th>
                <th className={styles.headerCell} scope="col">
                  <span className={styles.visuallyHidden}>
                    {intl.formatMessage(globalMessages.redelegate)}
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
