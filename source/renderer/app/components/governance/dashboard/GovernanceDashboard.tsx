import React from 'react';
import { defineMessages, injectIntl, intlShape } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import CurrentVoteSummary from '../../voting/voting-governance/CurrentVoteSummary';
import type { WalletVotingTarget } from '../../../api/wallets/types';
import styles from './GovernanceDashboard.scss';

const messages = defineMessages({
  pageTitle: {
    id: 'governance.dashboard.pageTitle',
    defaultMessage: '!!!Governance Dashboard',
    description: 'Title of the governance dashboard page',
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
});

export type WalletDelegationSummary = {
  walletId: string;
  walletName: string;
  currentVote: WalletVotingTarget;
};

type Props = {
  wallets: WalletDelegationSummary[];
  onChangeDelegation: (walletId: string) => void;
  onChooseDRep: () => void;
  intl: intlShape.isRequired;
};

function GovernanceDashboard({
  wallets,
  onChangeDelegation,
  onChooseDRep,
  intl,
}: Props) {
  return (
    <div className={styles.container}>
      <h2 className={styles.pageTitle}>
        {intl.formatMessage(messages.pageTitle)}
      </h2>
      <div className={styles.walletList}>
        {wallets.map(({ walletId, walletName, currentVote }) => {
          const isSentinel =
            currentVote.kind === 'abstain' ||
            currentVote.kind === 'no_confidence';
          return (
            <div key={walletId} className={styles.walletSection}>
              <h3 className={styles.walletName}>{walletName}</h3>
              <CurrentVoteSummary currentVote={currentVote} />
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
            </div>
          );
        })}
      </div>
    </div>
  );
}

export default injectIntl(GovernanceDashboard);
