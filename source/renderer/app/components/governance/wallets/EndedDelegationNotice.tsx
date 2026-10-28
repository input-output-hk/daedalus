import React from 'react';
import {
  defineMessages,
  FormattedMessage,
  injectIntl,
  intlShape,
} from 'react-intl';
import type BigNumber from 'bignumber.js';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import { DiscreetWalletAmount } from '../../../features/discreet-mode';
import { messages as currentVoteMessages } from '../../voting/voting-governance/CurrentDRepSummary.messages';
import { messages as heldRewardsMessages } from '../../notifications/HeldRewardsNotification';
import DRepIdDisplay from '../_shared/DRepIdDisplay';
import type { VoteDelegationState } from '../../../utils/governance/voteDelegationState';
import styles from './EndedDelegationNotice.scss';

const messages = defineMessages({
  title: {
    id: 'governance.dashboard.endedDelegation.title',
    defaultMessage: '!!!Voting power no longer delegated',
    description:
      'Title of the notice listing wallets whose vote delegation ended',
  },
});

export type EndedDelegation = {
  walletId: string;
  walletName: string;
  state: VoteDelegationState;
  heldReward: BigNumber;
};

type Props = {
  delegations: Array<EndedDelegation>;
  onDelegate: (walletId: string) => void;
  intl: intlShape.isRequired;
};

/**
 * The wallets whose voting power is no longer delegated, above the wallets
 * table. It is not dismissible: it stays until each wallet delegates again.
 */
function EndedDelegationNotice({ delegations, onDelegate, intl }: Props) {
  if (delegations.length === 0) return null;
  return (
    <section className={styles.component} role="status">
      <h2 className={styles.title}>{intl.formatMessage(messages.title)}</h2>
      {delegations.map(({ walletId, walletName, state, heldReward }) => {
        if (state.kind !== 'ended' && state.kind !== 'not_in_registry') {
          return null;
        }
        let text = currentVoteMessages.endedRetired;
        if (state.kind === 'not_in_registry') {
          text = currentVoteMessages.notInRegistry;
        } else if (state.registeredAgain) {
          text = currentVoteMessages.endedRetiredRegisteredAgain;
        }
        return (
          <div key={walletId} className={styles.wallet}>
            <div className={styles.walletName}>{walletName}</div>
            <p className={styles.description}>{intl.formatMessage(text)}</p>
            <DRepIdDisplay drepId={state.drep.raw} />
            {heldReward.gt(0) && (
              <p className={styles.description}>
                <FormattedMessage
                  {...currentVoteMessages.heldRewards}
                  values={{
                    rewardsAmount: <DiscreetWalletAmount amount={heldReward} />,
                  }}
                />
              </p>
            )}
            <div className={styles.actions}>
              <Button
                label={intl.formatMessage(heldRewardsMessages.delegateButton)}
                onClick={() => onDelegate(walletId)}
                skin={ButtonSkin}
              />
            </div>
          </div>
        );
      })}
    </section>
  );
}

export default injectIntl(EndedDelegationNotice);
