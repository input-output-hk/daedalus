import React, { useMemo, useState } from 'react';
import { observer } from 'mobx-react';
import { injectIntl } from 'react-intl';
import { Input } from 'react-polymorph/lib/components/Input';
import { Button } from 'react-polymorph/lib/components/Button';

import BigNumber from 'bignumber.js';
import BorderedBox from '../../widgets/BorderedBox';
import { messages } from './VotingPowerDelegation.messages';
import styles from './VotingPowerDelegation.scss';
import type { Intl } from '../../../types/i18nTypes';
import { FormattedHTMLMessageWithLink } from '../../widgets/FormattedHTMLMessageWithLink';
import WalletsDropdown from '../../widgets/forms/WalletsDropdown';
import Wallet from '../../../domains/Wallet';
import StakePool from '../../../domains/StakePool';
import ItemsDropdown from '../../widgets/forms/ItemsDropdown';
import { assertIsBech32WithPrefix } from '../../../../../common/utils/assertIsBech32WithPrefix';
import { VotingPowerDelegationConfirmationDialog } from './VotingPowerDelegationConfirmationDialog';
import type { DelegateVotesParams } from '../../../api/voting/types';
import { Separator } from '../../widgets/separator/Separator';

type Props = {
  getStakePoolById: (...args: Array<any>) => any;
  intl: Intl;
  onExternalLinkClick: (...args: Array<any>) => any;
  onSubmit: (params: DelegateVotesParams) => void;
  stakePools: Array<StakePool>;
  wallets: Array<Wallet>;
};

type VoteType = 'abstain' | 'noConfidence' | 'drep';

// TODO discuss if we need to restrict the length
const isDrepIdValid = (drepId: string) => {
  try {
    assertIsBech32WithPrefix(drepId, ['drep', 'drep_script']);
  } catch (e) {
    return false;
  }

  return true;
};

function VotingPowerDelegation({
  getStakePoolById,
  intl,
  onExternalLinkClick,
  onSubmit,
  wallets,
  stakePools,
}: Props) {
  const [confirmationDialogVisible, setConfirmationDialogVisible] = useState(
    false
  );
  const [selectedWalletId, setSelectedWalletId] = useState<string | null>(null);
  const [selectedVoteType, setSelectedVoteType] = useState<VoteType>('drep');
  const [drepInputState, setDrepInputState] = useState({
    dirty: false,
    value: '',
  });
  const drepInputIsValid = useMemo<boolean>(
    () => (drepInputState.dirty ? isDrepIdValid(drepInputState.value) : true),
    [drepInputState.dirty, drepInputState.value]
  );
  const formIsValid =
    !!selectedWalletId &&
    (selectedVoteType === 'drep'
      ? drepInputState.dirty && drepInputState.value && drepInputIsValid
      : true);
  const voteTypes: { value: VoteType; label: string }[] = [
    {
      value: 'abstain',
      label: intl.formatMessage(messages.abstain),
    },
    {
      value: 'noConfidence',
      label: intl.formatMessage(messages.noConfidence),
    },
    {
      value: 'drep',
      label: intl.formatMessage(messages.delegateToDRep),
    },
  ];
  return (
    <>
      <div className={styles.component}>
        <BorderedBox>
          <h1 className={styles.heading}>
            {intl.formatMessage(messages.heading)}
          </h1>
          <div className={styles.info}>
            <p>
              <FormattedHTMLMessageWithLink
                message={{
                  ...messages.paragraph1,
                  values: {
                    linkLabel: messages.learnMoreLinkLabel,
                    linkURL: messages.paragraph1LinkUrl,
                  },
                }}
                onExternalLinkClick={onExternalLinkClick}
              />
            </p>
          </div>

          <Separator />

          <WalletsDropdown
            className={styles.walletSelect}
            // @ts-ignore ts-migrate(2322) FIXME: Type '{ className: any; label: any; numberOfStakeP... Remove this comment to see the full error message
            label={intl.formatMessage(messages.selectWalletLabel)}
            numberOfStakePools={stakePools.length}
            wallets={wallets}
            onChange={(walletId: string) => setSelectedWalletId(walletId)}
            placeholder={intl.formatMessage(messages.selectWalletPlaceholder)}
            value={selectedWalletId}
            getStakePoolById={getStakePoolById}
          />

          {selectedWalletId && (
            <ItemsDropdown
              className={styles.voteTypeSelect}
              label={intl.formatMessage(messages.selectVotingTypeLabel)}
              options={voteTypes}
              handleChange={(option) => setSelectedVoteType(option.value)}
              value={selectedVoteType}
            />
          )}

          {selectedWalletId && selectedVoteType === 'drep' && (
            <Input
              className={styles.drepInput}
              onChange={(value) => {
                setDrepInputState({
                  dirty: true,
                  value,
                });
              }}
              spellCheck={false}
              value={drepInputState.value}
              label={
                <div>
                  {intl.formatMessage(messages.drepInputLabel)}{' '}
                  <a
                    href="#"
                    onClick={(event) => {
                      event.preventDefault();
                      onExternalLinkClick('https://www.1694.io/en/dreps/list');
                    }}
                  >
                    {intl.formatMessage(messages.drepInputLabelLink)}
                  </a>
                </div>
              }
              placeholder={intl.formatMessage(messages.drepInputPlaceholder)}
              error={
                drepInputIsValid
                  ? undefined
                  : intl.formatMessage(messages.drepInputError)
              }
            />
          )}
          <Button
            label={intl.formatMessage(messages.submitLabel)}
            className={styles.voteSubmit}
            disabled={!formIsValid}
            onClick={() => setConfirmationDialogVisible(true)}
          />
        </BorderedBox>
      </div>
      {confirmationDialogVisible && (
        <VotingPowerDelegationConfirmationDialog
          fees={new BigNumber('1')}
          onClose={() => setConfirmationDialogVisible(false)}
          onConfirm={(passphrase) =>
            onSubmit({
              walletId: selectedWalletId,
              passphrase,
              dRepId: drepInputState.value || selectedVoteType,
            })
          }
          selectedWallet={wallets.find((w) => w.id === selectedWalletId)}
          vote={drepInputState.value || selectedVoteType}
        />
      )}
    </>
  );
}

export default injectIntl(observer(VotingPowerDelegation));
