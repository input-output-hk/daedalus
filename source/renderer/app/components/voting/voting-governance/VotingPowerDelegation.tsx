import React, { useMemo, useState } from 'react';
import { observer } from 'mobx-react';
import { injectIntl } from 'react-intl';
import { Input } from 'react-polymorph/lib/components/Input';
import { Button } from 'react-polymorph/lib/components/Button';

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

type Props = {
  getStakePoolById: (...args: Array<any>) => any;
  intl: Intl;
  onExternalLinkClick: (...args: Array<any>) => any;
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
  wallets,
  stakePools,
}: Props) {
  const [selectedWalletId, setSelectedWalletId] = useState<string | null>(null);
  const [selectedVoteType, setSelectedVoteType] = useState<VoteType>('drep');
  const [drepInputState, setDrepInputState] = useState({
    blurred: false,
    value: '',
  });
  const drepInputIsValid = useMemo<boolean>(
    () => (drepInputState.blurred ? isDrepIdValid(drepInputState.value) : true),
    [drepInputState.blurred, drepInputState.value]
  );
  const formIsValid =
    !!selectedWalletId &&
    (selectedVoteType === 'drep'
      ? drepInputState.blurred && drepInputState.value && drepInputIsValid
      : true);
  const voteTypes: { value: VoteType; label: string }[] = [
    {
      value: 'abstain',
      label: 'Abstain',
    },
    {
      value: 'noConfidence',
      label: 'No confidence',
    },
    {
      value: 'drep',
      label: 'Delegate to dRep',
    },
  ];
  return (
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

        <WalletsDropdown
          className={styles.walletSelect}
          // @ts-ignore ts-migrate(2322) FIXME: Type '{ className: any; label: any; numberOfStakeP... Remove this comment to see the full error message
          label={'Select a wallet to delegate from'}
          numberOfStakePools={stakePools.length}
          wallets={wallets}
          onChange={(walletId: string) => setSelectedWalletId(walletId)}
          placeholder={'Select a wallet …'}
          value={selectedWalletId}
          getStakePoolById={getStakePoolById}
        />

        {selectedWalletId && (
          <ItemsDropdown
            className={styles.voteTypeSelect}
            label={'Select voting registration type'}
            placeholder={'Select delegation option'}
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
                blurred: false,
                value,
              });
            }}
            onBlur={() => {
              setDrepInputState((prevState) => ({
                ...prevState,
                blurred: true,
              }));
            }}
            spellCheck={false}
            value={drepInputState.value}
            label={
              <div>
                Please type or paste a valid DRep ID here. Look up{' '}
                <a
                  onClick={() =>
                    onExternalLinkClick('https://www.1694.io/en/dreps/list')
                  }
                >
                  DRep directory
                </a>
              </div>
            }
            placeholder={'Paste DRep ID here.'}
            error={drepInputIsValid ? undefined : 'Invalid DRep ID'}
          />
        )}
        <Button
          label={'Submit'}
          className={styles.voteSubmit}
          disabled={!formIsValid}
          onClick={() =>
            console.log('Submitting: ', {
              voteType: selectedVoteType,
              ...(selectedVoteType === 'drep'
                ? { drepId: drepInputState.value }
                : {}),
            })
          }
        />
      </BorderedBox>
    </div>
  );
}

export default injectIntl(observer(VotingPowerDelegation));
