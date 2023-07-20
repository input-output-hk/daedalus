import React from 'react';
import { observer } from 'mobx-react';
import BorderedBox from '../../widgets/BorderedBox';
import styles from './VotingInfo.scss';
import ResultsPhase from './ResultsPhase';
import SnapshotPhase from './SnapshotPhase';
import VotingPhase from './VotingPhase';
import TallyingPhase from './TallyingPhase';
import Headline from './Headline';
import AppStore from './AppStore';
import RegisterToVote from './RegisterToVote';
import ApiError from './ApiError';
import { FundPhase } from '../../../stores/VotingStore';
import { VotingProps as Props, PhaseProps } from './types';

const phaseToComponentMap: { [key in FundPhase]: React.FC<PhaseProps> } = {
  [FundPhase.SNAPSHOT]: SnapshotPhase,
  [FundPhase.VOTING]: VotingPhase,
  [FundPhase.TALLYING]: TallyingPhase,
  [FundPhase.RESULTS]: ResultsPhase,
};

function VotingInfo({
  currentLocale,
  currentDateFormat,
  currentTimeFormat,
  fundPhase,
  fundInfo,
  onRegisterToVoteClick,
  onExternalLinkClick,
}: Props) {
  const PhaseComponent = phaseToComponentMap[fundPhase];
  return (
    <div className={styles.component}>
      <BorderedBox>
        <Headline onExternalLinkClick={onExternalLinkClick} />
        <hr className={styles.separator} />
        <div className={styles.bottomContent}>
          {fundPhase === null && <ApiError />}
          {fundPhase && (
            <>
              <div className={styles.leftContent}>
                <PhaseComponent
                  fundInfo={fundInfo}
                  currentLocale={currentLocale}
                  currentDateFormat={currentDateFormat}
                  currentTimeFormat={currentTimeFormat}
                  onExternalLinkClick={onExternalLinkClick}
                />
                <div className={styles.appStoreSpacing}>
                  <AppStore
                    onAppleStoreLinkClick={onExternalLinkClick}
                    onAndroidStoreLinkClick={onExternalLinkClick}
                  />
                </div>
              </div>
              <div className={styles.rightContent}>
                <RegisterToVote onRegisterToVoteClick={onRegisterToVoteClick} />
              </div>
            </>
          )}
        </div>
      </BorderedBox>
    </div>
  );
}

export default observer(VotingInfo);
