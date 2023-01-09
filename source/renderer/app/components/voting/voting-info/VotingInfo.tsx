import React from 'react';
import { injectIntl } from 'react-intl';
import type { Intl } from '../../../types/i18nTypes';
import BorderedBox from '../../widgets/BorderedBox';
import styles from './VotingInfo.scss';
import Headline from './Headline';
import AppStore from './AppStore';
import RegisterToVote from './RegisterToVote';
import { ExternalLinkButton } from '../../widgets/ExternalLinkButton';
import { messages } from './VotingInfo.messages';

export type Props = {
  intl: Intl;
  onRegisterToVoteClick: (...args: Array<any>) => any;
  onExternalLinkClick: (...args: Array<any>) => any;
};

function VotingInfo({
  intl,
  onRegisterToVoteClick,
  onExternalLinkClick,
}: Props) {
  return (
    <div className={styles.component}>
      <BorderedBox>
        <Headline onExternalLinkClick={onExternalLinkClick} />
        <hr className={styles.separator} />
        <div className={styles.bottomContent}>
          <div className={styles.leftContent}>
            <div className={styles.appStoreSpacing}>
              <AppStore
                onAppleStoreLinkClick={onExternalLinkClick}
                onAndroidStoreLinkClick={onExternalLinkClick}
              />
            </div>
            <div className={styles.resultsButton}>
              <ExternalLinkButton
                label={intl.formatMessage(messages.viewResultsLinkLabel)}
                onClick={() =>
                  onExternalLinkClick(
                    intl.formatMessage(messages.viewResultsLinkURL)
                  )
                }
              />
            </div>
          </div>
          <div className={styles.rightContent}>
            <RegisterToVote onRegisterToVoteClick={onRegisterToVoteClick} />
          </div>
        </div>
      </BorderedBox>
    </div>
  );
}

export default injectIntl(VotingInfo);
