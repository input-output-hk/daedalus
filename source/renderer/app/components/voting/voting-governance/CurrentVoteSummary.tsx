import React from 'react';
import { injectIntl, intlShape } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import DRepIdDisplay from '../../governance/_shared/DRepIdDisplay';
import DRepSourceLabel from '../../governance/_shared/DRepSourceLabel';
import type { WalletVotingTarget } from '../../../api/wallets/types';
import { messages } from './CurrentVoteSummary.messages';
import styles from './CurrentVoteSummary.scss';

type Props = {
  currentVote: WalletVotingTarget | null;
  intl: intlShape.isRequired;
};

// Status labels render through the local message set because
// DRepSourceLabel's variant union cannot express them; DRepSourceLabel is
// reused only for the on-chain source label on the DRep state.
function CurrentVoteSummary({ currentVote, intl }: Props) {
  if (currentVote == null) {
    return (
      <section
        className={styles.component}
        aria-label={intl.formatMessage(messages.noDelegationTitle)}
      >
        <h3 className={styles.header}>
          {intl.formatMessage(messages.noDelegationTitle)}
        </h3>
        <p className={styles.warning} role="alert">
          <span className={styles.warningGlyph} aria-hidden="true">
            !
          </span>
          {intl.formatMessage(messages.noDelegationWarning)}
        </p>
        <p className={styles.subline}>
          {intl.formatMessage(messages.noDelegationSubline)}
        </p>
        <div>
          <Button
            className={styles.cta}
            skin={ButtonSkin}
            label={intl.formatMessage(messages.noDelegationCta)}
          />
        </div>
      </section>
    );
  }

  if (currentVote.kind === 'drep') {
    return (
      <section
        className={styles.component}
        aria-label={intl.formatMessage(messages.headerCurrent)}
      >
        <h3 className={styles.header}>
          {intl.formatMessage(messages.headerCurrent)}
        </h3>
        <div className={styles.statusRow}>
          <span className={styles.statusBadge}>
            <span className={styles.glyph} aria-hidden="true">
              ●
            </span>
            {intl.formatMessage(messages.statusDelegatedToDRep)}
          </span>
          <DRepSourceLabel source="on-chain" className={styles.sourceLabel} />
        </div>
        <div className={styles.idRow}>
          <DRepIdDisplay drepId={currentVote.drep.raw} />
        </div>
      </section>
    );
  }

  const isAbstain = currentVote.kind === 'abstain';
  return (
    <section
      className={styles.component}
      aria-label={intl.formatMessage(messages.headerCurrent)}
    >
      <h3 className={styles.header}>
        {intl.formatMessage(messages.headerCurrent)}
      </h3>
      <div className={styles.statusRow}>
        <span className={styles.statusBadge}>
          <span className={styles.glyph} aria-hidden="true">
            {isAbstain ? '⊘' : '✕'}
          </span>
          {intl.formatMessage(
            isAbstain ? messages.statusAbstain : messages.statusNoConfidence
          )}
        </span>
      </div>
      <p className={styles.caption}>
        {intl.formatMessage(
          isAbstain ? messages.abstainCaption : messages.noConfidenceCaption
        )}
      </p>
    </section>
  );
}

export default injectIntl(CurrentVoteSummary);
