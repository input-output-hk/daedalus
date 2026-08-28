import React from 'react';
import { defineMessages, injectIntl, intlShape } from 'react-intl';
import NormalSwitch from '../../widgets/forms/NormalSwitch';
import DRepFacetSelect from '../_shared/DRepFacetSelect';
import DRepFacetNumber from '../_shared/DRepFacetNumber';
import { DREP_COHORT_SIZE_OPTIONS } from '../_shared/drepCohort';
import type { DRepCohortCriteria } from '../_shared/drepCohort';
import { governanceSharedMessages } from '../_shared/governanceSharedMessages';
import { drepCriteriaMessages } from '../_shared/drepCriteriaMessages';
import styles from './DRepCohortCriteriaPanel.scss';

const messages = defineMessages({
  votingPowerDecrement: {
    id: 'governance.drepDirectory.cohort.votingPowerDecrement',
    defaultMessage: '!!!Lower the voting power ceiling',
    description: 'Accessible label of the button that lowers the ceiling',
  },
  votingPowerIncrement: {
    id: 'governance.drepDirectory.cohort.votingPowerIncrement',
    defaultMessage: '!!!Raise the voting power ceiling',
    description: 'Accessible label of the button that raises the ceiling',
  },
  votingPowerLabel: {
    id: 'governance.drepDirectory.cohort.votingPowerLabel',
    defaultMessage: '!!!Voting Power Threshold',
    description: 'Label of the suggestion voting-power ceiling control',
  },
  sizeLabel: {
    id: 'governance.drepDirectory.cohort.sizeLabel',
    defaultMessage: '!!!Suggestions',
    description: 'Label of the control setting how many DReps are suggested',
  },
});

/**
 * The ceiling a reader can dial, as a percentage of governance stake.
 *
 * Half a percent at the bottom because a ceiling below that admits almost
 * nobody; a hundred at the top because that is every vote there is, so it
 * excludes no DRep and stands in for having no ceiling at all.
 */
const MIN_SHARE_PERCENT = 0.5;
const MAX_SHARE_PERCENT = 100;
const SHARE_STEP_PERCENT = 0.5;
const SHARE_DECIMALS = 2;

interface Props {
  criteria: DRepCohortCriteria;
  onCriteriaChange: (criteria: DRepCohortCriteria) => void;
  intl: intlShape.isRequired;
}

function DRepCohortCriteriaPanel({ criteria, onCriteriaChange, intl }: Props) {
  const toggle = (
    label: string,
    checked: boolean,
    onChange: (checked: boolean) => void
  ) => (
    <div className={styles.toggleFacet} key={label}>
      <span className={styles.toggleLabel}>{label}</span>
      <div className={styles.toggleControl}>
        <NormalSwitch
          className={styles.toggle}
          checked={checked}
          onChange={onChange}
        />
      </div>
    </div>
  );

  const facet = (
    label: string,
    value: string,
    onChange: (next: string) => void,
    options: Array<[string, string]>
  ) => (
    <DRepFacetSelect
      key={label}
      label={label}
      value={value}
      onChange={onChange}
      options={options}
      narrow
    />
  );

  return (
    /* No row of its own: these sit in the row the mode control leads,
       beside the filters that take their place in the other mode. */
    <>
      {/* The two switches wrap as a pair. Left to wrap independently, a
          narrow pane put one of them beside the mode control and the other on
          the line below, which read as two unrelated settings rather than the
          two halves of one question. */}
      <div className={styles.toggleGroup}>
        {toggle(
          intl.formatMessage(governanceSharedMessages.inactiveSoon),
          criteria.includeInactiveSoon,
          (includeInactiveSoon) =>
            onCriteriaChange({ ...criteria, includeInactiveSoon })
        )}
        {toggle(
          intl.formatMessage(drepCriteriaMessages.verifiedMetadata),
          criteria.requireVerifiedMetadata,
          (requireVerifiedMetadata) =>
            onCriteriaChange({ ...criteria, requireVerifiedMetadata })
        )}
      </div>
      {/* Typed rather than picked from three fixed shares. Where a
            reasonable ceiling sits depends on how stake is spread on the
            network the wallet is pointed at, which is not something a list
            written here can anticipate. The top of the range excludes nothing,
            since no DRep controls every vote, so it doubles as no ceiling at
            all and there is no separate option meaning the same thing. */}
      <DRepFacetNumber
        label={intl.formatMessage(messages.votingPowerLabel)}
        value={
          criteria.maxVotingPowerShare == null
            ? MAX_SHARE_PERCENT
            : Number(
                (criteria.maxVotingPowerShare * 100).toFixed(SHARE_DECIMALS)
              )
        }
        min={MIN_SHARE_PERCENT}
        max={MAX_SHARE_PERCENT}
        step={SHARE_STEP_PERCENT}
        suffix="%"
        decrementLabel={intl.formatMessage(messages.votingPowerDecrement)}
        incrementLabel={intl.formatMessage(messages.votingPowerIncrement)}
        onChange={(percent) =>
          onCriteriaChange({
            ...criteria,
            maxVotingPowerShare:
              percent >= MAX_SHARE_PERCENT ? null : percent / 100,
          })
        }
      />
      {facet(
        intl.formatMessage(messages.sizeLabel),
        String(criteria.size),
        (next) => onCriteriaChange({ ...criteria, size: Number(next) }),
        DREP_COHORT_SIZE_OPTIONS.map((size): [string, string] => [
          String(size),
          intl.formatNumber(size),
        ])
      )}
    </>
  );
}

export default injectIntl(DRepCohortCriteriaPanel);
