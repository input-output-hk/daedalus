import React from 'react';
import { defineMessages, injectIntl, intlShape } from 'react-intl';
import NormalSwitch from '../../widgets/forms/NormalSwitch';
import DRepFacetSelect from '../_shared/DRepFacetSelect';
import { LAPSING_SOON_EPOCHS } from '../_shared/drepExpiry';
import {
  DREP_COHORT_SIZE_OPTIONS,
  DREP_COHORT_VOTING_POWER_SHARE_OPTIONS,
} from '../_shared/drepCohort';
import type { DRepCohortCriteria } from '../_shared/drepCohort';
import styles from './DRepCohortCriteriaPanel.scss';

const messages = defineMessages({
  intro: {
    id: 'governance.drepDirectory.cohort.intro',
    defaultMessage:
      '!!!Suggestions are drawn at random from the DReps that meet these criteria.',
    description: 'Explanation above the suggestion criteria controls',
  },
  activeOnly: {
    id: 'governance.drepDirectory.cohort.activeOnly',
    defaultMessage: '!!!Active registration',
    description: 'Criterion restricting suggestions to active DReps',
  },
  excludeLapsingSoon: {
    id: 'governance.drepDirectory.cohort.excludeLapsingSoon',
    defaultMessage: '!!!Not lapsing within {epochs} epochs',
    description: 'Criterion excluding DReps whose voting power lapses soon',
  },
  requireVerifiedMetadata: {
    id: 'governance.drepDirectory.cohort.requireVerifiedMetadata',
    defaultMessage: '!!!Verified metadata',
    description: 'Criterion restricting suggestions to DReps with metadata',
  },
  votingPowerLabel: {
    id: 'governance.drepDirectory.cohort.votingPowerLabel',
    defaultMessage: '!!!Voting power under',
    description: 'Label of the suggestion voting-power ceiling control',
  },
  votingPowerNoLimit: {
    id: 'governance.drepDirectory.cohort.votingPowerNoLimit',
    defaultMessage: '!!!No limit',
    description: 'Option removing the suggestion voting-power ceiling',
  },
  sizeLabel: {
    id: 'governance.drepDirectory.cohort.sizeLabel',
    defaultMessage: '!!!Suggestions shown',
    description: 'Label of the control setting how many DReps are suggested',
  },
});

/** Sentinel for "no ceiling" in the select, whose values must be strings. */
const NO_LIMIT = 'none';

interface Props {
  criteria: DRepCohortCriteria;
  onCriteriaChange: (criteria: DRepCohortCriteria) => void;
  intl: intlShape.isRequired;
}

function DRepCohortCriteriaPanel({ criteria, onCriteriaChange, intl }: Props) {
  const asPercent = (value: number) =>
    intl.formatNumber(value, { style: 'percent', maximumFractionDigits: 2 });

  const toggle = (
    label: string,
    checked: boolean,
    onChange: (checked: boolean) => void
  ) => (
    <NormalSwitch
      className={styles.toggle}
      checked={checked}
      label={label}
      onChange={onChange}
    />
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
    />
  );

  return (
    <div className={styles.container}>
      <p className={styles.intro}>{intl.formatMessage(messages.intro)}</p>
      <div className={styles.toggles}>
        {toggle(
          intl.formatMessage(messages.activeOnly),
          criteria.activeOnly,
          (activeOnly) => onCriteriaChange({ ...criteria, activeOnly })
        )}
        {toggle(
          intl.formatMessage(messages.excludeLapsingSoon, {
            epochs: LAPSING_SOON_EPOCHS,
          }),
          criteria.excludeLapsingSoon,
          (excludeLapsingSoon) =>
            onCriteriaChange({ ...criteria, excludeLapsingSoon })
        )}
        {toggle(
          intl.formatMessage(messages.requireVerifiedMetadata),
          criteria.requireVerifiedMetadata,
          (requireVerifiedMetadata) =>
            onCriteriaChange({ ...criteria, requireVerifiedMetadata })
        )}
      </div>
      <div className={styles.facets}>
        {facet(
          intl.formatMessage(messages.votingPowerLabel),
          criteria.maxVotingPowerShare == null
            ? NO_LIMIT
            : String(criteria.maxVotingPowerShare),
          (next) =>
            onCriteriaChange({
              ...criteria,
              maxVotingPowerShare: next === NO_LIMIT ? null : Number(next),
            }),
          [
            [NO_LIMIT, intl.formatMessage(messages.votingPowerNoLimit)],
            ...DREP_COHORT_VOTING_POWER_SHARE_OPTIONS.map(
              (share): [string, string] => [String(share), asPercent(share)]
            ),
          ]
        )}
        {facet(
          intl.formatMessage(messages.sizeLabel),
          String(criteria.size),
          (next) => onCriteriaChange({ ...criteria, size: Number(next) }),
          DREP_COHORT_SIZE_OPTIONS.map((size): [string, string] => [
            String(size),
            intl.formatNumber(size),
          ])
        )}
      </div>
    </div>
  );
}

export default injectIntl(DRepCohortCriteriaPanel);
