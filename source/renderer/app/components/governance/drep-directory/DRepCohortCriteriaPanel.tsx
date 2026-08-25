import React from 'react';
import { defineMessages, injectIntl, intlShape } from 'react-intl';
import NormalSwitch from '../../widgets/forms/NormalSwitch';
import DRepFacetSelect from '../_shared/DRepFacetSelect';
import {
  DREP_COHORT_SIZE_OPTIONS,
  DREP_COHORT_VOTING_POWER_SHARE_OPTIONS,
} from '../_shared/drepCohort';
import type { DRepCohortCriteria } from '../_shared/drepCohort';
import { drepCriteriaMessages } from '../_shared/drepCriteriaMessages';
import styles from './DRepCohortCriteriaPanel.scss';

const messages = defineMessages({
  votingPowerNoLimit: {
    id: 'governance.drepDirectory.cohort.votingPowerNoLimit',
    defaultMessage: '!!!No limit',
    description: 'Option removing the suggestion voting-power ceiling',
  },
  votingPowerLabel: {
    id: 'governance.drepDirectory.cohort.votingPowerLabel',
    defaultMessage: '!!!Voting power under',
    description: 'Label of the suggestion voting-power ceiling control',
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
      <div className={styles.toggles}>
        {toggle(
          intl.formatMessage(drepCriteriaMessages.notInactiveSoon),
          criteria.excludeInactiveSoon,
          (excludeInactiveSoon) =>
            onCriteriaChange({ ...criteria, excludeInactiveSoon })
        )}
        {toggle(
          intl.formatMessage(drepCriteriaMessages.verifiedMetadata),
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
