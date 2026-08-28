import { defineMessages } from 'react-intl';

/**
 * The names of the suggestion criteria, in one place.
 *
 * Two surfaces say what the cohort was drawn under: the summary above the
 * suggestions lists them, and the panel behind it turns them on and off. Naming
 * a criterion twice let the two disagree about what the rule was called, so
 * they read from here.
 *
 * Whole phrases rather than sentence fragments to be joined. A translator sees
 * each one entire, and no list has to be assembled out of clauses that only
 * work in English word order.
 */
export const drepCriteriaMessages = defineMessages({
  active: {
    id: 'governance.drepDirectory.cohort.criteria.active',
    defaultMessage: '!!!Active registration',
    description:
      'Suggestion criterion: the DRep registration is active. Always applied',
  },
  verifiedMetadata: {
    id: 'governance.drepDirectory.cohort.criteria.verifiedMetadata',
    defaultMessage: '!!!Verified metadata',
    description: 'Suggestion criterion: metadata was fetched and hash-verified',
  },
  votingPowerUnder: {
    id: 'governance.drepDirectory.cohort.criteria.votingPowerUnder',
    defaultMessage: '!!!Voting power under {share}',
    description:
      'Suggestion criterion: below the voting-power ceiling, which the user sets',
  },
});
