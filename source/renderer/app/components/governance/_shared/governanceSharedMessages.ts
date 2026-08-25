import { defineMessages } from 'react-intl';

/**
 * Strings more than one governance screen says.
 *
 * Each component declaring its own messages is the convention here, and it
 * works while a string belongs to one screen. These do not: the directory, the
 * detail view and the delegation form all report a DRep's voting power, all
 * wait for the same data, and all describe Abstain and No Confidence in the
 * same words. Declared separately they were translated separately and could
 * drift apart on the next edit, so that two screens would describe one thing
 * two ways with nothing to catch it.
 */
export const governanceSharedMessages = defineMessages({
  loadingDRepData: {
    id: 'governance.shared.loadingDRepData',
    defaultMessage: '!!!Loading DRep data…',
    description: 'Shown wherever DRep data is still being fetched',
  },
  votingPower: {
    id: 'governance.shared.votingPower',
    defaultMessage: '!!!Voting power',
    description:
      'Label of the stake delegated to a DRep, on cards, in the table and on the detail view',
  },
  votingPowerUnavailable: {
    id: 'governance.shared.votingPowerUnavailable',
    defaultMessage: '!!!Stake distribution unavailable, try again later.',
    description: 'Shown in place of a voting power that could not be read',
  },
  inactiveSoon: {
    id: 'governance.shared.inactiveSoon',
    defaultMessage: '!!!Inactive Soon',
    description:
      'A DRep within the threshold of going inactive: the badge, the filter and the suggestion criterion all name it this',
  },
  allDReps: {
    id: 'governance.shared.allDReps',
    defaultMessage: '!!!All DReps',
    description: 'Names the unfiltered population, as a mode and as a heading',
  },
  showAllDReps: {
    id: 'governance.shared.showAllDReps',
    defaultMessage: '!!!Show all DReps',
    description: 'Action that leaves the suggestions for the full population',
  },
  abstainDescription: {
    id: 'governance.shared.abstainDescription',
    defaultMessage:
      '!!!Your stake is recorded on chain as not participating in governance.',
    description: 'What delegating to Abstain does to a wallet stake',
  },
  noConfidenceDescription: {
    id: 'governance.shared.noConfidenceDescription',
    defaultMessage:
      '!!!Your stake counts as Yes on every motion of no confidence, and as No on every other governance action.',
    description: 'What delegating to No Confidence does to a wallet stake',
  },
});
