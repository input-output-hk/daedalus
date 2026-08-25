import { isSameDRep } from '../../../source/renderer/app/utils/governance/isSameDRep';
import {
  currentVoteOptions,
  resolveCurrentVote,
} from '../../../storybook/stories/governance/_utils/fixtures';

// The DRep id the delegation stories pre-fill into the form. Its bech32 body
// is all `q`, which is zero, so it decodes to twenty-eight zero bytes.
const PREFILLED_DREP_ID =
  'drep1ygqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq7vlc9n';

describe('delegation story fixtures', () => {
  // Same-vote comparison is keyed on (credentialHex, credentialType), not on
  // the identifier a reader sees. A fixture credential that happens to equal
  // the pre-filled one makes the form report that the wallet already votes for
  // the DRep it is being asked to change to, while displaying two visibly
  // different identifiers, which reads as the form being broken.
  it.each(Object.entries(currentVoteOptions))(
    'does not collide with the pre-filled DRep: %s',
    (_label, option) => {
      expect(isSameDRep(PREFILLED_DREP_ID, resolveCurrentVote(option))).toBe(
        false
      );
    }
  );

  it('gives every DRep fixture a credential of its own', () => {
    const credentials: string[] = [];
    for (const option of Object.values(currentVoteOptions)) {
      const vote = resolveCurrentVote(option);
      if (vote?.kind === 'drep') {
        credentials.push(
          `${vote.drep.credentialHex}:${vote.drep.credentialType}`
        );
      }
    }

    expect(new Set(credentials).size).toBe(credentials.length);
  });
});
