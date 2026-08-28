import BigNumber from 'bignumber.js';
import faker from '@faker-js/faker';
import {
  createSeededRandom,
  drawDRepCohort,
  selectDRepCohortPool,
  DEFAULT_DREP_COHORT_CRITERIA,
} from '../../../../source/renderer/app/components/governance/_shared/drepCohort';
import type { DRepCohortCriteria } from '../../../../source/renderer/app/components/governance/_shared/drepCohort';
import type { AppDRepDirectoryEntry } from '../../../../source/renderer/app/stores/GovernanceStore';

/**
 * One generator for every governance story that needs a DRep list.
 *
 * Stories used to name their own handful of entries, which meant each one
 * showed a different, and usually kinder, chain than the last: two DReps where
 * a real directory has a thousand, everyone active where mainnet is 37% active,
 * and every name a short ASCII one. A screen reviewed against that is reviewed
 * against a happy path nobody will ever see.
 *
 * The proportions below are measured, not guessed. Sampling 1,000 registered
 * mainnet DReps through Koios at epoch 650: 368 active, 404 with resolvable
 * metadata, 298 more than six epochs from lapsing, and given names running 1 to
 * 76 characters with 4% carrying non-Latin characters. Applying the default
 * cohort criteria cumulatively leaves 204 of the 1,000 eligible.
 *
 * Every population is seeded, so a story renders the same chain on every reload
 * and a screenshot means something.
 */

// Sampled from mainnet: CJK, katakana, Latin extended, the ada sign and
// regional-indicator flag pairs, alongside ordinary ASCII. Truncation, wrapping
// and font fallback are only visible against names like these.
export const MAINNET_DREP_NAMES = [
  // Measured on mainnet. The first is the longest name observed, 76 characters
  // carrying two regional-indicator flag pairs.
  'Porto Cripto DRep 🇧🇷 🇵🇹 (Atico & Bosco, from Cardanistas Stake Pool - CARDs)',
  'Sebastien Guillemot セバ',
  'Nimuë Lady of the Lake Pool',
  'Luc₳s @ 45B.io (Direct Voter)',
  'KOPI 咖啡 Singapore',
  'Mike Rogero (羅邁凱）',
  'Jose Martinez | Atlas Network Transmissions',
  'No Treasury Withdrawals until ADA > $3',
  'Cardano Academy',
  // Scripts a worldwide DRep set will reach even where this sample did not.
  // Rendering, line breaking and font fallback differ across all of them, and
  // a directory that has only been looked at in Latin has only been looked at
  // in the easiest case.
  'Δημοκρατία Κατανομή', // Greek
  'Кардано Делегат Совет', // Cyrillic
  '카르다노 거버넌스 대표', // Korean
  '卡尔达诺治理代表委员会', // Chinese
  'カルダノ・ガバナンス代表', // Japanese
  'Ísfjörður Stakepool ehf.', // Latin with diacritics
  'DRep ⚡️ Zero-Fee 🇰🇷 <Ops/> & "Quotes"', // emoji plus markup-shaped characters
  'Ādhāra Śāsana Pratinidhi', // Latin extended
  // Every one of these strings is written by the DRep. A name shaped like
  // markup belongs in the population so that a reviewer sees it rendered as
  // the text it is, rather than trusting that it would be.
  '<script>alert("DRep")</script>',
  'Bidi ‮ spoof DRep',
];

/**
 * Voting power fitted to the chain rather than spread evenly across its range.
 *
 * Drawing uniformly between zero and the observed maximum is the obvious move
 * and produces a population nothing like mainnet. The concentration threshold
 * sits at 1.5% of the total, roughly 74M ADA, and a uniform draw puts most of
 * the population above it where the chain puts 18 DReps in 1,000. Every card
 * would carry the warning, and a warning every card carries is not a warning.
 *
 * These parameters come from the same 1,000-DRep sample as everything else
 * here. 16% hold no voting power at all. The rest are log-normal in ADA with a
 * mean log of 10.387 and a standard deviation of 4.254, which puts the median
 * near 58,600 ADA, the maximum near 565.8M, and roughly 2 to 3% above the
 * concentration threshold.
 */
/** The denominator shares are measured against: mainnet's order of magnitude. */
export const TOTAL_DREP_STAKE = new BigNumber('4937800000000000');

const ACTIVE_SHARE = 368 / 1000;
const VERIFIED_SHARE = 404 / 1000;
const LAPSING_SOON_SHARE = 1 - 298 / 368;

const ZERO_POWER_SHARE = 165 / 1000;
const LOG_POWER_MEAN = 10.387;
const LOG_POWER_DEVIATION = 4.254;
const MAX_VOTING_POWER_ADA = 565_846_927;

/** Box-Muller, so the normal draw comes from the same seeded source. */
function standardNormal(random: () => number): number {
  const u = Math.max(random(), Number.MIN_VALUE);
  const v = random();
  return Math.sqrt(-2 * Math.log(u)) * Math.cos(2 * Math.PI * v);
}

function drawVotingPower(random: () => number): BigNumber {
  if (random() < ZERO_POWER_SHARE) return new BigNumber(0);
  const ada = Math.exp(
    LOG_POWER_MEAN + standardNormal(random) * LOG_POWER_DEVIATION
  );
  const clamped = Math.min(Math.max(Math.round(ada), 1), MAX_VOTING_POWER_ADA);
  return new BigNumber(clamped).multipliedBy(1_000_000);
}

export interface DRepPopulationOptions {
  /** Same seed, same population. Vary it to get a different chain, not a longer one. */
  seed?: number;
  /** Proportions default to the mainnet measurements; override to force a shape. */
  activeShare?: number;
  verifiedShare?: number;
  lapsingSoonShare?: number;
}

/**
 * Distinct names, so a reader can tell one DRep from another.
 *
 * Reusing a short pool made a second page of results indistinguishable from the
 * first: the only thing that changed was an opaque identifier, which is not
 * something anyone reads. Faker supplies the bulk, seeded so a story renders
 * the same directory every time, and the measured mainnet names are dealt in
 * first so that truncation, wrapping and font fallback are exercised on every
 * population rather than only on the one story that remembered to ask for them.
 */
function createNameSource(seed: number) {
  faker.seed(seed);
  const used = new Set<string>();
  let dealt = 0;

  const distinct = (candidate: string): string => {
    if (!used.has(candidate)) {
      used.add(candidate);
      return candidate;
    }
    let suffix = 2;
    while (used.has(`${candidate} ${suffix}`)) suffix += 1;
    const unique = `${candidate} ${suffix}`;
    used.add(unique);
    return unique;
  };

  return {
    next(): string {
      if (dealt < MAINNET_DREP_NAMES.length) {
        const name = MAINNET_DREP_NAMES[dealt];
        dealt += 1;
        return distinct(name);
      }
      dealt += 1;
      // A mix of the two shapes real DRep names take: people, and pools or
      // organisations standing behind a person.
      return distinct(
        dealt % 3 === 0
          ? `${faker.company.companyName()} DRep`
          : faker.name.findName()
      );
    },
  };
}

function drepIdFor(index: number): string {
  return `drep1yg7s8vuv_8ff8a9y6z0m8p4kw7q9s8n3d7m9p2l0v8k6m6m2k${String(
    index
  ).padStart(4, '0')}`;
}

function anchorFor(index: number) {
  return {
    hash: `6a5e200d2f3a10202020202020202020202020202020202020202020${String(
      index
    ).padStart(8, '0')}`,
    url: `https://governance.example.org/dreps/${index}.json`,
  };
}

/**
 * A DRep population shaped like mainnet's.
 *
 * Metadata is one predicate, not two: an entry carries an anchor exactly when
 * it carries a verified name, because that is the only thing the app can
 * determine about a DRep's metadata and fixtures must not imply otherwise.
 */
export function makeDRepPopulation(
  size: number,
  options: DRepPopulationOptions = {}
): AppDRepDirectoryEntry[] {
  const {
    seed = 1,
    activeShare = ACTIVE_SHARE,
    verifiedShare = VERIFIED_SHARE,
    lapsingSoonShare = LAPSING_SOON_SHARE,
  } = options;
  const random = createSeededRandom(seed);
  const names = createNameSource(seed);

  const population = Array.from({ length: size }, (_, i) => {
    const index = i + 1;
    const isActive = random() < activeShare;
    const isVerified = random() < verifiedShare;
    const isInactiveSoon = isActive && random() < lapsingSoonShare;

    return {
      drepId: drepIdFor(index),
      status: isActive ? ('active' as const) : ('inactive' as const),
      // Six or fewer epochs is the lapsing window the expiry badge reads.
      drepActivity: isInactiveSoon
        ? 1 + Math.floor(random() * 6)
        : 7 + Math.floor(random() * 14),
      anchor: isVerified ? anchorFor(index) : null,
      verifiedName: isVerified ? names.next() : null,
      doNotList: false,
      votingPower: drawVotingPower(random),
    };
  });

  return population;
}

/**
 * The cohort a population actually yields, drawn by the shipping selection
 * rather than hand-picked. A story that names its own twenty entries is showing
 * a list the app would never produce.
 */
export function drawCohortFrom(
  population: AppDRepDirectoryEntry[],
  criteria: DRepCohortCriteria = DEFAULT_DREP_COHORT_CRITERIA,
  seed = 1,
  totalDRepStake: BigNumber | null = TOTAL_DREP_STAKE
): AppDRepDirectoryEntry[] {
  return drawDRepCohort(
    selectDRepCohortPool(population, criteria, totalDRepStake),
    seed
  );
}

/**
 * The largest DRep detail page mainnet can currently produce.
 *
 * Measured across the 404 sampled DReps whose metadata resolved. Rendered text
 * runs to a median of 1,073 characters, a 95th percentile of 2,888, and a
 * maximum of 3,719. The dimensions below are each field's own observed maximum
 * put on one page, so the story is a worst case rather than a plausible case:
 * motivations 3,374 characters, objectives 1,071, qualifications 1,000, a
 * 76-character name, eight references, an 80-character reference label and a
 * 125-character URI.
 *
 * Byte size is a separate matter and not represented here. The largest document
 * in the sample is 274,310 bytes, of which 268,625 is a base64 JPEG in the
 * CIP-119 `image` field. Prose is not what makes a metadata document large.
 */
const LOREM =
  'Governance participation requires sustained attention to treasury discipline, protocol parameter changes and the constitutional process, and a DRep who intends to vote responsibly has to explain how they will weigh those against one another. ';

const fillTo = (length: number): string => {
  let text = '';
  while (text.length < length) text += LOREM;
  return text.slice(0, length).trim();
};

export const LARGEST_KNOWN_DREP_METADATA = {
  // Mainnet's longest given name, emoji flag pairs included.
  verifiedName: MAINNET_DREP_NAMES[0],
  objectives: fillTo(1071),
  motivations: fillTo(3374),
  qualifications: fillTo(1000),
  paymentAddress:
    'addr1qyr53s0h929lksqp5v8rhlveu4skwp8ugdz87ghaswu95v6q9mncexq3sz7phzf5x4yuez5ljkhfauj6puptdtp86ekq8ndej2',
  references: [
    {
      type: 'link',
      label: 'DRep Voting Framework for a Sustainable Cardano Ecosystem',
      uri: 'https://governance.example.org/notes/drep-voting-framework-for-a-sustainable-ecosystem/index.html',
    },
    { type: 'identity', label: 'X (Twitter)', uri: 'https://x.com/example' },
    { type: 'link', label: 'Website', uri: 'https://example.org/' },
    { type: 'other', label: 'A', uri: 'https://example.org/a' },
    { type: 'link', label: 'Voting record', uri: 'https://example.org/votes' },
    { type: 'link', label: 'Blog', uri: 'https://example.org/blog' },
    { type: 'identity', label: 'Keybase', uri: 'https://keybase.io/example' },
    {
      type: 'other',
      label: 'Constitutional Committee submissions archive 2024',
      uri: 'https://example.org/cc',
    },
  ],
};
