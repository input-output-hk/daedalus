/**
 * Deterministic seeded shuffle for the default DRep cohort. mulberry32 is a
 * tiny 32-bit PRNG: the same (items, seed) pair always yields the same
 * permutation, so cohort order is reproducible for the whole app session.
 */
function mulberry32(seed: number): () => number {
  let state = seed >>> 0;
  return () => {
    state = (state + 0x6d2b79f5) >>> 0;
    let t = state;
    t = Math.imul(t ^ (t >>> 15), t | 1);
    t ^= t + Math.imul(t ^ (t >>> 7), t | 61);
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
}

export function generateCohortSeed(): number {
  return Math.floor(Math.random() * 4294967296) >>> 0;
}

export function seededShuffle<T>(items: T[], seed: number): T[] {
  const result = [...items];
  const random = mulberry32(seed);
  for (let i = result.length - 1; i > 0; i--) {
    const j = Math.floor(random() * (i + 1));
    [result[i], result[j]] = [result[j], result[i]];
  }
  return result;
}
