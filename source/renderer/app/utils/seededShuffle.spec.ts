import { generateCohortSeed, seededShuffle } from './seededShuffle';

describe('seededShuffle', () => {
  const items = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j'];

  it('returns the same permutation for the same seed and input', () => {
    expect(seededShuffle(items, 42)).toEqual(seededShuffle(items, 42));
  });

  it('returns a different permutation for a different seed', () => {
    // Deterministic PRNG: if these two seeds ever collide on this input,
    // change the second seed rather than weakening the assertion.
    expect(seededShuffle(items, 1)).not.toEqual(seededShuffle(items, 2));
  });

  it('preserves membership and does not mutate its input', () => {
    const input = [...items];
    const shuffled = seededShuffle(input, 7);

    expect(input).toEqual(items);
    expect([...shuffled].sort()).toEqual([...items].sort());
  });

  it('handles empty and single-item arrays', () => {
    expect(seededShuffle([], 7)).toEqual([]);
    expect(seededShuffle(['only'], 7)).toEqual(['only']);
  });

  it('generates seeds inside the unsigned 32-bit range', () => {
    for (let i = 0; i < 100; i++) {
      const seed = generateCohortSeed();
      expect(Number.isInteger(seed)).toBe(true);
      expect(seed).toBeGreaterThanOrEqual(0);
      expect(seed).toBeLessThan(4294967296);
    }
  });
});
