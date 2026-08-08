const CHUNKS_PER_EPOCH = 2160;

// 2-arg form: raw immutable chunk counts from watchdog probe events
export function computeBehindByEpochs(
  localImmutableCount: number,
  latestCertifiedImmutable: number
): number;
// 3-arg form: tip objects + snapshot epoch (storybook usage / legacy)
export function computeBehindByEpochs(
  localTip: { epoch: number } | null,
  networkTip: { epoch: number } | null,
  snapshotEpoch: number
): number;
export function computeBehindByEpochs(
  a: number | { epoch: number } | null,
  b: number | { epoch: number } | null,
  c?: number
): number {
  if (c !== undefined) {
    const localEpoch = a != null && typeof a === 'object' ? a.epoch : 0;
    return Math.max(0, c - localEpoch);
  }
  const local = typeof a === 'number' ? a : 0;
  const latest = typeof b === 'number' ? b : 0;
  return Math.floor(Math.max(0, latest - local) / CHUNKS_PER_EPOCH);
}
