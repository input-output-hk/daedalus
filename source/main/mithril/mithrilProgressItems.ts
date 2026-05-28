import type { MithrilProgressItem } from '../../common/types/mithril-bootstrap.types';

export const markActiveProgressItemAs = (
  items: MithrilProgressItem[],
  state: 'completed' | 'error'
): MithrilProgressItem[] =>
  items.map((item) => (item.state === 'active' ? { ...item, state } : item));

export const addProgressItem = (
  items: MithrilProgressItem[],
  id: string,
  label: string,
  state: MithrilProgressItem['state']
): MithrilProgressItem[] => {
  if (items.some((item) => item.id === id)) return items;
  return [...items, { id, label, state, timestamp: new Date().toISOString() }];
};

export const upsertProgressItem = (
  items: MithrilProgressItem[],
  id: string,
  label: string,
  state: MithrilProgressItem['state']
): MithrilProgressItem[] => {
  if (!items.some((item) => item.id === id)) {
    return addProgressItem(items, id, label, state);
  }

  return items.map((item) => (item.id === id ? { ...item, label, state } : item));
};
