/**
 * Whether a list screen shows cards or a table.
 *
 * The DRep directory and the stake pools screen offer the same choice, so they
 * share one stored preference rather than each keeping its own and drifting
 * apart. Screens are keyed by name so a new one can opt in without a migration.
 */
export type ListViewMode = 'cards' | 'table';

export type ListViewScreen = 'drepDirectory' | 'stakePools';

export type ListViewPreferences = Partial<Record<ListViewScreen, ListViewMode>>;

export const DEFAULT_LIST_VIEW_MODE: ListViewMode = 'cards';
