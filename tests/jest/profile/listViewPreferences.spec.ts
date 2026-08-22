import ProfileStore from '../../../source/renderer/app/stores/ProfileStore';
import { DEFAULT_LIST_VIEW_MODE } from '../../../source/renderer/app/types/listViewTypes';

const makeStore = (stored: Record<string, unknown> = {}) => {
  const setListViewPreferences = jest.fn().mockResolvedValue(undefined);
  const api = {
    localStorage: {
      getListViewPreferences: jest.fn().mockResolvedValue(stored),
      setListViewPreferences,
    },
  };
  const store = Object.create(ProfileStore.prototype) as ProfileStore;
  // The preference methods are self-contained: they read the api and the one
  // observable, so the store does not need its full wiring to exercise them.
  (store as any).api = api;
  (store as any).listViewPreferences = {};
  return { store, api, setListViewPreferences };
};

describe('list view preferences', () => {
  it('defaults to cards for a screen with no stored choice', () => {
    const { store } = makeStore();
    expect(store.getListViewMode('drepDirectory')).toBe(DEFAULT_LIST_VIEW_MODE);
    expect(store.getListViewMode('stakePools')).toBe('cards');
  });

  it('loads what was stored', async () => {
    const { store } = makeStore({ stakePools: 'table' });
    await (store as any)._loadListViewPreferences();
    expect(store.getListViewMode('stakePools')).toBe('table');
    expect(store.getListViewMode('drepDirectory')).toBe('cards');
  });

  it('persists a change without disturbing the other screen', async () => {
    const { store, setListViewPreferences } = makeStore();
    await (store as any)._loadListViewPreferences();

    await store.setListViewMode('drepDirectory', 'table');
    expect(store.getListViewMode('drepDirectory')).toBe('table');
    expect(store.getListViewMode('stakePools')).toBe('cards');

    await store.setListViewMode('stakePools', 'table');
    expect(setListViewPreferences).toHaveBeenLastCalledWith({
      drepDirectory: 'table',
      stakePools: 'table',
    });
  });

  it('survives a missing stored value', async () => {
    const { store, api } = makeStore();
    (api.localStorage.getListViewPreferences as jest.Mock).mockResolvedValue(
      undefined
    );
    await (store as any)._loadListViewPreferences();
    expect(store.getListViewMode('drepDirectory')).toBe('cards');
  });
});
