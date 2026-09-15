import React, { useEffect, useState } from 'react';

/*
 * Local replacement for withState from @dump247/storybook-state.
 *
 * That package imports @storybook/addons and calls addons.getChannel() inside
 * withState itself, so every call site touched the addon channel at module
 * evaluation. @storybook/addons has no version in the Storybook 8 line, so the
 * corpus cannot carry it across the version hop.
 *
 * This touches no Storybook API at all, which is what makes it behave the same
 * on either side of that hop. It reproduces the package's observable behaviour
 * rather than approximating it: the state object is frozen, set merges into it,
 * reset notifies only when something actually changed, and one store is created
 * per withState() call and lives as long as the module, so a story that is
 * navigated away from and back keeps its state.
 *
 * It is a shim. These call sites become args-backed later, and it goes then.
 */

export type Store<T> = {
  /** The current state. Frozen: assign through set rather than mutating it. */
  state: T;
  /** Merge the given keys into the current state. */
  set(nextState: Partial<T>): void;
  /** Restore the state the store was created with. */
  reset(): void;
};

type Subscriber = () => void;

type MutableStore<T> = Store<T> & {
  subscribe(handler: Subscriber): () => void;
};

function createStore<T extends Record<string, any>>(
  initialState: T
): MutableStore<T> {
  const initial = Object.freeze({ ...initialState }) as T;
  let current: T = initial;
  const subscribers = new Set<Subscriber>();
  const notify = () => subscribers.forEach((handler) => handler());

  return {
    get state() {
      return current;
    },
    set(nextState: Partial<T>) {
      current = Object.freeze({ ...current, ...nextState }) as T;
      notify();
    },
    reset() {
      if (current !== initial) {
        current = initial;
        notify();
      }
    },
    subscribe(handler: Subscriber) {
      subscribers.add(handler);
      return () => {
        subscribers.delete(handler);
      };
    },
  };
}

type StoryRenderer<T> = (
  store: Store<T>
) => React.ReactElement | React.ReactElement[];

type StatefulStoryProps<T> = {
  store: MutableStore<T>;
  storyFn: StoryRenderer<T>;
};

/*
 * The hooks have to live here rather than in what withState returns. Storybook
 * calls the story function directly, so a story function that used hooks itself
 * would be calling them outside a render.
 */
function StatefulStory<T extends Record<string, any>>({
  store,
  storyFn,
}: StatefulStoryProps<T>) {
  const [, setRenderCount] = useState(0);

  useEffect(
    () => store.subscribe(() => setRenderCount((count) => count + 1)),
    [store]
  );

  return <>{storyFn(store)}</>;
}

export function withState<T extends Record<string, any>>(
  initialState: T,
  storyFn: StoryRenderer<T>
) {
  const store = createStore(initialState);
  return function WithLocalState() {
    return <StatefulStory store={store} storyFn={storyFn} />;
  };
}

export default withState;
