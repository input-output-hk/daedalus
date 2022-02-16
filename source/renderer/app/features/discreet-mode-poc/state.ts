import { createReducer, States, StatesTransition } from 'react-states';

type ToggleState = TypedStates<{
  ON: {};
  OFF: {};
}>;

export type State = TypedStates<{
  LOADING: {};
  LOADED: {
    isOpeningInDiscreetMode: boolean;
    discreetMode: ToggleState;
  };
}>;

export type Actions = TypedActions<{
  LOADED: {
    savedUserSetting: boolean;
  };
  TOGGLE_DISCREET_MODE: {};
  TOGGLE_OPEN_IN_DISCREET_MODE: {};
}>;

export type Commands = TypedCommands<{
  SAVE_OPEN_IN_DISCREET_MODE: {
    value: boolean;
  };
}>;

export type StateMachine = States<State, Actions, Commands>;
type Transition = StatesTransition<StateMachine>;

export const discreetModeReducer = createReducer<StateMachine>({
  LOADING: {
    LOADED: (state, event): Transition => ({
      state: 'LOADED',
      isOpeningInDiscreetMode: event.savedUserSetting,
      discreetMode: {
        state: event.savedUserSetting ? 'ON' : 'OFF',
      },
    }),
  },
  LOADED: {
    TOGGLE_DISCREET_MODE: (state): Transition => ({
      ...state,
      discreetMode: {
        state: state.discreetMode.state === 'ON' ? 'OFF' : 'ON',
      },
    }),
    TOGGLE_OPEN_IN_DISCREET_MODE: (state): Transition => [
      {
        ...state,
        isOpeningInDiscreetMode: !state.isOpeningInDiscreetMode,
      },
      {
        cmd: 'SAVE_OPEN_IN_DISCREET_MODE',
        value: !state.isOpeningInDiscreetMode,
      },
    ],
  },
});
