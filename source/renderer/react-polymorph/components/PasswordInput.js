// @flow
import stringEntropy from 'fast-password-entropy';
import React, { StatelessFunctionalComponent, useContext } from 'react';
import { ThemeContext } from './HOC/ThemeContext';

// internal utility functions
import { composeTheme, addThemeId } from '../utils/themes';

// import constants
import { IDENTIFIERS } from '.';
import type { InputProps } from './Input';

export const calculatePasswordScore = (
  password: string,
  entropyFactor: number = 0.01
): number => Math.min((stringEntropy(password) * entropyFactor).toFixed(2), 1);

const STATE = {
  DEFAULT: 'default',
  ERROR: 'error',
  INSECURE: 'insecure',
  WEAK: 'weak',
  STRONG: 'strong',
};

export type PasswordInputProps = InputProps & {
  debounceDelay?: number,
  entropyFactor?: number,
  isPasswordRepeat?: boolean,
  isShowingTooltipOnFocus: boolean,
  isShowingTooltipOnHover: boolean,
  isTooltipOpen: boolean,
  minLength?: number,
  minStrongScore?: number,
  repeatPassword?: string,
  state?: $Values<typeof STATE>,
  passwordFeedbacks?: {
    insecure: string,
    weak: string,
    strong: string,
    noMatch: string,
  },
  tooltip?: string | boolean,
  useDebounce?: boolean,
};

export const PasswordInput: StatelessFunctionalComponent<PasswordInputProps> = (
  props
) => {
  const {
    context,
    passwordFeedbacks,
    entropyFactor,
    error,
    minLength,
    maxLength,
    minStrongScore,
    isPasswordRepeat,
    repeatPassword,
    skin,
    state,
    theme,
    themeOverrides,
    tooltip,
    ...rest
  } = props;

  // Theme
  const themeContext = context || useContext(ThemeContext);
  const composedTheme = composeTheme(
    addThemeId(theme || themeContext.theme, props.themeId),
    addThemeId(props.themeOverrides, props.themeId),
    themeContext.ROOT_THEME_API
  );
  // Skin
  const PasswordInputSkin =
    skin || themeContext.skins[IDENTIFIERS.PASSWORD_INPUT];

  // Logic
  let dynamicState = PasswordInput.STATE.DEFAULT;
  let passwordFeedback = null;
  const password = props.value;
  const score = calculatePasswordScore(password, entropyFactor);
  const isValidPassword =
    password.length >= minLength && password.length <= maxLength;
  const isNotEmpty = password.length > 0;

  if (error) {
    dynamicState = PasswordInput.STATE.ERROR;
    passwordFeedback = error;
  } else if (tooltip) {
    passwordFeedback = tooltip;
  } else if (isPasswordRepeat) {
    if (repeatPassword === props.value) {
      dynamicState = PasswordInput.STATE.DEFAULT;
      passwordFeedback = null;
    } else {
      dynamicState = PasswordInput.STATE.ERROR;
      passwordFeedback = passwordFeedbacks.noMatch;
    }
  } else if (isValidPassword) {
    if (score < minStrongScore) {
      dynamicState = PasswordInput.STATE.WEAK;
      passwordFeedback = passwordFeedbacks[PasswordInput.STATE.WEAK];
    } else {
      dynamicState = PasswordInput.STATE.STRONG;
      passwordFeedback = passwordFeedbacks[PasswordInput.STATE.STRONG];
    }
  } else if (isNotEmpty) {
    dynamicState = PasswordInput.STATE.INSECURE;
    passwordFeedback = passwordFeedbacks[PasswordInput.STATE.INSECURE];
  }
  return (
    <PasswordInputSkin
      error={error}
      theme={composedTheme}
      score={score}
      state={state || dynamicState}
      tooltip={passwordFeedback}
      {...rest}
    />
  );
};

// Static Properties

PasswordInput.STATE = STATE;

PasswordInput.displayName = 'PasswordInput';

PasswordInput.defaultProps = {
  debounceDelay: 1000,
  entropyFactor: 0.01,
  passwordFeedbacks: {
    insecure: 'insecure',
    weak: 'weak',
    strong: 'strong',
    noMatch: "doesn't match",
  },
  isPasswordRepeat: false,
  isTooltipOpen: false,
  isShowingTooltipOnFocus: true,
  isShowingTooltipOnHover: true,
  minLength: 10,
  maxLength: 255,
  minStrongScore: 0.75,
  readOnly: false,
  theme: null,
  themeId: IDENTIFIERS.PASSWORD_INPUT,
  themeOverrides: {},
  useDebounce: true,
  value: '',
};
