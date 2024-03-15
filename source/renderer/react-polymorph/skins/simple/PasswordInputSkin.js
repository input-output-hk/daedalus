// @flow
import classnames from 'classnames';
import React from 'react';

import { Input } from '../../components/Input';
import { PasswordInput } from '../../components/PasswordInput';
import type { PasswordInputProps } from '../../components/PasswordInput';
import { SimplePasswordInputVariables } from '../../themes/simple/SimplePasswordInput';
import { useDebouncedValueChangedIndicator } from '../../utils/hooks';

type Props = PasswordInputProps & {
  score: number,
  theme: Object,
};

function getPopOverBgColorForState(state: PasswordInput.STATE): string {
  switch (state) {
    case PasswordInput.STATE.ERROR:
    case PasswordInput.STATE.INSECURE:
      return `var(${SimplePasswordInputVariables.errorColor})`;
    case PasswordInput.STATE.WEAK:
      return `var(${SimplePasswordInputVariables.warningColor})`;
    case PasswordInput.STATE.STRONG:
      return `var(${SimplePasswordInputVariables.successColor})`;
    case PasswordInput.STATE.DEFAULT:
    default:
      return 'var(--rp-pop-over-bg-color)';
  }
}

export function PasswordInputSkin(props: Props) {
  const {
    className,
    error,
    debounceDelay,
    isShowingTooltipOnFocus,
    isShowingTooltipOnHover,
    isTooltipOpen,
    score,
    state,
    theme,
    themeId,
    tooltip,
    useDebounce,
    value,
    ...inputProps
  } = props;
  const hasInitialValueChanged = useDebounce
    ? useDebouncedValueChangedIndicator(value, debounceDelay)
    : true;
  const hasTooltip = hasInitialValueChanged && tooltip != null;
  const stateColor = getPopOverBgColorForState(state);
  const isErrorState = state === PasswordInput.STATE.ERROR;
  return (
    <div
      className={classnames([
        theme[themeId].root,
        theme[themeId][state],
        className,
      ])}
    >
      <Input
        {...inputProps}
        error={hasTooltip ? tooltip : null}
        showErrorState={hasTooltip && isErrorState}
        hideErrorState={!isErrorState}
        value={value}
        type="password"
        themeVariables={{
          '--rp-pop-over-bg-color': stateColor,
        }}
      />
      <div className={theme[themeId].indicator}>
        <div
          className={theme[themeId].score}
          style={{ width: `${(score || 0) * 100}%` }}
        />
      </div>
    </div>
  );
}

// Static Properties

PasswordInputSkin.displayName = 'PasswordInputSkin';
