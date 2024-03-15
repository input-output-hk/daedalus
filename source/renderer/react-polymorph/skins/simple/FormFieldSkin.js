// @flow
import type { ElementRef } from 'react';
import React, { useState } from 'react';
import classnames from 'classnames';
import type { FormFieldProps } from '../../components/FormField';
import { PopOver } from '../../components/PopOver';
import { SimpleFormFieldVariables } from '../../themes/simple/SimpleFormField';
import { handleRefState, manageRef } from '../../utils/hooks';

type Props = FormFieldProps & {
  formFieldRef: ElementRef<*>,
  focusChild: Function,
  setError: Function,
};

export function FormFieldSkin(props: Props) {
  const updateOnRefChanges = manageRef(props.formFieldRef);
  const isFormFieldFocused = handleRefState(props.formFieldRef, {
    on: 'focus',
    off: 'blur',
  });
  const isFormFieldHovered = handleRefState(props.formFieldRef, {
    on: 'mouseenter',
    off: 'mouseleave',
  });
  const hasError = props.error != null;
  return (
    <div
      className={classnames([
        props.className,
        props.theme[props.themeId].root,
        props.disabled ? props.theme[props.themeId].disabled : null,
        props.error ? props.theme[props.themeId].errored : null,
      ])}
      style={props.themeVariables}
    >
      {props.label && (
        <label
          role="presentation"
          aria-hidden
          className={props.theme[props.themeId].label}
          onClick={props.focusChild}
          htmlFor={props.id}
        >
          {props.label}
        </label>
      )}
      <PopOver
        visible={
          props.isErrorShown === true ||
          (props.isErrorHidden !== true &&
            hasError &&
            ((props.isShowingErrorOnFocus && isFormFieldFocused) ||
              (props.isShowingErrorOnHover && isFormFieldHovered)))
        }
        content={props.error}
        themeVariables={{
          '--rp-pop-over-bg-color': `var(${SimpleFormFieldVariables.errorColor}`,
          ...props.themeVariables,
        }}
        placement="bottom"
        popperOptions={{ modifiers: [{ name: 'flip', enabled: false }] }}
        duration={[300, 0]}
      >
        <div className={props.theme[props.themeId].inputWrapper}>
          {props.render(updateOnRefChanges)}
        </div>
      </PopOver>
    </div>
  );
}
