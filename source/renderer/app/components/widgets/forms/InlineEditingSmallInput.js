// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { get } from 'lodash';
import SVGInline from 'react-svg-inline';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import { Input } from 'react-polymorph/lib/components/Input';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import { Button } from 'react-polymorph/lib/components/Button';
import classnames from 'classnames';
import ReactToolboxMobxForm from '../../../utils/ReactToolboxMobxForm';
import styles from './InlineEditingSmallInput.scss';
import { FORM_VALIDATION_DEBOUNCE_WAIT } from '../../../config/timingConfig';
import penIcon from '../../../assets/images/pen.inline.svg';
import crossIcon from '../../../assets/images/close-cross.inline.svg';
import arrowIcon from '../../../assets/images/arrow-right.inline.svg';

type Props = {
  className?: string,
  isActive: boolean,
  isDisabled: boolean,
  placeholder?: string,
  inputFieldLabel?: string,
  inputFieldValue: string,
  onStartEditing?: Function,
  onStopEditing?: Function,
  onCancelEditing?: Function,
  onSubmit: Function,
  isValid: Function,
  validationErrorMessage: string,
  successfullyUpdated: boolean,
  inputBlocked?: boolean,
  maxLength?: number,
};

type State = {
  isActive: boolean,
};

@observer
export default class InlineEditingSmallInput extends Component<Props, State> {
  state = {
    isActive: false,
  };

  static defaultProps = {
    onStartEditing: () => {},
    onStopEditing: () => {},
    onCancelEditing: () => {},
  };

  validator = new ReactToolboxMobxForm(
    {
      fields: {
        inputField: {
          value: this.props.inputFieldValue,
          validators: [
            ({ field }) => [
              this.props.isValid(field.value),
              this.props.validationErrorMessage,
            ],
          ],
        },
      },
    },
    {
      options: {
        validateOnChange: true,
        validationDebounceWait: FORM_VALIDATION_DEBOUNCE_WAIT,
      },
    }
  );

  submit = () => {
    this.validator.submit({
      onSuccess: form => {
        const { inputField } = form.values();
        this.setState({ isActive: false });
        if (inputField !== this.props.inputFieldValue) {
          this.props.onStopEditing();
          this.props.onSubmit(inputField);
        } else {
          this.props.onCancelEditing();
        }
      },
    });
  };

  handleInputKeyDown = (event: KeyboardEvent) => {
    if (event.which === 13) {
      // ENTER key
      this.onBlur();
    }
    if (event.which === 27) {
      // ESCAPE key
      this.onCancel();
    }
  };

  onFocus = () => {
    this.setState({ isActive: true });
    if (this.props.onStartEditing) this.props.onStartEditing();
  };

  onBlur = () => {
    if (this.state.isActive) {
      this.submit();
    }
  };

  onCancel = () => {
    const inputField = this.validator.$('inputField');
    inputField.value = this.props.inputFieldValue;
    this.setState({ isActive: false });
    if (this.props.onCancelEditing) this.props.onCancelEditing();
  };

  componentDidUpdate() {
    if (this.props.isActive) {
      const { inputBlocked } = this.props;
      // eslint-disable-next-line no-unused-expressions
      this.inputField && !inputBlocked && this.inputField.focus();
    }
  }

  get input() {
    const fallbackInput = {
      blur: () => {},
      focus: () => {},
    };
    return get(this, 'inputField.inputElement.current', fallbackInput);
  }

  inputField: Input;

  render() {
    const { validator } = this;
    const {
      className,
      inputFieldLabel,
      isDisabled,
      inputBlocked,
      maxLength,
      placeholder,
    } = this.props;
    const { isActive } = this.state;
    let { successfullyUpdated } = this.props;
    const inputField = validator.$('inputField');
    const componentStyles = classnames([
      className,
      styles.component,
      isActive ? styles.isActive : null,
      isDisabled ? styles.disabled : null,
      inputField.error ? styles.hasError : null,
    ]);
    const inputStyles = classnames([
      successfullyUpdated ? 'input_animateSuccess' : null,
      isActive ? null : 'input_cursorPointer',
    ]);

    if (isActive || inputBlocked) {
      successfullyUpdated = false;
    }

    return (
      <div
        className={componentStyles}
        onBlur={this.onBlur}
        onMouseUp={() => {
          this.input.focus();
          this.onFocus();
        }}
        role="presentation"
        aria-hidden
      >
        <Input
          className={inputStyles}
          themeOverrides={styles}
          type="text"
          maxLength={maxLength}
          label={inputFieldLabel}
          value={inputField.value}
          onChange={inputField.onChange}
          onFocus={inputField.onFocus}
          onBlur={inputField.onBlur}
          onKeyDown={event => this.handleInputKeyDown(event)}
          error={isActive || inputBlocked ? inputField.error : null}
          disabled={isDisabled}
          placeholder={placeholder || ''}
          ref={input => {
            this.inputField = input;
          }}
          skin={InputSkin}
        />
        {!isDisabled && (
          <>
            {!isActive ? (
              <Button
                className={styles.rightButton}
                label={
                  <SVGInline
                    svg={penIcon}
                    className={styles.penIcon}
                    style={{ pointerEvents: 'none' }}
                  />
                }
                onMouseUp={() => this.input.focus()}
                onMouseDown={(event: SyntheticMouseEvent<HTMLElement>) => {
                  event.persist();
                  event.preventDefault();
                  event.stopPropagation();
                }}
                skin={ButtonSkin}
              />
            ) : (
              <>
                <Button
                  className={styles.leftButton}
                  label={
                    <SVGInline
                      svg={crossIcon}
                      className={styles.crossIcon}
                      style={{ pointerEvents: 'none' }}
                    />
                  }
                  onMouseUp={() => {
                    this.onCancel();
                    this.input.blur();
                  }}
                  onMouseDown={(event: SyntheticMouseEvent<HTMLElement>) => {
                    event.persist();
                    event.preventDefault();
                    event.stopPropagation();
                  }}
                  skin={ButtonSkin}
                />
                <Button
                  className={styles.rightButton}
                  label={
                    <SVGInline
                      svg={arrowIcon}
                      className={styles.arrowIcon}
                      style={{ pointerEvents: 'none' }}
                    />
                  }
                  onMouseUp={() => this.input.blur()}
                  onMouseDown={(event: SyntheticMouseEvent<HTMLElement>) => {
                    event.persist();
                    event.preventDefault();
                    event.stopPropagation();
                  }}
                  skin={ButtonSkin}
                />
              </>
            )}
          </>
        )}
      </div>
    );
  }
}
