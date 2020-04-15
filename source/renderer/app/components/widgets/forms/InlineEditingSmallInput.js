// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
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

  inputField: Input;

  render() {
    const { validator } = this;
    const {
      className,
      inputFieldLabel,
      // isActive,
      inputBlocked,
      maxLength,
    } = this.props;
    const { isActive } = this.state;
    let { successfullyUpdated } = this.props;
    const inputField = validator.$('inputField');
    const componentStyles = classnames([
      className,
      styles.component,
      isActive ? styles.isActive : null,
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
        onMouseUp={this.onFocus}
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
          disabled={!isActive}
          ref={input => {
            this.inputField = input;
          }}
          skin={InputSkin}
        />
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
            onClick={() => this.inputField.focus()}
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
              onClick={() => {}}
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
              onClick={this.submit}
              skin={ButtonSkin}
            />
          </>
        )}
      </div>
    );
  }
}
