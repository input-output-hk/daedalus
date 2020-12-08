// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import vjf from 'mobx-react-form/lib/validators/VJF';
import SVGInline from 'react-svg-inline';
import classnames from 'classnames';
import { Input } from 'react-polymorph/lib/components/Input';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import ReactToolboxMobxForm from '../../../utils/ReactToolboxMobxForm';
import styles from './InlineEditingInput.scss';
import { FORM_VALIDATION_DEBOUNCE_WAIT } from '../../../config/timingConfig';
import penIcon from '../../../assets/images/pen.inline.svg';
import crossIcon from '../../../assets/images/close-cross.inline.svg';
import arrowIcon from '../../../assets/images/arrow-right.inline.svg';
import spinningIcon from '../../../assets/images/spinner-ic.inline.svg';

const messages = defineMessages({
  change: {
    id: 'inline.editing.input.change.label',
    defaultMessage: '!!!change',
    description: 'Label "change" on inline editing inputs in inactive state.',
  },
  cancel: {
    id: 'inline.editing.input.cancel.label',
    defaultMessage: '!!!cancel',
    description: 'Label "cancel" on inline editing inputs in inactive state.',
  },
  changesSaved: {
    id: 'inline.editing.input.changesSaved',
    defaultMessage: '!!!Your changes have been saved',
    description:
      'Message "Your changes have been saved" for inline editing (eg. on Profile Settings page).',
  },
});

type Props = {
  className?: string,
  isActive: boolean,
  inputFieldLabel: string,
  inputFieldValue: string,
  inputFieldPlaceholder?: string,
  onStartEditing?: Function,
  onStopEditing?: Function,
  onCancelEditing: Function,
  onBlur?: Function,
  onSubmit: Function,
  isValid: Function,
  valueErrorMessage?: string,
  errorMessage?: ?string,
  successfullyUpdated: boolean,
  inputBlocked?: boolean,
  disabled?: boolean,
  readOnly?: boolean,
  maxLength?: number,
  isSubmitting?: boolean,
  validateOnChange?: boolean,
};

@observer
export default class InlineEditingInput extends Component<Props> {
  static defaultProps = {
    validateOnChange: true,
    valueErrorMessage: '',
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  validator = new ReactToolboxMobxForm(
    {
      fields: {
        inputField: {
          value: this.props.inputFieldValue,
          validators: [
            ({ field }) => [
              this.props.isValid(field.value),
              this.props.valueErrorMessage,
            ],
          ],
        },
      },
    },
    {
      plugins: { vjf: vjf() },
      options: {
        validateOnChange: this.props.validateOnChange,
        validationDebounceWait: FORM_VALIDATION_DEBOUNCE_WAIT,
      },
    }
  );

  submit = () => {
    this.validator.submit({
      onSuccess: async (form) => {
        const { inputField } = form.values();
        const { onSubmit, onStopEditing, onCancelEditing } = this.props;
        if (inputField !== this.props.inputFieldValue) {
          onSubmit(inputField);
          if (onStopEditing) onStopEditing();
        } else {
          onCancelEditing();
        }
      },
    });
  };

  handleInputKeyDown = (event: KeyboardEvent) => {
    if (event.which === 13) {
      // ENTER key
      this.submit();
    }
    if (event.which === 27) {
      // ESCAPE key
      this.onCancel();
    }
  };

  onFocus = () => {
    const { onStartEditing } = this.props;
    if (this.props.readOnly) return;
    if (onStartEditing) onStartEditing();
  };

  onBlur = () => {
    const { onBlur } = this.props;
    const inputField = this.validator.$('inputField');
    inputField.value = this.props.inputFieldValue;
    if (onBlur) onBlur();
  };

  onCancel = () => {
    this.onBlur();
    this.props.onCancelEditing();
  };

  componentDidUpdate({ inputFieldValue: prevValue }: Props) {
    const { inputFieldValue: nextValue } = this.props;
    const inputField = this.validator.$('inputField');
    if (prevValue !== nextValue) {
      inputField.set(nextValue);
    }
    if (this.props.isActive) {
      const { inputBlocked } = this.props;
      // eslint-disable-next-line no-unused-expressions
      this.inputField && !inputBlocked && this.inputField.focus();
    }
  }

  inputField: Input;
  inputFieldIsSet: boolean = false;

  render() {
    const { validator } = this;
    const {
      className,
      inputFieldLabel,
      isActive,
      inputBlocked,
      maxLength,
      inputFieldPlaceholder,
      disabled,
      readOnly,
      isSubmitting,
      errorMessage,
    } = this.props;
    let { successfullyUpdated } = this.props;
    const { intl } = this.context;
    const inputField = validator.$('inputField');
    const componentStyles = classnames([
      className,
      styles.component,
      isActive ? null : styles.inactive,
      readOnly ? styles.readOnly : null,
      isSubmitting ? styles.isSubmitting : null,
    ]);
    const inputStyles = classnames([
      successfullyUpdated ? 'input_animateSuccess' : null,
      isActive ? null : 'input_cursorPointer',
    ]);
    const buttonsWrapperStyles = classnames([
      styles.buttonsWrapper,
      readOnly ? styles.readOnly : null,
    ]);
    const editButtonStyles = classnames([styles.button, styles.editButton]);
    const cancelButtonStyles = classnames([styles.button, styles.cancelButton]);
    const okButtonStyles = classnames([styles.button, styles.okButton]);
    const submittingButtonStyles = classnames([
      styles.button,
      styles.submittingButton,
    ]);

    if (isActive) successfullyUpdated = false;

    let error = errorMessage;
    if ((isActive || inputBlocked) && inputField.error)
      error = inputField.error;

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
          placeholder={inputFieldPlaceholder || ''}
          themeOverrides={styles}
          type="text"
          maxLength={maxLength}
          label={inputFieldLabel}
          value={inputField.value}
          onChange={inputField.onChange}
          onFocus={inputField.onFocus}
          onBlur={inputField.onBlur}
          onKeyDown={(event) => this.handleInputKeyDown(event)}
          error={error}
          disabled={!isActive || disabled}
          readOnly={readOnly}
          showErrorState={!!error}
          ref={(input) => {
            this.inputField = input;
          }}
          skin={InputSkin}
        />

        <div className={buttonsWrapperStyles}>
          {!isActive &&
            !isSubmitting &&
            inputFieldLabel.length &&
            !readOnly && (
              <Button
                className={editButtonStyles}
                onClick={inputField.onFocus}
                label={<SVGInline svg={penIcon} className={styles.icon} />}
                skin={ButtonSkin}
              />
            )}
          {isActive && (
            <Button
              className={cancelButtonStyles}
              onClick={this.onCancel}
              label={<SVGInline svg={crossIcon} className={styles.icon} />}
              skin={ButtonSkin}
            />
          )}
          {isActive && (
            <Button
              className={okButtonStyles}
              onClick={this.submit}
              label={<SVGInline svg={arrowIcon} className={styles.icon} />}
              skin={ButtonSkin}
            />
          )}
          {isSubmitting && (
            <Button
              className={submittingButtonStyles}
              onClick={() => {}}
              label={<SVGInline svg={spinningIcon} className={styles.icon} />}
              label1=""
              skin={ButtonSkin}
            />
          )}
        </div>

        {successfullyUpdated && (
          <div className={styles.savingResultLabel}>
            {intl.formatMessage(messages.changesSaved)}
          </div>
        )}
      </div>
    );
  }
}
