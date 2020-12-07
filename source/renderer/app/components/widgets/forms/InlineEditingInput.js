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
  getInputField?: Function,
  onStartEditing?: Function,
  onStopEditing?: Function,
  onCancelEditing: Function,
  onSubmit: Function,
  onSubmitEnd?: Function,
  isValid: Function,
  validationErrorMessage: string,
  successfullyUpdated: boolean,
  inputBlocked?: boolean,
  disabled?: boolean,
  readOnly?: boolean,
  maxLength?: number,
  isSubmitting?: boolean,
};

type State = {
  isActive: boolean,
};

@observer
export default class InlineEditingInput extends Component<Props, State> {
  state = {
    isActive: false,
  };

  static defaultProps = {
    onStartEditing: () => {},
    onStopEditing: () => {},
    validateOnChange: true,
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
              this.props.validationErrorMessage,
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
      onSuccess: (form) => {
        const { inputField } = form.values();
        const {
          onSubmit,
          onStopEditing,
          onCancelEditing,
          onSubmitEnd,
        } = this.props;
        if (inputField !== this.props.inputFieldValue) {
          onSubmit(inputField);
          if (onStopEditing) onStopEditing();
        } else {
          onCancelEditing();
        }
        if (!onSubmitEnd) this.setState({ isActive: false });
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
    this.setState({ isActive: true });
    if (onStartEditing) onStartEditing();
  };

  onCancel = () => {
    const inputField = this.validator.$('inputField');
    inputField.value = this.props.inputFieldValue;
    this.setState({ isActive: false });
    this.props.onCancelEditing();
  };

  componentDidUpdate({ inputFieldValue: prevValue }: Props) {
    const { inputFieldValue: nextValue } = this.props;
    if (prevValue !== nextValue) {
      const inputField = this.validator.$('inputField');
      inputField.set(nextValue);
    }
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
      isActive,
      inputBlocked,
      maxLength,
      inputFieldPlaceholder,
      disabled,
      readOnly,
      isSubmitting,
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

    return (
      <div
        className={componentStyles}
        onBlur={this.onCancel}
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
          onBlur={inputField.onCancel}
          onKeyDown={(event) => this.handleInputKeyDown(event)}
          error={isActive || inputBlocked ? inputField.error : null}
          disabled={!isActive || disabled}
          readOnly={readOnly}
          ref={(input) => {
            const { getInputField } = this.props;
            this.inputField = input;
            if (getInputField) {
              getInputField(input.inputElement);
            }
          }}
          skin={InputSkin}
          autoFocus
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
      </div>
    );
  }
}
