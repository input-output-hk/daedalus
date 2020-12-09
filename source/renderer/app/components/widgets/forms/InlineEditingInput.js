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
  label: string,
  value: string,
  placeholder?: string,
  onStartEditing?: Function,
  onStopEditing?: Function,
  onCancelEditing?: Function,
  onBlur?: Function,
  onSubmit: Function,
  isValid: Function,
  valueErrorMessage?: string,
  errorMessage?: ?string,
  successfullyUpdated: boolean,
  disabled?: boolean,
  readOnly?: boolean,
  maxLength?: number,
  isLoading?: boolean,
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
          value: this.props.value,
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
        if (inputField !== this.props.value) {
          onSubmit(inputField);
          if (onStopEditing) onStopEditing();
        } else if (inputField !== '') {
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

  onFocus = (e) => {
    console.log('onFocus');
    // e.persist()
    // e.stopPropagation();
    // e.preventDefault();
    const { onStartEditing } = this.props;
    if (this.props.readOnly) return;
    if (onStartEditing) onStartEditing();
  };

  onBlur = (e) => {
    console.log('onBlur');
    e.persist();
    e.stopPropagation();
    e.preventDefault();
    const { onBlur } = this.props;
    if (onBlur) onBlur();
  };

  onCancel = (e) => {
    console.log('onCancel');
    // if (e) {
    //   e.persist()
    //   // e.stopPropagation();
    //   e.preventDefault();
    // }
    const inputField = this.validator.$('inputField');
    const { value, onCancelEditing } = this.props;
    inputField.set(value);
    inputField.onBlur();
    console.log('inputField', inputField);
    if (onCancelEditing) onCancelEditing();
  };

  componentDidUpdate({ value: prevValue }: Props) {
    const { value: nextValue } = this.props;
    const inputField = this.validator.$('inputField');
    if (prevValue !== nextValue) {
      inputField.set(nextValue);
    }
    if (this.props.isActive) {
      // eslint-disable-next-line no-unused-expressions
      this.inputField && this.inputField.focus();
    }
  }

  inputField: Input;
  inputFieldIsSet: boolean = false;

  render() {
    const { validator } = this;
    const {
      className,
      label,
      isActive,
      maxLength,
      placeholder,
      disabled,
      readOnly,
      isLoading,
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
      isLoading ? styles.isLoading : null,
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

    let error = inputField.error ? inputField.error : errorMessage;

    return (
      <div className={componentStyles} role="presentation" aria-hidden>
        <Input
          {...inputField.bind()}
          className={inputStyles}
          placeholder={placeholder || ''}
          themeOverrides={styles}
          type="text"
          maxLength={maxLength}
          label={label}
          onMouseUp={this.onFocus}
          onBlur={this.onBlur}
          onKeyDown={(event) => this.handleInputKeyDown(event)}
          error={error}
          disabled={disabled}
          readOnly={readOnly}
          showErrorState={!!error}
          ref={(input) => {
            this.inputField = input;
          }}
          skin={InputSkin}
        />

        <div
          className={buttonsWrapperStyles}
          onMouseUp={(e) => {
            console.log('CLICK buttonsWrapperStyles');
            e.preventDefault();
            e.stopPropagation();
          }}
        >
          {!isActive && !isLoading && label.length && !readOnly && (
            <Button
              className={editButtonStyles}
              onMouseUp={this.onFocus}
              label={<SVGInline svg={penIcon} className={styles.icon} />}
              skin={ButtonSkin}
            />
          )}
          {isActive && (
            <Button
              className={cancelButtonStyles}
              onMouseUp={this.onCancel}
              label={<SVGInline svg={crossIcon} className={styles.icon} />}
              skin={ButtonSkin}
            />
          )}
          {isActive && (
            <Button
              className={okButtonStyles}
              onMouseUp={this.submit}
              label={<SVGInline svg={arrowIcon} className={styles.icon} />}
              skin={ButtonSkin}
            />
          )}
          {isLoading && (
            <Button
              className={submittingButtonStyles}
              onMouseUp={() => {}}
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
