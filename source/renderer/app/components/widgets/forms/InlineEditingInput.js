// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import { get } from 'lodash';
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
  label: string,
  value: string,
  placeholder?: string,
  onFocus?: Function,
  onCancel?: Function,
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

type State = {
  isActive: boolean,
  hideErrorMessage: boolean,
};

@observer
export default class InlineEditingInput extends Component<Props, State> {
  static defaultProps = {
    validateOnChange: true,
    valueErrorMessage: '',
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    isActive: false,
    hideErrorMessage: false,
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
        const { onSubmit, onCancel } = this.props;
        if (inputField !== this.props.value) {
          await onSubmit(inputField);
          this.setState({
            hideErrorMessage: false,
          });
          this.setInputBlur();
        } else if (inputField !== '') {
          if (onCancel) onCancel();
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
    this.setState({
      isActive: true,
    });
    const { onFocus, readOnly } = this.props;
    if (onFocus && !readOnly) onFocus();
  };

  onBlur = (event: InputEvent) => {
    event.stopPropagation();
    event.preventDefault();
    this.setState({
      isActive: false,
    });
    const { onBlur } = this.props;
    if (onBlur) onBlur();
  };

  onCancel = () => {
    const { value, onCancel } = this.props;
    const inputField = this.validator.$('inputField');
    inputField.set(value);
    this.setInputBlur();
    if (onCancel) onCancel();
  };

  setInputBlur = () => {
    const input = this.inputElement;
    if (input instanceof HTMLElement) input.blur();
  };

  onChange = (...props: KeyboardEvent) => {
    this.setState({
      hideErrorMessage: true,
    });
    const inputField = this.validator.$('inputField');
    inputField.onChange(...props);
  };

  componentDidUpdate({ value: prevValue, errorMessage: prevError }: Props) {
    const { value: nextValue, errorMessage: nextError } = this.props;
    const inputField = this.validator.$('inputField');
    if (!prevError && nextError) {
      const input = this.inputElement;
      if (input instanceof HTMLElement) input.focus();
    }
    // In case the `value` prop was updated
    // we need to manually update the ReactToolboxMobxForm input field
    if (prevValue !== nextValue) {
      inputField.set(nextValue);
    }
  }

  inputElement: HTMLElement;

  render() {
    const { validator } = this;
    const {
      className,
      label,
      maxLength,
      placeholder,
      disabled,
      readOnly,
      isLoading,
      errorMessage,
    } = this.props;
    const { isActive, hideErrorMessage } = this.state;
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

    let error;
    if (inputField.error) error = inputField.error;
    else if (!hideErrorMessage) error = errorMessage;

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
          onFocus={this.onFocus}
          onBlur={this.onBlur}
          onChange={this.onChange}
          onKeyDown={(event) => this.handleInputKeyDown(event)}
          error={error}
          disabled={disabled}
          readOnly={readOnly}
          showErrorState={!!error}
          ref={(input) => {
            if (!this.inputElement) {
              this.inputElement = get(input, 'inputElement.current');
            }
          }}
          skin={InputSkin}
        />

        <div
          className={buttonsWrapperStyles}
          onMouseDown={(event: KeyboardEvent) => {
            event.preventDefault();
            event.stopPropagation();
          }}
          onMouseUp={(event: KeyboardEvent) => {
            event.preventDefault();
            event.stopPropagation();
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
