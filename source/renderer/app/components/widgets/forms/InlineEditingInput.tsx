/* eslint-disable react/no-did-update-set-state */
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import { get } from 'lodash';
import { Button } from 'react-polymorph/lib/components/Button';
import vjf from 'mobx-react-form/lib/validators/VJF';
import SVGInline from 'react-svg-inline';
import classnames from 'classnames';
import { Input } from 'react-polymorph/lib/components/Input';
import ReactToolboxMobxForm from '../../../utils/ReactToolboxMobxForm';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './InlineEditingInput.scss' or ... Remove this comment to see the full error message
import styles from './InlineEditingInput.scss';
import { FORM_VALIDATION_DEBOUNCE_WAIT } from '../../../config/timingConfig';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/pen.inl... Remove this comment to see the full error message
import penIcon from '../../../assets/images/pen.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/close-c... Remove this comment to see the full error message
import crossIcon from '../../../assets/images/close-cross.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/arrow-r... Remove this comment to see the full error message
import arrowIcon from '../../../assets/images/arrow-right.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/spinner... Remove this comment to see the full error message
import spinningIcon from '../../../assets/images/spinner-ic.inline.svg';
import { ENTER_KEY_CODE, ESCAPE_KEY_CODE } from '../../../config/numbersConfig';

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
  className?: string;
  label: string;
  value: string;
  placeholder?: string;
  onFocus?: (...args: Array<any>) => any;
  onCancel?: (...args: Array<any>) => any;
  onBlur?: (...args: Array<any>) => any;
  onSubmit: (...args: Array<any>) => any;
  isValid: (...args: Array<any>) => any;
  valueErrorMessage?: string | ((...args: Array<any>) => any);
  errorMessage?: string | null | undefined;
  disabled?: boolean;
  readOnly?: boolean;
  maxLength?: number;
  isLoading?: boolean;
  validateOnChange?: boolean;
  successfullyUpdated?: boolean;
};
type State = {
  isActive: boolean;
  hasChanged: boolean;
  successfullyUpdated: boolean;
};

@observer
class InlineEditingInput extends Component<Props, State> {
  static defaultProps = {
    validateOnChange: true,
    valueErrorMessage: '',
  };
  static contextTypes = {
    intl: intlShape.isRequired,
  };
  state = {
    isActive: false,
    hasChanged: false,
    successfullyUpdated: false,
  };
  validator = new ReactToolboxMobxForm(
    // @ts-ignore ts-migrate(2554) FIXME: Expected 0 arguments, but got 2.
    {
      fields: {
        inputField: {
          value: this.props.value,
          validators: [
            ({ field }) => {
              const { value } = field;
              const { valueErrorMessage } = this.props;
              const errorMessage =
                typeof valueErrorMessage === 'function'
                  ? valueErrorMessage(value)
                  : valueErrorMessage;
              return [this.props.isValid(value), errorMessage || null];
            },
          ],
        },
      },
    },
    {
      plugins: {
        vjf: vjf(),
      },
      options: {
        validateOnChange: this.props.validateOnChange,
        validationDebounceWait: FORM_VALIDATION_DEBOUNCE_WAIT,
      },
    }
  );
  submit = () => {
    // @ts-ignore ts-migrate(2339) FIXME: Property 'submit' does not exist on type 'ReactToo... Remove this comment to see the full error message
    this.validator.submit({
      onSuccess: async (form) => {
        this.setInputBlur();
        const { inputField } = form.values();
        const { onSubmit, errorMessage } = this.props;

        if (!inputField) {
          return;
        }

        if (inputField !== this.props.value || errorMessage) {
          this.setState({
            hasChanged: true,
            successfullyUpdated: false,
          });
          await onSubmit(inputField);
          this.setState({
            hasChanged: false,
            successfullyUpdated: true,
          });
        } else {
          this.setState({
            hasChanged: false,
          });
        }
      },
    });
  };
  handleInputKeyDown = (event: KeyboardEvent) => {
    if (event.which === ENTER_KEY_CODE) {
      this.submit();
    } else if (event.which === ESCAPE_KEY_CODE) {
      this.onCancel();
    }
  };
  onFocus = () => {
    const { disabled, onFocus, readOnly } = this.props;

    if (!disabled && !readOnly) {
      this.setState({
        isActive: true,
      });
      if (onFocus) onFocus();
    }
  };
  onBlur = (event: InputEvent) => {
    event.stopPropagation();
    event.preventDefault();
    const { disabled, readOnly, onBlur } = this.props;
    this.setState({
      isActive: false,
    });

    if (!disabled && !readOnly && onBlur) {
      onBlur();
    }
  };
  onCancel = () => {
    const { value, onCancel, errorMessage } = this.props;
    // @ts-ignore ts-migrate(2339) FIXME: Property '$' does not exist on type 'ReactToolboxM... Remove this comment to see the full error message
    const inputField = this.validator.$('inputField');
    const newValue = !errorMessage ? value : '';
    inputField.set(newValue);
    if (onCancel) onCancel();
    this.setInputFocus();
    this.setState({
      hasChanged: true,
    });
  };
  setInputFocus = () => {
    const input = this.inputElement;
    if (input instanceof HTMLElement) input.focus();
  };
  setInputBlur = () => {
    const input = this.inputElement;
    if (input instanceof HTMLElement) input.blur();
  };
  // @ts-ignore ts-migrate(2370) FIXME: A rest parameter must be of an array type.
  onChange = (...props: KeyboardEvent) => {
    this.setState({
      hasChanged: true,
    });
    // @ts-ignore ts-migrate(2339) FIXME: Property '$' does not exist on type 'ReactToolboxM... Remove this comment to see the full error message
    const inputField = this.validator.$('inputField');
    // @ts-ignore ts-migrate(2488) FIXME: Type 'KeyboardEvent' must have a '[Symbol.iterator... Remove this comment to see the full error message
    inputField.onChange(...props);
  };

  componentDidUpdate({ value: prevValue, errorMessage: prevError }: Props) {
    const { value: nextValue, errorMessage: nextError } = this.props;
    // @ts-ignore ts-migrate(2339) FIXME: Property '$' does not exist on type 'ReactToolboxM... Remove this comment to see the full error message
    const inputField = this.validator.$('inputField');

    // If there's an error, we focus the input again
    if (nextError) {
      this.setInputFocus();
    } else if (prevError && !nextError) {
      // else we blur it
      this.setInputBlur();
    }

    // In case the `value` prop was updated
    // we need to manually update the ReactToolboxMobxForm input field
    if (prevValue !== nextValue) {
      inputField.set(nextValue);

      if (nextValue === '') {
        this.setState({
          hasChanged: false,
        });
      }
    }

    // If the `value` props was updated
    // after a submit action
    // we show the `success` message
    const successfullyUpdated = !!nextValue && prevValue !== nextValue;

    if (successfullyUpdated) {
      this.setState({
        successfullyUpdated,
      });
    }
  }

  inputElement: HTMLElement;
  preventDefaultHelper = (event: KeyboardEvent) => {
    event.preventDefault();
    event.stopPropagation();
  };

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
    const { isActive, hasChanged } = this.state;
    let { successfullyUpdated } = this.props;

    if (successfullyUpdated === undefined) {
      ({ successfullyUpdated } = this.state);
    }

    const { intl } = this.context;
    // @ts-ignore ts-migrate(2339) FIXME: Property '$' does not exist on type 'ReactToolboxM... Remove this comment to see the full error message
    const inputField = validator.$('inputField');
    let error;
    if (inputField.error) error = inputField.error;
    else if (!hasChanged) error = !!errorMessage;
    const showEditButton =
      !isActive && !isLoading && !hasChanged && label.length && !readOnly;
    const showFocusButtons =
      !isLoading && !disabled && !readOnly && (isActive || hasChanged);
    const showLoadingButton = isLoading;
    const componentStyles = classnames([
      className,
      styles.component,
      isActive ? null : styles.inactive,
      readOnly ? styles.readOnly : null,
      isLoading ? styles.isLoading : null,
      showEditButton || showLoadingButton ? styles.twoButtons : null,
      showFocusButtons ? styles.twoButtons : null,
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
    return (
      <div className={componentStyles}>
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
          error={isActive ? error : !!error}
          disabled={disabled}
          readOnly={readOnly}
          ref={(input) => {
            if (!this.inputElement) {
              this.inputElement = get(input, 'inputElement.current');
            }
          }}
        />

        <div
          className={buttonsWrapperStyles}
          // @ts-ignore ts-migrate(2322) FIXME: Type '(event: KeyboardEvent) => void' is not assig... Remove this comment to see the full error message
          onMouseDown={this.preventDefaultHelper}
          // @ts-ignore ts-migrate(2322) FIXME: Type '(event: KeyboardEvent) => void' is not assig... Remove this comment to see the full error message
          onMouseUp={this.preventDefaultHelper}
        >
          {showEditButton && (
            <Button
              className={editButtonStyles}
              onMouseUp={this.setInputFocus}
              label={<SVGInline svg={penIcon} className={styles.icon} />}
            />
          )}
          {showFocusButtons && (
            <Button
              className={cancelButtonStyles}
              onClick={this.onCancel}
              label={<SVGInline svg={crossIcon} className={styles.icon} />}
            />
          )}
          {showFocusButtons && (
            <Button
              className={okButtonStyles}
              onMouseUp={this.submit}
              label={<SVGInline svg={arrowIcon} className={styles.icon} />}
            />
          )}
          {showLoadingButton && (
            <Button
              className={submittingButtonStyles}
              onMouseUp={() => {}}
              label={<SVGInline svg={spinningIcon} className={styles.icon} />}
            />
          )}
        </div>

        {successfullyUpdated && (
          <div className={styles.savingResultLabel}>
            {intl.formatMessage(messages.changesSaved)}
          </div>
        )}

        {errorMessage && !hasChanged && (
          <div className={styles.errorMessage}>{errorMessage}</div>
        )}
      </div>
    );
  }
}

export default InlineEditingInput;
