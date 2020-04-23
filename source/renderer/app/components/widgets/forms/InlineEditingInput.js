// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import classnames from 'classnames';
import { Input } from 'react-polymorph/lib/components/Input';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import ReactToolboxMobxForm from '../../../utils/ReactToolboxMobxForm';
import styles from './InlineEditingInput.scss';
import { FORM_VALIDATION_DEBOUNCE_WAIT } from '../../../config/timingConfig';

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
  onStartEditing: Function,
  onStopEditing: Function,
  onCancelEditing: Function,
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
export default class InlineEditingInput extends Component<Props, State> {
  state = {
    isActive: false,
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
        if (inputField !== this.props.inputFieldValue) {
          this.props.onSubmit(inputField);
          this.props.onStopEditing();
        } else {
          this.props.onCancelEditing();
        }
        this.setState({ isActive: false });
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
    this.props.onStartEditing();
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
    this.props.onCancelEditing();
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
      isActive,
      inputBlocked,
      maxLength,
    } = this.props;
    let { successfullyUpdated } = this.props;
    const { intl } = this.context;
    const inputField = validator.$('inputField');
    const componentStyles = classnames([
      className,
      styles.component,
      isActive ? null : styles.inactive,
    ]);
    const inputStyles = classnames([
      successfullyUpdated ? 'input_animateSuccess' : null,
      isActive ? null : 'input_cursorPointer',
    ]);

    if (isActive) successfullyUpdated = false;

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

        {isActive && (
          <button className={styles.button} onMouseDown={this.onCancel}>
            {intl.formatMessage(messages.cancel)}
          </button>
        )}

        {successfullyUpdated && (
          <div className={styles.savingResultLabel}>
            {intl.formatMessage(messages.changesSaved)}
          </div>
        )}
      </div>
    );
  }
}
