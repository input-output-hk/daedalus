// @flow
import React, { Component, } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import classnames from 'classnames';
import Input from 'react-toolbox/lib/input/Input';
import ReactToolboxMobxForm from '../../../lib/ReactToolboxMobxForm';
import styles from './InlineEditingInput.scss';

const messages = defineMessages({
  change: {
    id: 'inline.editing.input.change.label',
    defaultMessage: '!!!change',
    description: 'Label "change" on inline editing inputs in inactive state.'
  },
  cancel: {
    id: 'inline.editing.input.cancel.label',
    defaultMessage: '!!!cancel',
    description: 'Label "cancel" on inline editing inputs in inactive state.'
  },
  changesSaved: {
    id: 'inline.editing.input.changesSaved',
    defaultMessage: '!!!Your changes have been saved',
    description: 'Message "Your changes have been saved" for inline editing (eg. on Profile Settings page).'
  }
});

@observer
export default class InlineEditingInput extends Component {

  props: {
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
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  validator = new ReactToolboxMobxForm({
    fields: {
      inputField: {
        value: this.props.inputFieldValue,
        validators: [({ field }) => (
          [
            this.props.isValid(field.value),
            this.props.validationErrorMessage
          ]
        )],
      }
    }
  }, {
    options: {
      validateOnChange: true,
      validationDebounceWait: 250,
    },
  });

  handleInputKeyDown = (event: KeyboardEvent) => {
    if (event.which === 13) { // ENTER key
      this.submit();
    }
    if (event.which === 27) { // ESCAPE key
      this.props.onCancelEditing();
    }
  };

  submit() {
    this.validator.submit({
      onSuccess: (form) => {
        const { inputField } = form.values();
        if (inputField !== this.props.inputFieldValue) {
          this.props.onSubmit(inputField);
          this.props.onStopEditing();
        } else {
          this.props.onCancelEditing();
        }
      }
    });
  }

  componentDidUpdate() {
    if (this.props.isActive) {
      this.inputField.getWrappedInstance().focus();
    }
  }

  inputField: Input;

  render() {
    const { validator } = this;
    const {
      inputFieldLabel,
      isActive,
      onStartEditing,
      onCancelEditing,
      inputFieldValue,
      successfullyUpdated
    } = this.props;
    const { intl } = this.context;
    const inputField = validator.$('inputField');
    const componentStyles = classnames([
      styles.component,
      isActive ? null : styles.inactive,
    ]);
    const inputStyles = classnames([
      successfullyUpdated ? 'input_animateSuccess' : null,
      isActive ? null : 'input_cursorPointer'
    ]);

    return (
      <div
        className={componentStyles}
        onBlur={this.submit.bind(this)}
        onClick={onStartEditing}
        role="presentation"
        aria-hidden
      >

        <Input
          className={inputStyles}
          type="text"
          label={inputFieldLabel}
          value={isActive ? inputField.value : inputFieldValue}
          onChange={inputField.onChange}
          onFocus={inputField.onFocus}
          onBlur={inputField.onBlur}
          onKeyDown={event => this.handleInputKeyDown(event)}
          error={isActive ? inputField.error : null}
          disabled={!isActive}
          ref={(input) => { this.inputField = input; }}
        />

        {isActive && (
          <button
            className={styles.button}
            onClick={onCancelEditing}
          >
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
