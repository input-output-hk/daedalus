// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import classnames from 'classnames';
import Input from 'react-toolbox/lib/input/Input';
import MobxReactForm from 'mobx-react-form';
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
});

@observer
export default class InlineEditingInput extends Component {
  static propTypes = {
    isActive: PropTypes.bool.isRequired,
    inputFieldLabel: PropTypes.string.isRequired,
    inputFieldValue: PropTypes.string.isRequired,
    onStartEditing: PropTypes.func.isRequired,
    onStopEditing: PropTypes.func.isRequired,
    onCancelEditing: PropTypes.func.isRequired,
    onSubmit: PropTypes.func.isRequired,
    isValid: PropTypes.func.isRequired,
    validationErrorMessage: PropTypes.string.isRequired,
    successfullyUpdated: PropTypes.bool.isRequired
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  validator = new MobxReactForm({
    options: {
      validateOnChange: false
    },
    fields: {
      inputField: {
        value: this.props.inputFieldValue,
        validate: [({ field }) => {
          return [this.props.isValid(field.value), this.props.validationErrorMessage];
        }]
      }
    }
  }, {});

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
      this.refs.inputField.refs.wrappedInstance.focus();
    }
  }

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
    const inputStyles = classnames([
      successfullyUpdated ? 'input_animateSuccess' : null,
    ]);
    const savingResultLabel = classnames([
      successfullyUpdated ? styles.savingResultLabelVisible : styles.savingResultLabelInvisible,
    ]);
    return (
      <div
        className={styles.component}
        onBlur={this.submit.bind(this)}
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
          ref="inputField"
        />
        {!isActive && (
          <button
            className={styles.button}
            onClick={onStartEditing}
          >
            {intl.formatMessage(messages.change)}
          </button>
        )}
        {isActive && (
          <button
            className={styles.button}
            onClick={onCancelEditing}
          >
            {intl.formatMessage(messages.cancel)}
          </button>
        )}
        <div className={savingResultLabel}>Your changes have been saved</div>
      </div>
    );
  }

}
