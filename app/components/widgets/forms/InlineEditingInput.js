// @flow
import React, { Component, } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import classnames from 'classnames';
import Input from 'react-polymorph/lib/components/Input';
import SimpleInputSkin from 'react-polymorph/lib/skins/simple/raw/InputSkin';
import ReactToolboxMobxForm from '../../../utils/ReactToolboxMobxForm';
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

  submit = () => {
    this.validator.submit({
      onSuccess: (form) => {
        const { inputField } = form.values();
        if (inputField !== this.props.inputFieldValue) {
          this.props.onSubmit(inputField);
          this.props.onStopEditing();
        } else {
          this.props.onCancelEditing();
        }
        this.setState({ isActive: false });
      }
    });
  };

  handleInputKeyDown = (event: KeyboardEvent) => {
    if (event.which === 13) { // ENTER key
      this.onBlur();
    }
    if (event.which === 27) { // ESCAPE key
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
      this.inputField.focus();
    }
  }

  inputField: Input;

  render() {
    const { validator } = this;
    const {
      className,
      inputFieldLabel,
      isActive,
      inputFieldValue,
      successfullyUpdated
    } = this.props;
    const { intl } = this.context;
    const inputField = validator.$('inputField');
    const componentStyles = classnames([
      className,
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
        onBlur={this.onBlur}
        onClick={this.onFocus}
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
          skin={<SimpleInputSkin />}
        />

        {isActive && (
          <button
            className={styles.button}
            onMouseDown={this.onCancel}
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
