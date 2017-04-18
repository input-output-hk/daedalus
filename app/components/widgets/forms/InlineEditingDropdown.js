// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import classnames from 'classnames';
import Dropdown from 'react-toolbox/lib/dropdown/Dropdown';
import styles from './InlineEditingDropdown.scss';

const messages = defineMessages({
  changesSaved: {
    id: 'inline.editing.dropdown.changesSaved',
    defaultMessage: '!!!Your changes have been saved',
    description: 'Message "Your changes have been saved" for inline editing (eg. on Wallet Settings page).',
  },
});

@observer
export default class InlineEditingDropdown extends Component {

  props: {
    isActive: boolean,
    label: string,
    options: Array<{ value: (number | string), label: string }>,
    value: number | string,
    onChange: Function,
    successfullyUpdated: boolean,
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const {
      isActive,
      label,
      options,
      value,
      onChange,
      successfullyUpdated,
    } = this.props;
    const dropdownStyles = classnames([
      successfullyUpdated ? 'dropdown_animateSuccess' : null,
    ]);
    return (
      <div className={styles.component}>

        <Dropdown
          className={dropdownStyles}
          label={label}
          source={options}
          value={value}
          onChange={onChange}
          disabled={!isActive}
        />

        {successfullyUpdated && (
          <div className={styles.savingResultLabel}>
            {intl.formatMessage(messages.changesSaved)}
          </div>
        )}

      </div>
    );
  }

}
