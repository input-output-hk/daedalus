// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import classnames from 'classnames';
import Select from 'react-polymorph/lib/components/Select';
import SelectSkin from 'react-polymorph/lib/skins/simple/SelectSkin';
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

        <Select
          className={dropdownStyles}
          label={label}
          options={options}
          value={value}
          onChange={onChange}
          disabled={!isActive}
          skin={<SelectSkin />}
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
