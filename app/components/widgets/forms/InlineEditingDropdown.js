// @flow
import React, { Component, PropTypes } from 'react';
import { observer, PropTypes as MobxPropTypes } from 'mobx-react';
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
  static propTypes = {
    isActive: PropTypes.bool.isRequired,
    label: PropTypes.string.isRequired,
    options: MobxPropTypes.arrayOrObservableArrayOf(PropTypes.shape({
      value: PropTypes.oneOfType([
        PropTypes.number,
        PropTypes.string
      ]).isRequired,
      label: PropTypes.string.isRequired,
    })).isRequired,
    value: PropTypes.oneOfType([
      PropTypes.number,
      PropTypes.string
    ]).isRequired,
    onChange: PropTypes.func.isRequired,
    successfullyUpdated: PropTypes.bool.isRequired,
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
