// @flow
import React, { Component } from 'react';
import type { Node } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import SVGInline from 'react-svg-inline';
import classnames from 'classnames';
import { Select } from 'react-polymorph/lib/components/Select';
import { SelectSkin } from 'react-polymorph/lib/skins/simple/SelectSkin';
import styles from './InlineEditingDropdown.scss';
import questionMarkIcon from '../../../assets/images/question-mark.inline.svg';

const messages = defineMessages({
  changesSaved: {
    id: 'inline.editing.dropdown.changesSaved',
    defaultMessage: '!!!Your changes have been saved',
    description:
      'Message "Your changes have been saved" for inline editing (eg. on Wallet Settings page).',
  },
});

type Props = {
  className?: string,
  isActive: boolean,
  label: string,
  tooltip?: string | Node,
  options: Array<{ value: number | string, label: string }>,
  value: number | string,
  onSubmit: Function,
  onStartEditing: Function,
  onStopEditing: Function,
  successfullyUpdated: boolean,
};

@observer
export default class InlineEditingDropdown extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  onChange = (value: number | string) => {
    this.props.onStartEditing();
    this.props.onSubmit(value);
    this.props.onStopEditing();
  };

  render() {
    const { intl } = this.context;
    const {
      className,
      isActive,
      label,
      tooltip,
      options,
      value,
      successfullyUpdated,
    } = this.props;
    const componentClasses = classnames([className, styles.component]);
    const dropdownStyles = classnames([
      successfullyUpdated ? 'dropdown_animateSuccess' : null,
    ]);

    const labelText = [
      label,
      !!tooltip && (
        <PopOver content={tooltip} key="tooltip">
          <SVGInline
            svg={questionMarkIcon}
            className={styles.questionMarkIcon}
          />
        </PopOver>
      ),
    ];

    return (
      <div className={componentClasses}>
        <Select
          className={dropdownStyles}
          label={labelText}
          options={options}
          value={value}
          onChange={this.onChange}
          disabled={!isActive}
          skin={SelectSkin}
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
