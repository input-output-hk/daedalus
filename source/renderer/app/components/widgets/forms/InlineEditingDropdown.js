// @flow
import React, { Component } from 'react';
import type { Node } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import { Tooltip } from 'react-polymorph/lib/components/Tooltip';
import { TooltipSkin } from 'react-polymorph/lib/skins/simple/TooltipSkin';
import SVGInline from 'react-svg-inline';
import classnames from 'classnames';
import { Select } from 'react-polymorph/lib/components/Select';
import { SelectSkin } from 'react-polymorph/lib/skins/simple/SelectSkin';
import styles from './InlineEditingDropdown.scss';
import tooltipStyles from './InlineEditingDropdown-tooltip.scss';
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
    const componentClasses = classnames(className, styles.component);
    const dropdownStyles = classnames({
      dropdown_animateSuccess: successfullyUpdated,
    });

    const labelText = [
      label,
      !!tooltip && (
        <Tooltip
          skin={TooltipSkin}
          themeOverrides={tooltipStyles}
          tip={tooltip}
          key="tooltip"
          className={styles.tooltip}
        >
          <SVGInline
            svg={questionMarkIcon}
            className={styles.questionMarkIcon}
          />
        </Tooltip>
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
