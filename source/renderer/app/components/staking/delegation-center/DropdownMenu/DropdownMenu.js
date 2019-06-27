// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { Select } from 'react-polymorph/lib/components/Select';
import DropdownSelectSkin from './DropdownSelectSkin';
import DropdownToggle from './DropdownToggle';
import styles from './DropdownMenu.scss';

type Props = {
  label: any,
  menuItems: Array<{ value: number | string, label: any }>,
  onMenuItemClick: Function,
};

type State = {
  selectedOptionValue: any,
};

@observer
export default class DropdownMenu extends Component<Props, State> {
  state = {
    selectedOptionValue: null,
  };

  onChange = (selectedOptionValue: string) => {
    const { onMenuItemClick } = this.props;

    onMenuItemClick(selectedOptionValue);
    this.setState({ selectedOptionValue });
  };

  render() {
    const { label, menuItems } = this.props;
    const { selectedOptionValue } = this.state;

    return (
      <div className={styles.component}>
        <Select
          label={<DropdownToggle label={label} onClick={() => null} />}
          onChange={({ value }) => this.onChange(value)}
          options={menuItems}
          skin={DropdownSelectSkin}
          value={selectedOptionValue}
        />
      </div>
    );
  }
}
