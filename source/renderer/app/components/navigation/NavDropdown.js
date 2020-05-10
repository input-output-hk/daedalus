// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import { Dropdown } from 'react-polymorph/lib/components/Dropdown';
import NavButton from './NavButton';
import styles from './NavDropdown.scss';

type Props = {
  label: string,
  activeItem: string,
  icon?: string,
  isActive: boolean,
  options: Array<{
    value: number | string,
    label: string,
  }>,
  onChange: Function,
  hasNotification?: boolean,
};

@observer
export default class NavDropdown extends Component<Props> {
  render() {
    const {
      label,
      icon,
      isActive,
      onChange,
      options,
      activeItem,
      hasNotification,
    } = this.props;
    const componentStyles = classnames([
      styles.component,
      hasNotification ? styles.hasNotification : null,
    ]);
    const getOptionLabelStyles = (optionHasNotification: boolean) =>
      classnames([
        styles.optionLabel,
        optionHasNotification ? styles.hasNotification : null,
      ]);
    return (
      <div className={componentStyles}>
        <Dropdown
          label={
            <NavButton
              label={label}
              icon={icon}
              isActive={isActive}
              onClick={() => {}}
              hasNotification={hasNotification}
            />
          }
          onItemSelected={({ value }) => {
            onChange(value);
          }}
          optionRenderer={o => (
            <div className={getOptionLabelStyles(o.hasNotification)}>
              {o.label}
            </div>
          )}
          items={options}
          activeItem={options.find(o => o.value === activeItem)}
          noArrow
          optionHeight={32}
        />
      </div>
    );
  }
}
