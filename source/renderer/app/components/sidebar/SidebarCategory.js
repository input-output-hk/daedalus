// @flow
import React, { Component } from 'react';
import SVGInline from 'react-svg-inline';
import { observer } from 'mobx-react';
import classNames from 'classnames';
import InstructionsDialog from '../wallet/paper-wallet-certificate/InstructionsDialog';
import styles from './SidebarCategory.scss';

type Props = {
  icon: string,
  active: boolean,
  onClick: Function,
  openDialogAction?: Function,
  className: string,
};

@observer
export default class SidebarCategory extends Component<Props> {
  render() {
    const { icon, active, className } = this.props;
    const componentStyles = classNames([
      styles.component,
      active ? styles.active : null,
      className === 'supportRequest' ? styles.supportRequest : className
    ]);

    const iconStyles = classNames([
      className === 'supportRequest' ? styles.supportRequestIcon : styles.icon
    ]);

    return (
      <button className={componentStyles} onClick={this.handleClick.bind(this, className)}>
        <SVGInline svg={icon} className={iconStyles} />
      </button>
    );
  }

  handleClick = (className: string) => {
    if (className === 'paper-wallet-create-certificate' && this.props.openDialogAction) {
      this.props.openDialogAction({
        dialog: InstructionsDialog
      });
    } else {
      this.props.onClick();
    }
  }
}
