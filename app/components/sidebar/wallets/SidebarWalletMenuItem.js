// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import classNames from 'classnames';
import styles from './SidebarWalletMenuItem.scss';
import { eclipseString } from '../../../lib/string-helpers';

@observer
export default class SidebarWalletMenuItem extends Component {

  static propTypes = {
    title: PropTypes.string.isRequired,
    info: PropTypes.string.isRequired,
    active: PropTypes.bool,
    onClick: PropTypes.func,
  };

  render() {
    const { title, info, active, onClick } = this.props;
    const componentStyles = classNames([styles.component, active ? styles.active : null]);
    return (
      <button className={componentStyles} onClick={onClick}>
        <span className={styles.meta}>
          <span className={styles.title}>{eclipseString(title, 17)}</span>
          <span className={styles.info}>{info}</span>
        </span>
      </button>
    );
  }

}
