// @flow
import React, { Component } from 'react';
import type { Node } from 'react';
import { observer } from 'mobx-react';
import styles from './SettingsLayout.scss';

type Props = {
  children: Node,
  menu: Node,
  activePage: string,
};

@observer
export default class SettingsLayout extends Component<Props> {
  scrollableDomElement: ?HTMLElement = null;

  componentDidMount() {
    this.scrollableDomElement = document.querySelector(
      '.SettingsLayout_settingsPaneWrapper'
    );
  }

  componentDidUpdate(prevProps: Props) {
    const didActivePageChange = this.props.activePage !== prevProps.activePage;
    if (
      this.scrollableDomElement instanceof HTMLElement &&
      didActivePageChange
    ) {
      this.scrollableDomElement.scrollTop = 0;
    }
  }

  render() {
    const { menu, children } = this.props;
    return (
      <div className={styles.component}>
        <div className={styles.settingsPaneWrapper}>
          <div className={styles.settingsPane}>{children}</div>
        </div>
        {menu}
      </div>
    );
  }
}
