import React, { Component } from 'react';
// @ts-ignore ts-migrate(2305) FIXME: Module '"react"' has no exported member 'Node'.
import type { Node } from 'react';
import { observer } from 'mobx-react';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './SettingsLayout.scss' or its ... Remove this comment to see the full error message
import styles from './SettingsLayout.scss';

type Props = {
  children: Node;
  menu: Node;
  activePage: string;
};

@observer
class SettingsLayout extends Component<Props> {
  scrollableDomElement: HTMLElement | null | undefined = null;

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

export default SettingsLayout;
