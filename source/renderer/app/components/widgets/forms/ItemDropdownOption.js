// @flow
import React, { Component } from 'react';
import type { Node } from 'react';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import { intlShape, defineMessages } from 'react-intl';
import classnames from 'classnames';
import LoadingSpinner from '../LoadingSpinner';
import styles from './ItemDropdownOption.scss';

const messages = defineMessages({
  syncingLabel: {
    id: 'widgets.itemsDropdown.syncingLabel',
    defaultMessage: '!!!syncing',
    description: 'syncingLabel for ItemDropdownOption',
  },
});

export type ItemDropdown = {
  label: string | Node,
  detail?: string | Node,
  selected?: boolean,
  isSyncing?: boolean,
  syncingLabel?: string,
};

export default class ItemDropdownOption extends Component<ItemDropdown> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  renderDetail = () => {
    const { detail, isSyncing } = this.props;
    if (!detail || isSyncing) return null;
    return <div className={styles.detail}>{detail}</div>;
  };

  renderSyncingSpinner = () => {
    const { intl } = this.context;
    const defaultSyncingLabel = intl.formatMessage(messages.syncingLabel);
    const {
      isSyncing,
      selected,
      syncingLabel = defaultSyncingLabel,
    } = this.props;
    if (!isSyncing) return null;
    const syncingSpinnerStyles = classnames(styles.syncingSpinner, {
      [styles.selected]: selected,
    });
    return (
      <div className={syncingSpinnerStyles}>
        <PopOver content={syncingLabel} className={styles.syncingLabel1}>
          <LoadingSpinner medium />
        </PopOver>
      </div>
    );
  };

  render() {
    const { selected, detail, isSyncing, label } = this.props;
    const componentStyles = classnames(styles.component, {
      [styles.selected]: selected,
      [styles.noDetail]: !detail || isSyncing,
    });
    return (
      <div className={componentStyles}>
        <div className={styles.label}>{label}</div>
        {this.renderDetail()}
        {this.renderSyncingSpinner()}
      </div>
    );
  }
}
