// @flow
import React, { Component } from 'react';
import type { Node } from 'react';
import { intlShape, defineMessages } from 'react-intl';
import classnames from 'classnames';
import LoadingSpinner from '../LoadingSpinner';
import styles from './ItemDropdownOption.scss';

const messages = defineMessages({
  syncingLabel: {
    id: 'widgets.itemsDropdown.option.syncingLabel',
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

  renderLabel = () => {
    const { intl } = this.context;
    const defaultSyncingLabel = intl.formatMessage(messages.syncingLabel);
    const { label, isSyncing, syncingLabel = defaultSyncingLabel } = this.props;
    return (
      <div className={styles.label}>
        {label}
        {isSyncing && typeof label === 'string' && (
          <span className={styles.syncingLabel}>{syncingLabel}</span>
        )}
      </div>
    );
  };

  renderDetail = () => {
    const { detail, isSyncing } = this.props;
    if (!detail || isSyncing) return null;
    return <div className={styles.detail}>{detail}</div>;
  };

  renderSyncingSpinner = () => {
    const { isSyncing, selected } = this.props;
    if (!isSyncing) return null;
    const syncingSpinnerStyles = classnames(styles.syncingSpinner, {
      [styles.selected]: selected,
    });
    return (
      <div className={syncingSpinnerStyles}>
        <LoadingSpinner medium />
      </div>
    );
  };

  render() {
    const { selected, detail, isSyncing } = this.props;
    const componentStyles = classnames(styles.component, {
      [styles.selected]: selected,
      [styles.noDetail]: !detail || isSyncing,
    });
    return (
      <div className={componentStyles}>
        {this.renderLabel()}
        {this.renderDetail()}
        {this.renderSyncingSpinner()}
      </div>
    );
  }
}
