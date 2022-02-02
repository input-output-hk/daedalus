import React, { Component } from 'react';
// @ts-ignore ts-migrate(2305) FIXME: Module '"react"' has no exported member 'Node'.
import type { Node } from 'react';
import SVGInline from 'react-svg-inline';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import { intlShape, defineMessages } from 'react-intl';
import classnames from 'classnames';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './ItemDropdownOption.scss' or ... Remove this comment to see the full error message
import styles from './ItemDropdownOption.scss';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/spinner... Remove this comment to see the full error message
import tinySpinnerIcon from '../../../assets/images/spinner-tiny.inline.svg';

const messages = defineMessages({
  syncingLabel: {
    id: 'widgets.itemsDropdown.syncingLabel',
    defaultMessage: '!!!Syncing',
    description: 'syncingLabel for ItemDropdownOption',
  },
  syncingLabelProgress: {
    id: 'widgets.itemsDropdown.syncingLabelProgress',
    defaultMessage: '!!!Syncing {syncingProgress}%',
    description: 'syncingLabel for WalletsDropdown',
  },
});
export type ItemDropdown = {
  label: string | Node;
  detail?: string | Node;
  selected?: boolean;
  isSyncing?: boolean;
  syncingLabel?: string;
  syncingProgress?: number;
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
    const { syncingProgress } = this.props;
    const defaultSyncingLabel =
      typeof syncingProgress === 'number' || typeof syncingProgress === 'string'
        ? intl.formatMessage(messages.syncingLabelProgress, {
            syncingProgress,
          })
        : intl.formatMessage(messages.syncingLabel);
    const { isSyncing, syncingLabel = defaultSyncingLabel } = this.props;
    if (!isSyncing) return null;
    return (
      <div className={styles.syncingSpinner}>
        <PopOver content={syncingLabel} className={styles.syncingLabel}>
          <SVGInline svg={tinySpinnerIcon} className={styles.tinySpinner} />
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
        <div className={styles.label}>
          {label}
          {this.renderSyncingSpinner()}
        </div>
        {this.renderDetail()}
      </div>
    );
  }
}
