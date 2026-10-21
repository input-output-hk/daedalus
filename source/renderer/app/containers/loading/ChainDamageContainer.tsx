import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import type { InjectedProps } from '../../types/injectedPropsType';
import ChainDamageView from '../../components/loading/chain-damage/ChainDamageView';
import styles from '../../components/loading/mithril/MithrilBootstrap.scss';

@inject('stores', 'actions')
@observer
class ChainDamageContainer extends Component<InjectedProps> {
  static defaultProps = {
    stores: null,
    actions: null,
  };

  render() {
    const { chainDamage, nodeHeld, rebuildChain, dismissChainDamage } =
      this.props.stores.backend;
    if (chainDamage == null) return null;

    // Not now would leave a loading screen waiting for a node the watchdog
    // does not start, so it is offered only while a node runs.
    return (
      <div className={styles.card}>
        <ChainDamageView
          damage={chainDamage}
          onRebuild={rebuildChain}
          onDismiss={nodeHeld ? undefined : dismissChainDamage}
        />
      </div>
    );
  }
}

export default ChainDamageContainer;
