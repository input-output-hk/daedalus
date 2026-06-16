import React from 'react';
import { observer, inject } from 'mobx-react';
import DRepDirectory from '../../components/governance/drep-directory/DRepDirectory';
import GovernanceStore, {
  GovernanceRefreshState,
} from '../../stores/GovernanceStore';
import type { StoresMap } from '../../stores';

interface Props {
  stores?: StoresMap;
}

@inject('stores')
@observer
class DRepDirectoryPage extends React.Component<Props> {
  componentDidMount() {
    const governanceStore: GovernanceStore | undefined =
      this.props.stores?.governance;

    if (!governanceStore) {
      return;
    }

    if (
      governanceStore.refreshState === GovernanceRefreshState.Idle ||
      governanceStore.refreshState === GovernanceRefreshState.Failed
    ) {
      governanceStore.refresh();
    }
  }

  render() {
    const { stores } = this.props;
    const governanceStore: GovernanceStore | undefined = stores?.governance;

    if (!governanceStore) return null;

    return (
      <DRepDirectory
        drepList={governanceStore.drepList}
        refreshState={governanceStore.refreshState}
        error={governanceStore.error}
        lastFetchedAt={governanceStore.lastFetchedAt}
        onRefresh={() => governanceStore.refresh()}
      />
    );
  }
}

export default DRepDirectoryPage;
