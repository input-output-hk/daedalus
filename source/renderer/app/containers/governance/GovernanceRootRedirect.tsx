import React from 'react';
import { observer, inject } from 'mobx-react';
import { Redirect } from 'react-router-dom';
import type { StoresMap } from '../../stores';
import { ROUTES } from '../../routes-config';

interface Props {
  stores?: StoresMap;
}

@inject('stores')
@observer
class GovernanceRootRedirect extends React.Component<Props> {
  render() {
    const wallets = this.props.stores?.wallets?.all ?? [];
    const anyDelegating = wallets.some((w) => w.currentDRep != null);
    return (
      <Redirect
        to={
          anyDelegating
            ? ROUTES.GOVERNANCE.DASHBOARD
            : ROUTES.GOVERNANCE.DREPS
        }
      />
    );
  }
}

export default GovernanceRootRedirect;
