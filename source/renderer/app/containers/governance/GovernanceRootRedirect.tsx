import React from 'react';
import { Redirect } from 'react-router-dom';
import { ROUTES } from '../../routes-config';

class GovernanceRootRedirect extends React.Component {
  render() {
    return <Redirect to={ROUTES.GOVERNANCE.DASHBOARD} />;
  }
}

export default GovernanceRootRedirect;
