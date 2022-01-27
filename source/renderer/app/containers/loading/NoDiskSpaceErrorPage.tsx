import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import type { InjectedStoresProps } from '../../types/injectedPropsType';
import NoDiskSpaceError from '../../components/loading/no-disk-space-error/NoDiskSpaceError';

type Props = InjectedStoresProps;

@inject('stores')
@observer
class NoDiskSpaceErrorPage extends Component<Props> {
  static defaultProps = {
    stores: null,
  };

  render() {
    const { stores } = this.props;
    const {
      diskSpaceRequired,
      diskSpaceMissing,
      diskSpaceRecommended,
    } = stores.networkStatus;
    return (
      <NoDiskSpaceError
        diskSpaceRequired={diskSpaceRequired}
        diskSpaceMissing={diskSpaceMissing}
        diskSpaceRecommended={diskSpaceRecommended}
      />
    );
  }
}

export default NoDiskSpaceErrorPage;
