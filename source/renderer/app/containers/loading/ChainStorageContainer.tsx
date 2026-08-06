import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import ChainStorageLocationPicker from '../../components/chain-storage/ChainStorageLocationPicker';
import type { InjectedProps } from '../../types/injectedPropsType';

@inject('stores', 'actions')
@observer
class ChainStorageContainer extends Component<InjectedProps> {
  static defaultProps = {
    stores: null,
    actions: null,
  };

  render() {
    const { backend } = this.props.stores;
    const {
      defaultChainPath,
      customChainPath,
      validateChainStorageDirectory,
      setChainStorageDirectory,
      resetChainStorageDirectory,
      confirmStorageLocation,
    } = backend;

    return (
      <ChainStorageLocationPicker
        defaultChainPath={defaultChainPath}
        customChainPath={customChainPath}
        onValidateChainStorageDirectory={validateChainStorageDirectory}
        onSetChainStorageDirectory={setChainStorageDirectory}
        onResetChainStorageDirectory={resetChainStorageDirectory}
        onConfirmStorageLocation={confirmStorageLocation}
      />
    );
  }
}

export default ChainStorageContainer;
