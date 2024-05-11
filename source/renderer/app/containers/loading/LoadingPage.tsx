import React, { FC, useState, useEffect } from 'react';
import { observer } from 'mobx-react';
import CenteredLayout from '../../components/layout/CenteredLayout';
import NoDiskSpaceErrorPage from './NoDiskSpaceErrorPage';
import SystemTimeErrorPage from './SystemTimeErrorPage';
import SyncingConnectingPage from './SyncingConnectingPage';
import type { InjectedProps } from '../../types/injectedPropsType';

interface LoadingPageProps extends InjectedProps {}

const LoadingPage: FC<LoadingPageProps> = ({ stores, actions }) => {
  const { networkStatus } = stores;

  const [activeOverlay, setActiveOverlay] = useState<JSX.Element | null>(null);

  useEffect(() => {
    if (networkStatus.isNotEnoughDiskSpace) {
      setActiveOverlay(<NoDiskSpaceErrorPage />);
    } else if (
      !networkStatus.isSystemTimeCorrect &&
      !networkStatus.isNodeStopping &&
      !networkStatus.isNodeStopped
    ) {
      setActiveOverlay(<SystemTimeErrorPage />);
    } else {
      setActiveOverlay(null);
    }
  }, [
    networkStatus.isNotEnoughDiskSpace,
    networkStatus.isSystemTimeCorrect,
    networkStatus.isNodeStopping,
    networkStatus.isNodeStopped,
  ]);

  return (
    <CenteredLayout>
      <SyncingConnectingPage stores={stores} actions={actions} />
      {activeOverlay}
    </CenteredLayout>
  );
};

export default observer(LoadingPage);
