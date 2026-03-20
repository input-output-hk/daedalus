import React from 'react';
import { render, screen } from '@testing-library/react';
import '@testing-library/jest-dom';
import LoadingPage from './LoadingPage';

jest.mock('../../components/layout/CenteredLayout', () => {
  return function CenteredLayoutMock(props) {
    return <div>{props.children}</div>;
  };
});

jest.mock('./SyncingConnectingPage', () => {
  return function SyncingConnectingPageMock() {
    return <div>Syncing page</div>;
  };
});

jest.mock('./MithrilBootstrapPage', () => {
  return function MithrilBootstrapPageMock() {
    return <div>Mithril bootstrap overlay</div>;
  };
});

jest.mock('./NoDiskSpaceErrorPage', () => {
  return function NoDiskSpaceErrorPageMock() {
    return <div>No disk space</div>;
  };
});

jest.mock('./SystemTimeErrorPage', () => {
  return function SystemTimeErrorPageMock() {
    return <div>System time error</div>;
  };
});

const makeProps = (mithrilStatus = 'idle') => ({
  stores: {
    app: {
      isSetupPage: false,
    },
    networkStatus: {
      isNotEnoughDiskSpace: false,
      isSystemTimeCorrect: true,
      isNodeStopping: false,
      isNodeStopped: true,
    },
    mithrilBootstrap: {
      status: mithrilStatus,
    },
  },
  actions: null,
});

describe('LoadingPage', () => {
  it('keeps the Mithril overlay mounted when bootstrap is cancelled', () => {
    render(<LoadingPage {...(makeProps('cancelled') as any)} />);

    expect(screen.getByText('Syncing page')).toBeInTheDocument();
    expect(screen.getByText('Mithril bootstrap overlay')).toBeInTheDocument();
  });
});
