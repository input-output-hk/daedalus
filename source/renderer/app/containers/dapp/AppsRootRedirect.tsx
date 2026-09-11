import React from 'react';
import { inject, observer } from 'mobx-react';
import { Redirect } from 'react-router-dom';
import { ROUTES } from '../../routes-config';
import type { InjectedProps } from '../../types/injectedPropsType';
import { buildRoute } from '../../utils/routing';

type Props = { stores?: InjectedProps['stores'] };

const AppsRootRedirect = ({ stores }: Props) => {
  const activeWallet = stores!.wallets.active;
  const wallet =
    activeWallet && !activeWallet.isLegacy
      ? activeWallet
      : stores!.wallets.allWallets[0];
  return (
    <Redirect
      to={
        wallet
          ? buildRoute(ROUTES.APPS.PAGE, { id: wallet.id })
          : ROUTES.WALLETS.ROOT
      }
    />
  );
};

export default inject('stores')(observer(AppsRootRedirect));
