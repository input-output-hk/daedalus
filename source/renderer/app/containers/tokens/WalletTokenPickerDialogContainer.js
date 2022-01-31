// @flow

import { observer, inject } from 'mobx-react';
import React, { forwardRef, useCallback, useImperativeHandle } from 'react';
import type { StatelessFunctionalComponent, Ref } from 'react';
import type { ActionsMap } from '../../actions';
import type { Props as PickerProps } from '../../components/wallet/tokens/wallet-token-picker/types';
import WalletTokenPicker from '../../components/wallet/tokens/wallet-token-picker/WalletTokenPicker';
import type { StoresMap } from '../../stores';

export type Api = {
  close: () => void,
  open: () => void,
};

type Props = PickerProps & {
  actions: ActionsMap,
  forwardedRef: Ref<Api>,
  stores: StoresMap,
};

const WalletTokenPickerDialogContainer: StatelessFunctionalComponent<Props> = ({
  actions,
  forwardedRef,
  stores,
  ...pickerProps
}: Props) => {
  const isOpen = useCallback(() => stores.uiDialogs.isOpen(WalletTokenPicker), [
    stores,
  ]);

  const close = useCallback(() => {
    if (!isOpen()) return;
    actions.dialogs.closeActiveDialog.trigger();
  }, [actions, isOpen]);

  useImperativeHandle(
    forwardedRef,
    () =>
      ({
        close,
        open: () => {
          actions.dialogs.open.trigger({
            dialog: WalletTokenPicker,
          });
        },
      }: Api),
    [actions, close]
  );

  return isOpen() ? <WalletTokenPicker {...pickerProps} /> : null;
};

export default inject(
  'stores',
  'actions'
)(
  observer(
    forwardRef((props, ref) => (
      <WalletTokenPickerDialogContainer {...props} forwardedRef={ref} />
    ))
  )
);
