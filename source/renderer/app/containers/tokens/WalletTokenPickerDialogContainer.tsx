import { observer, inject } from 'mobx-react';
import React, { forwardRef, useCallback, useImperativeHandle } from 'react';
import type { StatelessFunctionalComponent } from 'react';
import type { ActionsMap } from '../../actions';
import type { Props as PickerProps } from '../../components/wallet/tokens/wallet-token-picker/types';
import WalletTokenPicker from '../../components/wallet/tokens/wallet-token-picker/WalletTokenPicker';
import type { StoresMap } from '../../stores';
export type Api = {
  open: () => void;
};
type Props = PickerProps & {
  actions: ActionsMap;
  // will be changed to correct type after migrating to TS
  forwardedRef: any;
  onAdd: () => void;
  // will be removed and omitted in PickerProps after migrating to TS
  onCancel: any;
  stores: StoresMap;
};

const WalletTokenPickerDialogContainer: StatelessFunctionalComponent<Props> = ({
  actions,
  forwardedRef,
  onAdd,
  onCancel,
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
  const add = useCallback(
    (checked) => {
      onAdd(checked);
      close();
    },
    [close, onAdd]
  );
  useImperativeHandle(
    forwardedRef,
    () =>
      ({
        open: () => {
          actions.dialogs.open.trigger({
            dialog: WalletTokenPicker,
          });
        },
      } as Api),
    [actions, close]
  );
  return isOpen() ? (
    <WalletTokenPicker {...pickerProps} onAdd={add} onCancel={close} />
  ) : null;
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
