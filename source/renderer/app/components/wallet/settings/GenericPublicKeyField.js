// @flow
import React, { useCallback, useState } from 'react';
import { observer } from 'mobx-react'; // 6.x or mobx-react-lite@1.4.0
import SVGInline from 'react-svg-inline';
import classnames from 'classnames';
import { Button } from 'react-polymorph/lib/components/Button';
import { Input } from 'react-polymorph/lib/components/Input';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import GenericPublicKeyFieldSkin from './GenericPublicKeyFieldSkin';
import qrCodeImage from '../../../assets/images/qr-code.inline.svg';
import revealKeyImage from '../../../assets/images/reveal-key.inline.svg';
import hideKeyImage from '../../../assets/images/hide-key.inline.svg';
import globalMessages from '../../../i18n/global-messages';
import type { Locale } from '../../../../../common/types/locales.types';
import { LOCALES } from '../../../../../common/types/locales.types';
import styles from './GenericPublicKeyField.scss';

type Props = {
  publicKey: string,
  locale: Locale,
  onCopyPublicKey: Function,
  onShowQRCode: Function,
  onOpenWalletKeyDialog: Function,
  t: Function,
  messages: Object,
};

const GenericPublicKeyField = observer((props: Props) => {
  const [publicKeyHidden, setPublicKeyHidden] = useState<boolean>(true);

  const togglePublicKeyVisibility = useCallback(() => {
    const { publicKey, onOpenWalletKeyDialog } = props;
    if (!publicKey) {
      onOpenWalletKeyDialog();
    } else {
      setPublicKeyHidden((prevCheck: boolean) => !prevCheck);
    }
  });

  const handleCopyPublicKey = useCallback(
    () => props.onCopyPublicKey(props.publicKey),
    []
  );

  const { publicKey, onShowQRCode, locale, t, messages } = props;
  const label = t(messages.publicKey);
  const fieldStyles = classnames([
    styles.field,
    publicKeyHidden || !publicKey ? styles.valueHidden : styles.valueShown,
    locale === LOCALES.japanese ? styles.withBigToggleButton : null,
  ]);
  const hiddenValuePlaceholder = t(messages.publicKeyShowInstruction);

  const toggleButtonTooltip = t(
    globalMessages[publicKeyHidden ? 'reveal' : 'hide']
  );

  const qrCodeButtonStyles = classnames([
    styles.imageButton,
    styles.qrCodeButton,
    'flat',
  ]);

  const revealHideButtonStyles = classnames([
    styles.imageButton,
    publicKeyHidden ? styles.revealButton : styles.hideButton,
    'flat',
  ]);

  return (
    <div className={styles.component}>
      <Input
        className={fieldStyles}
        type="text"
        label={label}
        value={publicKeyHidden ? hiddenValuePlaceholder : publicKey}
        readOnly
        skin={GenericPublicKeyFieldSkin}
        tooltip={t(globalMessages.copy)}
        valueVisible={!publicKeyHidden}
        onCopyValue={handleCopyPublicKey}
      />
      <div className={styles.addons}>
        {!publicKeyHidden && (
          <div className={styles.imageButtonContainer}>
            <PopOver content={t(messages.showQRCode)}>
              <Button
                className={qrCodeButtonStyles}
                onClick={onShowQRCode}
                label={<SVGInline svg={qrCodeImage} />}
              />
            </PopOver>
          </div>
        )}
        <PopOver content={toggleButtonTooltip}>
          <Button
            className={revealHideButtonStyles}
            label={
              publicKeyHidden ? (
                <SVGInline svg={revealKeyImage} />
              ) : (
                <SVGInline svg={hideKeyImage} />
              )
            }
            onClick={togglePublicKeyVisibility}
          />
        </PopOver>
      </div>
    </div>
  );
});

export default GenericPublicKeyField;
