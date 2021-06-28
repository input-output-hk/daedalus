// @flow
import React, { useCallback, useState } from 'react';
import { observer } from 'mobx-react'; // 6.x or mobx-react-lite@1.4.0
import SVGInline from 'react-svg-inline';
import classnames from 'classnames';
import { Button } from 'react-polymorph/lib/components/Button';
import { Input } from 'react-polymorph/lib/components/Input';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import { injectIntl, intlShape } from 'react-intl';
import PublicKeyFieldSkin from './PublicKeyFieldSkin';
import qrCodeImage from '../../../assets/images/qr-code.inline.svg';
import revealKeyImage from '../../../assets/images/reveal-key.inline.svg';
import hideKeyImage from '../../../assets/images/hide-key.inline.svg';
import globalMessages from '../../../i18n/global-messages';
import type { Locale } from '../../../../../common/types/locales.types';
import { LOCALES } from '../../../../../common/types/locales.types';
import styles from './PublicKeyField.scss';

type Props = {
  publicKey: string,
  locale: Locale,
  onCopyPublicKey: Function,
  onShowQRCode: Function,
  onOpenWalletKeyDialog: Function,
  intl: intlShape.isRequired,
  messages: Object,
  description?: string,
};

const PublicKeyField = observer((props: Props) => {
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

  const {
    publicKey,
    onShowQRCode,
    locale,
    intl,
    messages,
    description,
  } = props;
  const fieldStyles = classnames([
    styles.field,
    publicKeyHidden || !publicKey ? styles.valueHidden : styles.valueShown,
    locale === LOCALES.japanese ? styles.withBigToggleButton : null,
  ]);
  const hiddenValuePlaceholder = intl(messages.publicKeyShowInstruction);

  const toggleButtonTooltip = intl(
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
      <div className={styles.title}>{intl(messages.publicKey)}</div>
      {!!description && <div className={styles.contentBox}>{description}</div>}
      <div className={styles.inputBox}>
        <Input
          className={fieldStyles}
          type="text"
          value={publicKeyHidden ? hiddenValuePlaceholder : publicKey}
          readOnly
          skin={PublicKeyFieldSkin}
          tooltip={intl(globalMessages.copy)}
          valueVisible={!publicKeyHidden}
          onCopyValue={handleCopyPublicKey}
        />
        <div className={styles.addons}>
          {!publicKeyHidden && (
            <div className={styles.imageButtonContainer}>
              <PopOver content={intl(messages.showQRCode)}>
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
    </div>
  );
});

export default injectIntl(PublicKeyField);
