import React, { useCallback, useState, useEffect } from 'react';
import SVGInline from 'react-svg-inline';
import classnames from 'classnames';
import { Button } from 'react-polymorph/lib/components/Button';
import { Input } from 'react-polymorph/lib/components/Input';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import { injectIntl, intlShape } from 'react-intl';
import PublicKeyFieldSkin from './PublicKeyFieldSkin';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/qr-code... Remove this comment to see the full error message
import qrCodeImage from '../../../assets/images/qr-code.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/reveal-... Remove this comment to see the full error message
import revealKeyImage from '../../../assets/images/reveal-key.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/hide-ke... Remove this comment to see the full error message
import hideKeyImage from '../../../assets/images/hide-key.inline.svg';
import globalMessages from '../../../i18n/global-messages';
import type { Locale } from '../../../../../common/types/locales.types';
import { LOCALES } from '../../../../../common/types/locales.types';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './PublicKeyField.scss' or its ... Remove this comment to see the full error message
import styles from './PublicKeyField.scss';
import type { ReactIntlMessage } from '../../../types/i18nTypes';

type Props = {
  publicKey: string;
  locale: Locale;
  onCopyPublicKey: (...args: Array<any>) => any;
  onShowQRCode: (...args: Array<any>) => any;
  onOpenWalletKeyDialog: (...args: Array<any>) => any;
  intl: intlShape.isRequired;
  messages: Record<string, ReactIntlMessage>;
  description?: string;
};

const PublicKeyField = (props: Props) => {
  const {
    publicKey,
    onOpenWalletKeyDialog,
    onShowQRCode,
    locale,
    intl,
    messages,
    description,
    onCopyPublicKey,
  } = props;
  const [publicKeyHidden, setPublicKeyHidden] = useState<boolean>(!publicKey);
  // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
  const togglePublicKeyVisibility = useCallback(() => {
    if (!publicKey) {
      onOpenWalletKeyDialog();
    } else {
      setPublicKeyHidden((prevCheck: boolean) => !prevCheck);
    }
  });
  // This is called when the publicKey is set
  useEffect(() => {
    setPublicKeyHidden(!publicKey);
  }, [publicKey]);
  // This is called when the component is mounted the first time
  useEffect(() => {
    setPublicKeyHidden(true);
  }, []);
  const handleCopyPublicKey = useCallback(() => onCopyPublicKey(publicKey), [
    publicKey,
  ]);
  const fieldStyles = classnames([
    styles.field,
    publicKeyHidden || !publicKey ? styles.valueHidden : styles.valueShown,
    locale === LOCALES.japanese ? styles.withBigToggleButton : null,
  ]);
  const hiddenValuePlaceholder = intl.formatMessage(
    messages.publicKeyShowInstruction
  );
  const toggleButtonTooltip = intl.formatMessage(
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
      <div className={styles.title}>
        {intl.formatMessage(messages.publicKey)}
      </div>
      {!!description && <div className={styles.contentBox}>{description}</div>}
      <div className={styles.inputBox}>
        <Input
          className={fieldStyles}
          type="text"
          value={publicKeyHidden ? hiddenValuePlaceholder : publicKey}
          readOnly
          skin={PublicKeyFieldSkin}
          tooltip={intl.formatMessage(globalMessages.copy)}
          valueVisible={!publicKeyHidden}
          onCopyValue={handleCopyPublicKey}
        />
        <div className={styles.addons}>
          {!publicKeyHidden && (
            <div className={styles.imageButtonContainer}>
              <PopOver content={intl.formatMessage(messages.showQRCode)}>
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
};

export default injectIntl(PublicKeyField);
