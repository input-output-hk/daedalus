// @flow
import React, { Component, useCallback, useState } from 'react';
import ReactModal from 'react-modal';
import { observer } from 'mobx-react';
import SVGInline from 'react-svg-inline';
import classnames from 'classnames';
import { get } from 'lodash';
import {
  defineMessages,
  FormattedHTMLMessage,
  intlShape,
  injectIntl,
} from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { Checkbox } from 'react-polymorph/lib/components/Checkbox';
import { SwitchSkin } from 'react-polymorph/lib/skins/simple/SwitchSkin';
import { Link } from 'react-polymorph/lib/components/Link';
import { LinkSkin } from 'react-polymorph/lib/skins/simple/LinkSkin';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import { ButtonSpinnerSkin } from 'react-polymorph/lib/skins/simple/ButtonSpinnerSkin';
import ReactMarkdown from 'react-markdown';
import styles from './AnalyticsDIalog.scss';
import DialogCloseButton from '../widgets/DialogCloseButton';
import ProgressBarLarge from '../widgets/ProgressBarLarge';
import externalLinkIcon from '../../assets/images/link-ic.inline.svg';
import closeCrossThin from '../../assets/images/close-cross-thin.inline.svg';
import type { InjectedProps } from '../../types/injectedPropsType';
import TopBar from '../layout/TopBar';
import About from '../static/About';
import classNames from 'classnames';
import NormalSwitch from '../widgets/forms/NormalSwitch';
import { Intl } from '../../types/i18nTypes';

const messages = defineMessages({
  title: {
    id: 'analytics.dialog.title',
    defaultMessage: '!!!Anonymous data collection',
    description: 'Analytics dialog title',
  },
  description: {
    id: 'analytics.dialog.description',
    defaultMessage:
      '!!!All data is anonymous and is used only for product development purposes. Read more in Terms and Conditions.',
    description: 'Analytics data collection description',
  },
  dataCollectionDetailsTitle: {
    id: 'analytics.dialog.dataCollectionDetailsTitle',
    defaultMessage: '!!!What data do we collect?',
    description: 'Data collection details title',
  },
  dataCollectionDetailsUserBehaviour: {
    id: 'analytics.dialog.dataCollectionDetailsUserBehaviour',
    defaultMessage: '!!!User behavior (where the user clicks)',
    description: 'Description for the user behaviour data collection',
  },
  dataCollectionDetailsDeviceInfo: {
    id: 'analytics.dialog.dataCollectionDetailsDeviceInfo',
    defaultMessage: '!!!Device info (OS, RAM, disk space, etc)',
    description: 'Description for the device info data collection',
  },
  dataCollectionSwitchButton: {
    id: 'analytics.dialog.dataCollectionSwitchText',
    defaultMessage: '!!!Allow anonymous data collection',
    description: 'Data collection agreement switch button label',
  },
  confirmButton: {
    id: 'analytics.dialog.confirmButton',
    defaultMessage: '!!!Confirm',
    description: 'Analytics data collection confirmation button text',
  },
});

type Props = {
  intl: Intl,
};

const AnalyticsDialog = ({ intl }: Props) => {
  const [open, setOpen] = useState(true);
  const toggleOpen = useCallback(() => {
    setOpen((prevOpen) => !prevOpen);
  }, [setOpen]);

  return (
    <div className={styles.component}>
      <div className={styles.centeredBox}>
        <p className={styles.title}>{intl.formatMessage(messages.title)}</p>
        <p className={styles.description}>
          {intl.formatMessage(messages.description)}
        </p>
        {/* TODO create a common accordion/expandable component, based on DappTransactionRequest.js - metadata toggle */}
        <p className={styles.dataCollectionTitle}>
          {intl.formatMessage(messages.dataCollectionDetailsTitle)}
        </p>
        <ol className={styles.dataCollectionList}>
          <li>
            {intl.formatMessage(messages.dataCollectionDetailsUserBehaviour)}
          </li>
          <li>
            {intl.formatMessage(messages.dataCollectionDetailsDeviceInfo)}
          </li>
        </ol>
        <hr className={styles.hr} />
        <NormalSwitch
          onChange={toggleOpen}
          checked={open}
          label={intl.formatMessage(messages.dataCollectionSwitchButton)}
          className={styles.switch}
        />
        <Button
          className={styles.submitButton}
          label={intl.formatMessage(messages.confirmButton)}
          skin={ButtonSpinnerSkin}
          loading={false}
          onClick={() => {}}
        />
      </div>
    </div>
  );
};

export default injectIntl(AnalyticsDialog);
