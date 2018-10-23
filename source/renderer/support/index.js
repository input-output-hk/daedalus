// @flow
import { ipcRenderer } from 'electron';
import { SUPPORT_WINDOW } from '../../common/ipc-api';
import waitForExist from './utils/waitForExist';
import getOSInfo from './utils/getOSInfo';

type ZendeskInfo = {
  locale: string,
  themeVars: {
    '--theme-support-widget-header-color': string
  },
  network: string,
  locale: string,
  version: string,
  buildNumber: string,
  os: string,
  release: string,
};

type LogsInfo = {
  compressedLogsFileData: any,
  compressedLogsFileName: string,
  userConsentText: string,
};

const localesSetLanguage = {
  'en-US': 'en-US',
  'ja-JP': 'ja',
};

const localesFillForm = {
  'en-US': 'English',
  'ja-JP': 'Japanese',
};

const zenDeskFormSelects = [
  'product',
  'supportLanguage',
  'operatingSystem',
  'productVersion',
  'productAttribute',
];

let logsWereAttached = false;

const onSubmit = async (iframe) => {
  const doneButton:any = waitForExist(
    '.src-component-submitTicket-SubmitTicket-button',
    { context: iframe.contentDocument }
  );
  doneButton.onclick = closeWindow;
};

const addFormEventListeners = async (iframe: window) => {
  const form = await waitForExist('form', { context: iframe.contentDocument });
  const [cancelButton, successButton] = form.querySelectorAll('footer button');
  const minimizeButton = iframe.contentDocument.querySelector('[aria-label="Close"]');
  if (cancelButton) cancelButton.onclick = closeWindow;
  if (successButton) successButton.onclick = onSubmit.bind(this, iframe);
  if (minimizeButton) minimizeButton.style.display = 'none';
};

const attachCompressedLogs = (
  fileInput: HTMLInputElement,
  {
    compressedLogsFileData,
    compressedLogsFileName,
    userConsentText
  },
  iframe: window,
) => {
  const dT = new DataTransfer();
  if (dT.items) {
    const file = new File([compressedLogsFileData], compressedLogsFileName);
    dT.items.add(file);
    fileInput.files = dT.files;

    const doc = iframe.contentDocument;
    const attachmentLabelSelector = '.src-component-attachment-AttachmentList-container label';
    const attachmentLabel = doc.querySelector(attachmentLabelSelector);
    if (!attachmentLabel) return;
    const { className } = doc.querySelector('[data-garden-id="checkboxes.hint"]') || {};
    const attachmentWarningDiv = document.createElement('div');
    attachmentWarningDiv.className = className;
    attachmentWarningDiv.innerText = userConsentText;
    attachmentLabel.parentNode.insertBefore(attachmentWarningDiv, attachmentLabel.nextSibling);
  }
};

const setSelectValue = async (iframe: window, select: HTMLElement, value: any) => {
  select.click();
  const options = await waitForExist(
    '[data-garden-id="select.item"]',
    {
      context: iframe.contentDocument,
      selectAll: true,
    }
  );
  let selectValueWasSet = false;
  options.forEach((option: HTMLElement) => {
    if (option.innerText === value) {
      selectValueWasSet = true;
      option.click();
    }
  });
  if (!selectValueWasSet) {
    select.click();
  }
  select.blur();
};

const fillForm = async (formInfo: ZendeskInfo) => {

  const iframe = await waitForExist('#webWidget');
  const { network, locale, os, release, version, buildNumber } = formInfo;
  const form = await waitForExist('form', { context: iframe.contentDocument });
  const selects = form.querySelectorAll('[data-garden-id="select.select_view"]');

  const values = {
    product: `Daedalus wallet - ${network}`,
    operatingSystem: getOSInfo(os, release),
    supportLanguage: localesFillForm[locale],
    productVersion: `Daedalus ${version}+Cardano ${buildNumber}`,
  };

  for (let i = 0; i < selects.length; i++) {
    const valuesKey: string = zenDeskFormSelects[i];
    const value = values[valuesKey];
    if (value) await setSelectValue(iframe, selects[i], value);
  }

  form.querySelector('[data-garden-id="textfields.input"]').focus();
};

const closeWindow = () => {
  window.close();
  window.top && window.top.close();
};

ipcRenderer.on(
  SUPPORT_WINDOW.ZENDESK_INFO,
  (event, zendeskInfo: ZendeskInfo) => {
    const { locale, themeVars } = zendeskInfo;
    window.zE(() => {
      if (locale !== 'en-US') {
        window.zE.setLocale(localesSetLanguage[locale]);
      }
      window.zE.activate();
    });
    window.zESettings = {
      webWidget: {
        color: {
          theme: themeVars['--theme-support-widget-header-color'],
        }
      }
    };
    fillForm(zendeskInfo);
  }
);

ipcRenderer.on(SUPPORT_WINDOW.CLOSE, () => closeWindow);

ipcRenderer.on(SUPPORT_WINDOW.LOGS_INFO, async (event, logsInfo: LogsInfo) => {
  if (logsWereAttached) return false;
  logsWereAttached = true;
  const iframe = await waitForExist('#webWidget');
  const fileInput = await waitForExist(
    '#dropzone-input',
    { context: iframe.contentDocument }
  );
  attachCompressedLogs(fileInput, logsInfo, iframe);
});

waitForExist('#webWidget')
  .then(addFormEventListeners)
  .catch(() => {});

