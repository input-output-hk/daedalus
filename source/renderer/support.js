// @flow
import { ipcRenderer } from 'electron';
import { SUPPORT_WINDOW } from '../common/ipc-api';
import updateCSSVariables from './app/utils/updateCSSVariables';
import waitForExist from './app/utils/waitForExist';

const support = () => {

  type ZendeskInfo = {
    locale: string,
    themeVars: {
      '--theme-support-widget-header-color': string
    },
    network: string,
    locale: string,
    version: string,
    buildNumber: string,
  };

  type LogsInfo = {
    compressedLogsFileData: any,
    compressedLogsFileName: string,
  };

  const SECONDS_TO_REMOVE_OVERLAY = 10;
  let logsWereAttached = false;

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

  const hideLoadingOverlay = () => {
    if (document.body) {
      document.body.classList.add('hideOverlay');
    }
  };

  const onSubmit = async (iframe) => {
    const doneButton:any = waitForExist(
      '.src-component-submitTicket-SubmitTicket-button',
      { context: iframe.contentDocument }
    );
    doneButton.onclick = closeWindow;
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
    if (typeof value === 'function') {
      value(options);
    } else {
      options.forEach((option: HTMLElement) => {
        if (option.innerText === value) {
          option.click();
        }
      });
    }
    select.blur();
  };

  const addFormEventListeners = async (iframe: window) => {
    const form = await waitForExist('form', { context: iframe.contentDocument });
    const [cancelButton, successButton] = form.querySelectorAll('footer button');
    if (cancelButton) cancelButton.onclick = closeWindow;
    if (successButton) successButton.onclick = onSubmit.bind(this, iframe);
  };

  const attachCompressedLogs = (
    fileInput: HTMLInputElement,
    {
      compressedLogsFileData,
      compressedLogsFileName,
    },
  ) => {
    const dT = new DataTransfer();
    if (dT.items) {
      const file = new File([compressedLogsFileData], compressedLogsFileName);
      dT.items.add(file);
      fileInput.files = dT.files;
    }
    hideLoadingOverlay();
  };

  const fillForm = async (formInfo: ZendeskInfo) => {
    const iframe = await waitForExist('#webWidget');
    const { network, locale } = formInfo;
    let { version, buildNumber } = formInfo;
    const form = await waitForExist('form', { context: iframe.contentDocument });
    const selects = form.querySelectorAll('[data-garden-id="select.select_view"]');

    // TODO: Find a better way to handle non-existent versions
    if (buildNumber === 'dev') buildNumber = '1.3.0';
    if (version === '0.12.0') version = '0.11.0';

    const values = {
      product: `Daedalus wallet - ${network}`,
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

  setTimeout(hideLoadingOverlay, SECONDS_TO_REMOVE_OVERLAY * 1000);

  ipcRenderer.on(
    SUPPORT_WINDOW.ZENDESK_INFO,
    (event, zendeskInfo: ZendeskInfo) => {
      const { locale, themeVars } = zendeskInfo;
      updateCSSVariables(themeVars);
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
    attachCompressedLogs(fileInput, logsInfo);
  });

  waitForExist('#webWidget')
    .then(addFormEventListeners)
    .catch(() => {});

};

support();
