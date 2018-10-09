// @flow
import { ipcRenderer } from 'electron';
import { SUPPORT_WINDOW } from '../common/ipc-api';
import updateCSSVariables from './app/utils/updateCSSVariables';
import waitForExist from './app/utils/waitForExist';

const SECONDS_TO_REMOVE_OVERLAY = 10;

const support = () => {

  type ZendeskInfo = {
    locale: string,
    themeVars: {
      '--theme-support-widget-header-color': string
    }
  };

  type LogsInfo = {
    compressedLogsFileData: any,
    compressedLogsFileName: string,
    environment: any,
  };

  const locales = {
    'en-US': 'en-US',
    'ja-JP': 'ja',
  };

  const hideLoadingOverlay = () => {
    if (document.body) {
      document.body.classList.add('hideOverlay');
    }
  };

  const onSubmit = async (iframe) => {
    const doneButton = waitForExist(
      '.src-component-submitTicket-SubmitTicket-button',
      { context: iframe.contentDocument }
    );
    doneButton.onclick = closeWindow;
  };

  const setSelectValue = async (iframe, select) => {
    select.click();
    const options = await waitForExist(
      '[data-garden-id="select.item"]',
      {
        context: iframe.contentDocument,
        selectAll: true,
      }
    );
    // TODO: Click the correct option
    options[0].click();
    select.blur();
  };

  const formHandler = async (iframe: window) => {
    const form = await waitForExist('form', { context: iframe.contentDocument });
    const [cancelButton, successButton] = form.querySelectorAll('footer button');
    if (cancelButton) cancelButton.onclick = closeWindow;
    if (successButton) successButton.onclick = onSubmit.bind(this, iframe);
    const selects = form.querySelectorAll('[data-garden-id="select.select_view"]');
    for (const select of selects) {
      await setSelectValue(iframe, select);
    }
    form.querySelector('[data-garden-id="textfields.input"]').focus();
  };

  const attachCompressedLogs = (
    fileInput: HTMLInputElement,
    {
      compressedLogsFileData,
      compressedLogsFileName
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

  const closeWindow = () => {
    window.close();
    window.top && window.top.close();
  };

  setTimeout(hideLoadingOverlay, SECONDS_TO_REMOVE_OVERLAY * 1000);

  ipcRenderer.on(
    SUPPORT_WINDOW.ZENDESK_INFO,
    (event, { locale, themeVars }: ZendeskInfo) => {
      updateCSSVariables(themeVars);
      window.zE(() => {
        if (locale !== 'en-US') {
          window.zE.setLocale(locales[locale]);
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
    }
  );

  ipcRenderer.on(SUPPORT_WINDOW.CLOSE, () => closeWindow);

  ipcRenderer.on(SUPPORT_WINDOW.LOGS_INFO, async (event, logsInfo: LogsInfo) => {
    const iframe = await waitForExist('#webWidget');
    const fileInput = await waitForExist(
      '#dropzone-input',
      { context: iframe.contentDocument }
    );
    attachCompressedLogs(fileInput, logsInfo);
  });

  waitForExist('#webWidget')
    .then(formHandler)
    .catch(() => {});

};

support();
