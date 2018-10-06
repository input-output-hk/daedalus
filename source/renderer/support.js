// @flow
import { ipcRenderer } from 'electron';
import { SUPPORT_WINDOW } from '../common/ipc-api';
import updateCSSVariables from './app/utils/updateCSSVariables';
import waitForExist from './app/utils/waitForExist';

declare class File {
  data: [],
  name: string,
}

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

  // Hides the loading overlay
  const removeOverlay = () => {
    if (document.body) {
      document.body.classList.add('hideOverlay');
    }
  };

  const formHandler = (iframe: HTMLElement) => {
    // Closes the support window when cancelling the form
    const form = iframe.contentDocument.forms[0];
    const cancelButton = form.querySelector('button');
    cancelButton.onclick = closeWindow;
  };

  const attachCompressedLogs = (
    fileInput: HTMLElement,
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
    removeOverlay();
  };

  const closeWindow = () => {
    window.close();
    window.top && window.top.close();
  };

  setTimeout(removeOverlay, SECONDS_TO_REMOVE_OVERLAY * 1000);

  ipcRenderer.on(
    SUPPORT_WINDOW.ZENDESK_INFO,
    (event, { locale, themeVars }: ZendeskInfo) => {
      updateCSSVariables(themeVars);
      window.zE(() => {
        // window.zE.hide();
        if (locale !== 'en-US') window.zE.setLocale(locales[locale]);
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

  ipcRenderer.on(SUPPORT_WINDOW.LOGS_INFO, (event, logsInfo: LogsInfo) =>
    waitForExist('#webWidget')
      .then((iframe) =>
        window.Promise.all([iframe, waitForExist('#dropzone-input', { doc: iframe })])
      )
      .then((results) => attachCompressedLogs(results[1], logsInfo))
      .catch(() => {})
  );

  waitForExist('#webWidget')
    .then(formHandler)
    .catch(() => {});

};

support();
