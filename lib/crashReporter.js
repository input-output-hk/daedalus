import { crashReporter } from 'electron';

export const startCrashReporter = () => {
  crashReporter.start({
    productName: 'YourName',
    companyName: 'YourCompany',
    submitURL: 'https://your-domain.com/url-to-submit',
    uploadToServer: true
  })
};
