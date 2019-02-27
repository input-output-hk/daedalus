// @flow
import { PDFExtract } from 'pdf.js-extract';
import fs from 'fs';

import {
  decryptRegularVend,
  decryptForceVend,
  decryptRecoveryRegularVend,
  decryptRecoveryForceVend,
} from '../../common/crypto/decrypt';
import type {
  ParseRedemptionCodeRequest,
  ParseRedemptionCodeResponse
} from '../../common/ipc/api';
import { MainIpcChannel } from './lib/MainIpcChannel';
import { PARSE_REDEMPTION_CODE_CHANNEL } from '../../common/ipc/api';
import { Logger } from '../utils/logging';
import { ERRORS } from '../../common/ipc/constants';

// HELPERS

const cleanupTemporaryPDF = (pdfPath, isTemporaryDecryptedPdf) => {
  // Remove the temporary, decrypted PDF from disk
  if (pdfPath && isTemporaryDecryptedPdf) {
    try { fs.unlinkSync(pdfPath); } catch (e) {} // eslint-disable-line
  }
};

// CHANNEL
const parseRedemptionCodeChannel: (
  MainIpcChannel<ParseRedemptionCodeRequest, ParseRedemptionCodeResponse>
) = new MainIpcChannel(PARSE_REDEMPTION_CODE_CHANNEL);

// REQUEST HANDLER
const parseRedemptionCodeHandler = async (request: ParseRedemptionCodeRequest) => {
  Logger.debug('parseRedemptionCodeHandler', request);
  const { certificateFilePath, decryptionKey, redemptionType } = request;
  let pdfPath = null;
  let isTemporaryDecryptedPdf = false;
  // If pass phrase is given assume that it's an encrypted certificate
  if (decryptionKey) {
    try {
      // Decrypt the file
      const encryptedFile = fs.readFileSync(certificateFilePath);
      let decryptedFile;
      switch (redemptionType) {
        case 'forceVended':
          decryptedFile = decryptForceVend(decryptionKey, encryptedFile);
          break;
        case 'recoveryRegular':
          decryptedFile = decryptRecoveryRegularVend(decryptionKey, encryptedFile);
          break;
        case 'recoveryForceVended':
          decryptedFile = decryptRecoveryForceVend(decryptionKey, encryptedFile);
          break;
        default: // regular
          decryptedFile = decryptRegularVend(decryptionKey, encryptedFile);
      }
      // Write it to disk temporarily (so pdf extract can work with it)
      pdfPath = `${certificateFilePath}.pdf`;
      fs.writeFileSync(pdfPath, decryptedFile);
      isTemporaryDecryptedPdf = true;
    } catch (error) {
      Logger.error('Error while parsing redemption code', { error });
      return Promise.reject(error.message);
    }
  } else {
    pdfPath = certificateFilePath;
  }
  // Extract redemption code from certificate PDF
  try {
    const pdfExtract = new PDFExtract();
    return new Promise((resolve, reject) => {
      pdfExtract.extract(pdfPath, {}, (error, data) => {
        if (error) return reject(error);
        try {
          const redemptionKeyLabel = data.pages[0].content[9].str;
          if (
            redemptionKeyLabel !== 'REDEMPTION KEY'
            && redemptionKeyLabel !== '—————— REDEMPTION KEY ——————'
          ) {
            Logger.error('Incalid redemption certificate', request);
            reject(ERRORS.INVALID_REDEMPTION_CERTIFICATE_ERROR);
          }
          cleanupTemporaryPDF(pdfPath, isTemporaryDecryptedPdf);
          resolve(data.pages[0].content[8].str);
        } catch (exception) {
          reject(exception.message);
        }
      });
    });
  } catch (error) {
    cleanupTemporaryPDF(pdfPath, isTemporaryDecryptedPdf);
    return Promise.reject(error.message);
  }
};

// SETUP
export const setupParseRedemptionCodeHandler = () => {
  parseRedemptionCodeChannel.onRequest(parseRedemptionCodeHandler);
};
