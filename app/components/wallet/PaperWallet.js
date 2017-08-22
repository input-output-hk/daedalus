// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import * as pdfMake from 'pdfmake/build/pdfmake';
import * as pdfFonts from 'pdfmake/build/vfs_fonts';
import BorderedBox from '../widgets/BorderedBox';
import paperWalletPage1 from '../../assets/pdf/paper-wallet-page-1.png';
import paperWalletPage2 from '../../assets/pdf/paper-wallet-page-2.png';
import styles from './PaperWallet.scss';

// Assign pdfFonts to pdfMaker
Object.assign(pdfMake, pdfFonts.pdfMake);
Object.assign(pdfMake.default, pdfFonts.default.pdfMake);

@observer
export default class PaperWallet extends Component {

  props: {
    walletName: string,
    walletAddress: string,
  };

  state = {
    isGeneratingPDF: false,
  };

  docDefinition = {
    content: [
      // 1st page - Public key
      { // Page background
        image: paperWalletPage1,
        absolutePosition: { x: 0, y: 0 },
        fit: [595.28, 841.89], // A4 page size,
      },
      { // Wallet name
        text: this.props.walletName,
        style: 'walletNameStyle',
      },
      { // Wallet key type
        text: 'Public key',
        style: 'walletKeyTypeStyle',
      },
      { // Public key QR code
        qr: this.props.walletAddress,
        alignment: 'center',
        background: '#f8fbfd',
        fit: 80,
        foreground: '#3b5c9b',
      },
      { // Public key value
        text: this.props.walletAddress,
        style: 'walletAddressStyle',
      },
      // 2nd page - Private key
      { // Page background
        image: paperWalletPage2,
        absolutePosition: { x: 0, y: 0 },
        fit: [595.28, 841.89], // A4 page size
        pageBreak: 'before',
      },
      { // Wallet name
        text: this.props.walletName,
        style: 'walletNameStyle',
      },
      { // Wallet key type
        text: 'Private key',
        style: 'walletKeyTypeStyle',
      },
      { // Private key QR code
        qr: 'abcdefghijklmnopqrstuvwxyz1234567890',
        alignment: 'center',
        background: '#f8fbfd',
        fit: 80,
        foreground: '#3b5c9b',
      },
      { // Private key value
        text: 'abcdefghijklmnopqrstuvwxyz1234567890',
        style: 'walletAddressStyle',
      },
    ],
    pageMargins: [0, 0],
    pageSize: 'A4',
    styles: {
      walletNameStyle: {
        alignment: 'center',
        bold: true,
        color: '#3b5c9b',
        fontSize: 9,
        margin: [0, 310, 0, 0],
      },
      walletKeyTypeStyle: {
        alignment: 'center',
        color: '#3b5c9b',
        fontSize: 7,
        margin: 5,
      },
      walletAddressStyle: {
        alignment: 'center',
        color: '#3b5c9b',
        fontSize: 8,
        margin: [5, 10],
      },
    },
  };

  downloadPaperWallet = () => {
    this.setState({ isGeneratingPDF: true });
    setTimeout(() => { // Timeout is used to allow enought time for button text re-rendering
      pdfMake.createPdf(this.docDefinition).download('paper-wallet.pdf', () => {
        this.setState({ isGeneratingPDF: false });
      });
    }, 100);
  };

  render() {
    const {
      walletName, walletAddress,
    } = this.props;
    const { isGeneratingPDF } = this.state;

    return (
      <div className={styles.component}>

        <BorderedBox>

          <h1>Paper Wallet</h1>
          <p>Wallet name: {walletName}</p>
          <p>Private key: abcdefghijklmnopqrstuvwxyz1234567890</p>
          <p>Public key: {walletAddress}</p>
          <button
            onClick={this.downloadPaperWallet}
            disabled={isGeneratingPDF}
          >
            {isGeneratingPDF ? 'Generating Paper Wallet PDF' : 'Download Paper Wallet'}
          </button>

        </BorderedBox>

      </div>
    );
  }

}
