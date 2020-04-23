const { pki } = require('node-forge')
const fs = require('fs')

module.exports = function createAndWriteX509Cert(folder) {
  const keys = pki.rsa.generateKeyPair(2048)
  const cert = pki.createCertificate()
  cert.publicKey = keys.publicKey

  cert.serialNumber = '01'
  cert.validity.notBefore = new Date()
  cert.validity.notAfter.setFullYear(cert.validity.notBefore.getFullYear() + 1);

  const attrs = [{
    name: 'commonName',
    value: 'daedalus'
  }, {
    name: 'countryName',
    value: 'HK'
  }, {
    shortName: 'ST',
    value: 'NA'
  }, {
    name: 'localityName',
    value: 'NA'
  }, {
    name: 'organizationName',
    value: 'IOHK'
  }, {
    shortName: 'OU',
    value: 'NA'
  }]

  cert.setSubject(attrs)
  cert.setIssuer(attrs)
  cert.setExtensions([{
    name: 'basicConstraints',
    cA: true
  }, {
    name: 'keyUsage',
    keyCertSign: true,
    digitalSignature: true,
    nonRepudiation: true,
    keyEncipherment: true,
    dataEncipherment: true
  }, {
    name: 'extKeyUsage',
    serverAuth: true,
    clientAuth: true,
    codeSigning: true,
    emailProtection: true,
    timeStamping: true
  }, {
    name: 'nsCertType',
    client: true,
    server: true,
    email: true,
    objsign: true,
    sslCA: true,
    emailCA: true,
    objCA: true
  }, {
    name: 'subjectAltName',
    altNames: [{
      type: 7, // IP
      ip: '127.0.0.1'
    }]
  }])

  // self-sign certificate
  cert.sign(keys.privateKey)

  // convert a Forge certificate to PEM
  const crt = pki.certificateToPem(cert)
  const key = pki.privateKeyToPem(keys.privateKey)
  const pem = `${key}${crt}`

  fs.writeFileSync(`${folder}/ca.crt`, crt)
  fs.writeFileSync(`${folder}/client.key`, key)
  fs.writeFileSync(`${folder}/client.pem`, pem)
}