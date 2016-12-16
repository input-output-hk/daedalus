import crypto from 'crypto';

export default data => {
  const hash = crypto.createHash('sha256');
  hash.update(data, 'utf8', 'base64');
  return hash.digest('hex');
};
