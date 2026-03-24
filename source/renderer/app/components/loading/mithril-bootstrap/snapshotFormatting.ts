import { formattedBytesToSize } from '../../../utils/formatters';

const DIGEST_PREVIEW_LENGTH = 8;

export const truncateDigest = (digest: string) => {
  if (digest.length <= DIGEST_PREVIEW_LENGTH * 2) return digest;

  return `${digest.slice(0, DIGEST_PREVIEW_LENGTH)}...${digest.slice(
    -DIGEST_PREVIEW_LENGTH
  )}`;
};

export const formatSnapshotDate = (
  value: string | undefined,
  locale: string
) => {
  if (!value) return null;

  const parsedDate = new Date(value);
  const parsed = parsedDate.getTime();
  if (Number.isNaN(parsed)) return value;

  return parsedDate.toLocaleString(locale, {
    year: 'numeric',
    month: 'short',
    day: '2-digit',
    hour: '2-digit',
    minute: '2-digit',
  });
};

export const formatSnapshotSize = (size?: number) => {
  if (typeof size !== 'number' || size <= 0) return null;

  return formattedBytesToSize(size);
};

export const formatTransferSize = (size?: number | null) => {
  if (typeof size !== 'number' || size < 0) return null;

  if (size === 0) return '0 Bytes';

  return formattedBytesToSize(size);
};
