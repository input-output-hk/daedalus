// @flow
import fs from 'fs';

export const logUsedVersion = async (version: string, logFilePath: string) => {
  let usedVersions = null;
  try {
    // Load existing file
    const rawContent = await fs.promises.readFile(logFilePath, 'utf8');
    usedVersions = JSON.parse(rawContent);
    // Add current version if it has not yet been saved
    if (!usedVersions.includes(version)) {
      usedVersions.push(version);
    }
  } catch (error) {
    // The file doesn't exist
    if (error.code === 'ENOENT') {
      // Start with this version
      usedVersions = [version];
    }
  }
  if (usedVersions) {
    await fs.promises.writeFile(logFilePath, JSON.stringify(usedVersions));
  }
};
