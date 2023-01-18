import fs from 'fs';

export const logUsedVersion = async (version: string, logFilePath: string) => {
  let usedVersions = null;
  const currentVersionData = {
    version,
    // e.g. "0.13.2"
    date: new Date().toISOString(), // e.g. "2018-12-11T144501.0177"
  };

  try {
    // Load existing file
    const rawContent = await fs.promises.readFile(logFilePath, 'utf8');
    usedVersions = JSON.parse(rawContent);
    const versionsData = usedVersions && usedVersions.versions;
    const isAlreadyLogged = versionsData.some(
      (item) => item.version === version
    );

    // Add current version if it has not yet been saved
    if (!isAlreadyLogged) {
      versionsData.push(currentVersionData);
    }
  } catch (error) {
    // The file doesn't exist
    if (error.code === 'ENOENT') {
      // Start with this version
      usedVersions = {
        versions: [currentVersionData],
      };
    }
  }

  if (usedVersions) {
    await fs.promises.writeFile(logFilePath, JSON.stringify(usedVersions));
  }
};
