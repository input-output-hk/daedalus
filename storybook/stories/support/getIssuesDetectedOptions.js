const issues = [
  { id: 227, category: 'Daedalus', subCategory: 'Installation', title: 'Your computer time is out of sync.' },
  { id: 228, category: 'Daedalus', subCategory: 'Connection', title: 'Local block data is corrupted.' },
  { id: 229, category: 'Daedalus', subCategory: 'Operation', title: 'Launching node without admin rights.' },
  { id: 230, category: 'Daedalus', subCategory: 'Installation', title: 'File(s) missing.' },
  { id: 231, category: 'Daedalus', subCategory: 'Installation', title: 'Not enough space to store block data.' },
  { id: 234, category: 'Daedalus', subCategory: 'Operation', title: 'Network error.' },
  { id: 235, category: 'Daedalus', subCategory: 'Installation', title: 'User name contains non-Latin characters.' },
  { id: 236, category: 'Daedalus', subCategory: 'Operation', title: '‘open.lock’ file has been corrupted.' },
  { id: 237, category: 'Daedalus', subCategory: 'Operation', title: 'Firewall is blocking connection' },
];

export default (issuesFound: number): Array<any> => issues.slice(0, issuesFound);
