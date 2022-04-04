async function managerWebpack(baseConfig) {
  baseConfig.resolve.extensions.push('.ts', '.tsx');
  baseConfig.module.rules.push({ test: /\.tsx?$/, use: 'babel-loader' });
  return baseConfig;
}

export default { managerWebpack };
