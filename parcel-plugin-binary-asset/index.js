module.exports = function(bundler) {
  bundler.addAssetType("bin", require.resolve("./BinaryAsset"));
};
