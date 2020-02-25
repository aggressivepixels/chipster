const { Asset } = require("parcel-bundler");
const { readFile } = require("fs");
const { promisify } = require("util");

class BinaryAsset extends Asset {
  constructor(name, options) {
    super(name, options);
    this.type = "js";
  }

  async generate() {
    const buf = await promisify(readFile)(this.name);

    return {
      js: "module.exports=[" + buf.join(",") + "]"
    };
  }
}

module.exports = BinaryAsset;
