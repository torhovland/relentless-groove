var request = require('request-promise');

module.exports = function (options) {
  var config = options || {};

  function clear () {
    return request({
      method: "DELETE",
      uri: "http://localhost:2525/imposters"
    });
  }

  function create () {
    return request({
      method: "POST",
      uri: "http://localhost:2525/imposters",
      json: true,
      body: config
    });
  }

  return {
    clear,
    create
  };
};