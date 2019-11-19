exports.setSize = function(pct) {
  return function() {
    document.getElementById("bar").style.width = pct + '%';
  };
};

var fs = require('fs');

exports.readPasswd = function (onError, onSuccess) {
    var cancel = fs.readFile('/etc/passwd', {encoding: 'utf8'}, function (err, res) {
        if (err) {
            onError(err);
        } else {
            onSuccess(res);
        }
    });
    return function (cancelError, onCancelerError, onCancelerSuccess) {
        cancel();
        onCancelerSuccess();
    };
};
