var express = require('express');
var router = express.Router();

/* GET home page. */
router.get('/', function(req, res, next) {

  var waitSeconds = 1;
  res.setTimeout(waitSeconds*1000, function () {
      res.send({"status":"Waited for " + waitSeconds + "seconds."});
  });

});

module.exports = router;
