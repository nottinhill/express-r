var express = require('express');
var router = express.Router();

/* GET home page. */
router.get('/', function(req, res, next) {

  var waitMinutes = 3;
  res.setTimeout(waitMinutes*60*1000, function () {
      res.send({"status":"Waited for " + waitMinutes + "minutes."});
  });

});

module.exports = router;
