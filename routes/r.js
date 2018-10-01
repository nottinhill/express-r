var express = require('express');
var router = express.Router();
const { execFile } = require('child_process');


/* POST rscript listing. */
router.post('/', function(req, res, next) {

    if (!req.body.script || !req.body.env) {
        console.error("[express] FATAL ERROR: No Script or Environment Specified!")
    }

    console.log("[express] scriptName: "  + req.body.script);
    console.log("[express] env: "         + req.body.env);

    // Linux
    const path = "Rscript";
    const scriptPath = "/home/saveup-pas/";

    // Win
    // const path = "C:\\Program Files\\R\\R-3.5.1\\bin\\x64\\Rscript.exe"; // win64
    // const scriptPath = "C:\\Users\\UserName\\Documents\\devel\\express-r\\routes\\";

    const child=execFile(path, [scriptPath + req.body.script + '.R', '--vanilla', JSON.stringify(req.body)], (error, stdout, stderr) => {

      if (stderr) {
          console.log(`[express] stderr: ${stderr}`);
      }

      if (error) {
         console.error(`[express] ERROR: ${error}`);
         res.status(500).send(stdout);
      } else {
          console.log(`${stdout}`);
          res.status(200).send(stdout);
      }
  });
});

module.exports = router;

