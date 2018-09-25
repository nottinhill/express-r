var express = require('express');
var router = express.Router();
const { execFile } = require('child_process');


/* POST rscript listing. */
router.post('/', function(req, res, next) {

    const reqBody = req.body;

    const reqScript        = req.body.script;
    const reqEnv           = req.body.env;

    console.log("[express] scriptName: "  + reqScript);
    console.log("[express] env: "         + reqEnv);

    const path = "Rscript";
    const scriptPath = "./routes/";
    // const path = "C:\\Program Files\\R\\R-3.5.1\\bin\\x64\\Rscript.exe"; // win64
    // const scriptPath = "C:\\Users\\holden\\Documents\\devel\\express-r\\routes\\";

    const child=execFile(path, [scriptPath + reqScript + '.R', '--vanilla', reqEnv], (error, stdout, stderr) => {

      if (stderr) {
          console.log(`[express] stderr: ${stderr}`);
      }

      if (error) {
         console.error(`[express] exec error: ${error}`);
         res.status(500).send(stdout);
      } else {
          console.log(`[express] stdout: ${stdout}`);
          res.status(200).send(stdout);
      }
  });
});

module.exports = router;

