# express-r

Express Router to call R studio scripts. 

## Installation

### Optional

```
export PORT=1234
```

### Mandatory
```
npm install
npm start
```


## Usage 

```
curl -X POST http://localhost:8444/r 
--data   '{"foo":"bar","john":"doe"}'  
--header 'Content-Type: application/json'
```

_The MIT Licence - Copyright 2018 Stephan Kristyn_
