# daily-tents

Solves puzzles from https://www.brainbashers.com/tents.asp

Open a daily tent puzzle, copy the javascript from the page source (something like this):

```
var lcpuzzletext = 'June 15 - Easy 8 x 8';
var lnclock = new Date();
var lnsize = 8;
var lcpuzzle = '0000021100000002012000002000000111202102210000001000212020211000';
var lcrownumbers = '11113122';
var lccolnumbers = '30302112';
var lncurrentcell = -1;
var laimg = new Array(lnsize * lnsize);
var lcclickallowed = "Y";
var lcanswershown = "N";
var lcdate = '0615';
var lcdiff = 'Easy';
var lcprint = "";
```

Enjoy :)