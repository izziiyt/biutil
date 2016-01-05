#biutil (Scala-2.10/11)

[![Build Status](https://travis-ci.org/izziiyt/biutil.svg?branch=master)](https://travis-ci.org/izziiyt/biutil)

"self satisfied" Bioinformatics Utility Libraries for Scala  

###Packages

[Scala doc API](http://izziiyt.github.io/biutil/api)

####alignment

For managing bio-specific strings, such as DNA or Protein.

####biformat

For managing bio-specific format files, such as BED or WIG.  
These data are assumed as too big to put in memory.  
Because of that, Iterator is adopted.

####optimization

pending

###Build

If you want to use this library as it is, the easiest way is using sbt publishLocal.
```bash
git clone https://github.com/izziiyt/biutil.git
cd biutil
sbt + publishLocal
```

###LICENSE

Under the MIT License, see LICENSE.txt

###Author

Yuto Ichikawa  
