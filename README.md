#biutil

[![Build Status](https://travis-ci.org/izziiyt/biutil.svg?branch=master)](https://travis-ci.org/izziiyt/biutil)

"self satisfied" Bioinformatics Utility Libraries for Scala  

###Packages

[Scaladoc 2.11](http://izziiyt.github.io/scaladoc/biutil/2.11)

[Scaladoc 2.10](http://izziiyt.github.io/scaladoc/biutil/2.10)

####alignment

For managing bio-specific strings, such as DNA or Protein.

####biformat

For managing bio-specific format files, BED, WIG and MAF.  
These data are assumed as too big to put in memory.  
Because of that, Iterator extended Class is adopted.

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
