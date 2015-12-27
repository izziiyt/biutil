#biutil

"self satisfied" Bioinformatics Utility Libraries for Scala

###Packages

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
git clone https://github.com/izziiyt/biutil
cd biutil
sbt publishLocal
```

###LICENSE

Under the MIT License, see [LICENSE.txt](https://github.com/izziiyt/biutil/LICENSE.txt).

###Others
[sbt](http://www.scala-sbt.org/) is a build tool for scala.  
Installing takes you very long time because of resolving dependency.  
These depended libraries are cached in your $HOME/.sbt or $HOME/.ivy2 and used next time.  
'pack' and 'packInstall' are sbt-plugin developed in https://github.com/xerial/sbt-pack
   
###Developer's Information & Contact
   Yuto Ichikawa  
