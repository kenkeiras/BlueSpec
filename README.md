BlueSpec
========

Parse the [Common Lisp HyperSpec(TM)](http://www.lispworks.com/documentation/HyperSpec/Front/index.htm)
and output it's representation on [reStructuredText](http://docutils.sourceforge.net/rst.html) ready to
be passed to [Sphinx](http://sphinx-doc.org/).

Dependencies
------------

* Common Lisp interpreter
* [Quicklisp](http://www.quicklisp.org/)
* [Common Lisp HyperSpec(TM) files](http://www.lispworks.com/documentation/common-lisp.html)

Usage
-----

* Unzip the [HyperSpec(TM)](http://www.lispworks.com/documentation/common-lisp.html) files
on the same directory as the code, so directly inside the `HyperSpec` directory
are the ones named `Body` `Data` `Front` `Graphics` `Issues`.
* Create a directory named `sphinx` and generate a Sphinx documentation template,
for example

&nbsp;

    cd sphinx
    sphinx-quickstart # This will ask for some data, just remember to keep the suffix as .rst and the index file name

* Return to the code and run `main.lisp` (this may require to set the
[Quicklisp](http://www.quicklisp.org/) path, it's on the first line of the code.

  - On sbcl

    `sbcl --script main.lisp`


  - On clisp

    `clisp main.lisp`


* After this go back to the `sphinx` directory and run `make html`.
