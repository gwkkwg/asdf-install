{include resources/header.md}
{set-property title "Introduction | ASDF-Install Tutorial"}

### Introduction

If you're reading this you're probably already convinced that Common Lisp is a very fine programming language. However, while the [ANSI standard][hyperspec] is huge and provides tons of functionality there are a couple of things (like, say, XML parsers, web servers, GUIs, regular expressions) that aren't included and must either be provided by your particular implementation or otherwise by a third-party library. 

Hitherto these libraries had to be installed manually, an often complex process. However, many library authors are now packaging their systems using the ASDF-INSTALL standard, allowing for automatic installation on any Lisp system that supports it.   
   
{anchor #asdf}

### What is ASDF?

In order to understand what ASDF-INSTALL does we first have to understand what ASDF is and why we need it. [ASDF][36] ("Another System Definition Facility"), written by [Daniel Barlow][37], is a library that automates the compilation and loading of "systems", i.e. Lisp programs which are usually composed of a couple of files which have to be compiled and loaded in a certain order. ASDF is similar to the Unix `make` program. ASDF works with the majority of CL implementations in use today. 

   [36]: http://www.cliki.net/asdf
   [37]: http://ww.telent.net/

A similar system which precedes ASDF is [MK:DEFSYSTEM][38]. You don't need it for ASDF-INSTALL but it won't hurt to have it available for libraries which aren't aware of ASDF. However, this document makes no effort to explain how MK:DEFSYSTEM is used. See Henrik Motakef's article "[Fight The System][39]." 

   [38]: http://www.cliki.net/mk-defsystem
   [39]: http://rpgoldman.real-time.com/lisp/fight-the-system.html

_Update:_ Marco Antoniotti has patched ASDF-INSTALL to make it work with MK:DEFSYSTEM as well. See the [section about MK:DEFSYSTEM][mk:defsystem] below.   
   
{anchor asdf-install}

### What is ASDF-INSTALL?

[ASDF-INSTALL][41], also written by Dan Barlow, is layered atop of ASDF and can automatically download Lisp libraries from the Internet and install them for you. It is also able to detect and [resolve dependencies][dependencies] on other libraries. (These libraries have to be prepared for ASDF-INSTALL by their author. See more [below][using-a-library].) 

   [41]: http://www.cliki.net/asdf-install

ASDF-INSTALL was originally written for the [SBCL][44] Common Lisp implementation. It has been recently ported to [CMUCL][45], [Allegro Common Lisp][46], [Xanalys LispWorks][47], and [CLISP][48] by [Edi Weitz][49]. [Marco Baringer][50] added support for [OpenMCL][51], [James Anderson][52] added support for [Macintosh Common Lisp][53] (MCL).  Douglas Crosher added support for the [Scieneer Common Lisp][56].

   [44]: http://sbcl.sf.net/
   [45]: http://www.cons.org/cmucl/
   [46]: http://www.franz.com/products/allegrocl/
   [47]: http://www.lispworks.com/
   [48]: http://clisp.sourceforge.net/
   [49]: http://weitz.de/
   [50]: http://www.cliki.net/Marco%20Baringer
   [51]: http://openmcl.clozure.com/
   [52]: http://setf.de/
   [53]: http://www.digitool.com/
   [56]: http://www.scieneer.com/scl/

It'd be nice if users of other Lisps (like [Corman Lisp][54], or [ECL][55], could provide patches to make ASDF-INSTALL available on more platforms. 

   [54]: http://www.cormanlisp.com/
   [55]: http://ecls.sourceforge.net/

The original ASDF-INSTALL is distributed with SBCL. The latest incarnation of the "portable" version is available from [Common-Lisp.net][57]. 

   [57]: http://common-lisp.net/project/asdf-install/
   [58]: http://cvs.sourceforge.net/viewcvs.py/cclan/asdf-install/

Note that the "portable" version can be considered a fork of the original (SBCL-only) version. Likewise, versions of ASDF-INSTALL distributed with other Lisp implementations like OpenMCL are most likely forks of the "portable" version. This document mostly describes how to use the "portable" version. If you're on a system like SBCL or OpenMCL that comes with its own version of ASDF-INSTALL you are advised to _ignore_ the rest of this tutorial and instead consult the documentation that was supplied by your vendor. (Parts of this tutorial might apply to these implementations as well but they're not guaranteed to be kept up to date.) 

This tutorial and the portable version of ASDF-INSTALL were originally started by [Edi Weitz][59] but they're now maintained by [Gary King][60].   
   
   [59]: http://weitz.de/
   [60]: http://www.cliki.net/Gary%20King

{include resources/footer.md}