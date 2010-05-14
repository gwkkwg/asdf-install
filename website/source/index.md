{include "resources/links.md"}
{set-property html yes}
{set-property style-sheet style}
{set-property title "ASDF-Install"}

<div class="header">

## ASDF-Install

#### Bringing Lisp Libraries to you

</div>
<div class="contents">
<div class="system-links">
  * [Mailing Lists][1]
  * [Getting it][2]
  * [Changelog][changelog]
  * [System Tests][tr]
</div>
<div class="system-description">

### What it is

ASDF-Install is the start of Common Lisp's answer to CPAN: an
internet-based, dependency-chasing, installation system.
ASDF-Install was originally written for SBCL by Dan Barlow
and then ported to other Lisps by Edi Weitz and many others.
It knows how to work with systems defined by either ASDF or
MK-Defsystem.

ASDF-Install is a tool for downloading and installing lisp
packages. It:

  * downloads lisp libraries (defined using either
    [ASDF][download-asdf] or [MK-Defsystem][5]) and installs
    them in the local file system,

  * chases sub-system dependencies, and 

  * uses PGP signatures to verify the provenance of the
    downloaded code (requires [GPG][6])

The best sources of information on ASDF-Install are

  * The [tutorial][9] (written by [Edi Weitz][8] and now
    maintained by [Gary King][10])

  * its [CLiki page][cliki]

<a id="mailing-lists"></a>

### Mailing Lists

  * [asdf-install-devel][mailing-list]: A list for questions, patches,
    bug reports, and so on; It's for everything.

<a href="downloads"></a>

### Where is it

The current asdf-install repository is on
[github][github-asdf-install] and you can clone it using:

    git clone git://github.com/gwkkwg/asdf-install

Note that Some Lisps (e.g., SBCL (since 0.8.3) and OpenMCL (since
0.14.1) come with a version of ASDF-Install.

You can switch over to the portable version using the git
version above, this [gzipped tar file][14] or using
ASDF-Install itself (though you will probably need to edit
your local configuration files in order to use this version.

    
<a id="news"></a>

### What is happening

<table class="system-news">
<tr>
  <th>
      13 May 2010
  </th>
  <td>

Picking up old threads. switched to git. incorporate Faré
Rideau's ASDF2 patch

  </td>
</tr>
<tr>
  <th>
      13 June 2008
  </th>
  <td>

Thanks to Scott Burson for pointing out that
ASDF-Install has a README file (I had forgotten!)
and that it was out of date. This, and the
`load-asdf-install.lisp` file it mentions are now
working correctly again.

  </td>
</tr>
<tr>
  <th>
      2 December 2007
  </th>
  <td>
      Dan Muller, Andy Cristina and Attila Lendvai greatly improve ASDF-Install's windows support. Thanks! These changes are in the *unstable* branch (see above)
  </td>
</tr>
<tr>
  <th>
    7 October 2006
  </th>
  <td>
    Updated to version 0.6.0; attempts to improve GPG handling by simplifying it. Added :where argument to the install command. Altered location handling so that 0 is always cancel. Began to rework tutorial.
  </td>
</tr>
<tr>
  <th>
    17 May 2006
  </th>
  <td>
    Moved from Source Forge to Common-Lisp.net
  </td>
</tr>
</table>


</div>
{include footer.md}
</div>

   [1]: #mailing-lists
   [2]: #downloads
   [5]: http://www.cliki.net/mk-defsystem
   [6]: http://www.gnupg.org/ (GPG Link)
   [8]: http://weitz.de/ (Edi Weitz)
   [9]: tutorial/index.html (ASDF-Install Tutorial)
   [10]: http://www.metabang.com/about-gwking.html
   [11]: http://common-lisp.net/cgi-bin/mailman/listinfo/asdf-install-devel
   [13]: downloads
   [14]: http://common-lisp.net/project/asdf-install/asdf-install_latest.tar.gz
   [15]: http://common-lisp.net/project/cl-containers/shared/buttons/xhtml.gif (valid xhtml button)
   [16]: http://validator.w3.org/check/referer (xhtml1.1)
   [17]: http://common-lisp.net/project/cl-containers/shared/buttons/hacker.png (hacker emblem)
   [18]: http://www.catb.org/hacker-emblem/ (hacker)
   [19]: http://common-lisp.net/project/cl-containers/shared/buttons/lml2-powered.png (lml2 powered)
   [20]: http://lml2.b9.com/ (lml2 powered)
   [21]: http://common-lisp.net/project/cl-containers/shared/buttons/lambda-lisp.png (ALU emblem)
   [22]: http://www.lisp.org/ (Association of Lisp Users)
   [23]: http://common-lisp.net/project/cl-containers/shared/buttons/lisp-lizard.png (Common-Lisp.net)
   [24]: http://common-lisp.net/ (Common-Lisp.net)
   [tr]: test-report.html
