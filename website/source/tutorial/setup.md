{include resources/header.md}
{set-property title "Prerequisites | ASDF-Install Tutorial"}

### Prerequisites

This tutorial is aimed at Unix-like systems which include Linux and Mac OS X. If you're on MS Windows make sure to read the _Windows notes_ at the end of each section. 

Apart from one of the [supported Lisps][asdf-install] you will need [GnuPG][62] (which is  pre-installed on most Linux distributions but not on OS X). Install it first if you don't have it already. You may also need to install [the GNU version of `tar`][63] if you're not on Linux. 

   [62]: http://www.gnupg.org/
   [63]: http://www.gnu.org/software/tar/tar.html

(GnuPG is not strictly necessary - see [below][*verify-gpg-signatures*] - but it is recommended
if you want to be somewhat more sure that you're not installing arbitrary malicious code.) 

_Update:_ Beginning with version 0.14.1 ASDF-INSTALL is already included with the OpenMCL distribution. Also, AllegroCL 7.0 and higher include ASDF (but not ASDF-INSTALL.) See below for details. 

_Note:_ ASDF-Install will not work with MCL unless you start MCL 
from a terminal. 

_Windows note:_ If you want to use ASDF-INSTALL on Windows you must install [Cygwin][65] first. You can also install GnuPG from the Cygwin setup program. If you want to use CLISP you currently [have to use][66] the Cygwin version (which can also be installed from the setup application). The good news is that if you use Cygwin you can pretty much pretend you're on Unix and **skip** all the _Windows notes_ below. 

   [65]: http://www.cygwin.com/
   [66]: http://article.gmane.org/gmane.lisp.clisp.general/7891

(Update: Alex Mizrahi posted [some notes][67] about using the native Win32 version of CLISP to [comp.lang.lisp][68]. I asked him to send patches but he hasn't sent them yet.) 

   [67]: http://www.google.com/groups?selm=2gacj0Fi7moU1%40uni-berlin.de&output=gplain
   [68]: news://comp.lang.lisp

Whenever I use `~/` (the Unix shell notation for the user's home directory) in the following text what is actually meant is the value of ([`USER-HOMEDIR-PATHNAME`][user-homedir-pathname]). While on Unix/Linux all implementations seem to agree what this value should be, on Windows this is not the case. Read the docs of your Lisp. 

{anchor install-asdf}

#### Installing ASDF

([Skip][load-asdf] this section if you use SBCL or OpenMCL or AllegroCL 7.0 or higher.) [Download ASDF][71] and put the file `asdf.lisp` in a place where you want it to stay. Change into this directory and, from your Lisp, issue the command 
    
   [71]: http://www.cliki.net/asdf


    (load (compile-file "asdf.lisp"))

You should now have a new file the name of which depends on your implementation - probably something like `asdf.fasl`, `asdf.fas`, `asdf.fsl`, `asdf.ufsl`, `asdf.x86f`, or `asdf.so`. 

_Note:_ LispWorks 4.2 (and probably earlier versions) has a bug that prevents it from loading the compiled ASDF correctly. It is recommended that you upgrade to 4.3 but if for some reason you must use an older version you can skip the compilation step above and later just load the `.lisp` file instead in which case you'll use interpreted code. 

{anchor load-asdf}

#### Loading ASDF automatically

We want to make sure that ASDF is loaded whenever we start our Lisp. For this we'll use an _initialization file_. Most Lisps will read and execute the contents of a certain file on startup. This file is usually located in your home directory and might be called `.clinit.cl` (for Allegro Common Lisp), `.cmucl-init` (for CMUCL), `.lispworks` (for Xanalys LispWorks), `.clisprc` (for CLISP), `openmcl-init.lisp` (for OpenMCL), or `.scl-init` (for the Scieneer CL).  Consult your Lisp's documentation for details. 

Open this file (create it if it doesn't exist) and add this line 
    
    #-:asdf (load "/path/where/asdf/is/located/asdf")

where of course you have replaced `/path/where/asdf/is/located/` with the correct path to ASDF - see [last section][install-asdf]. We wrote `(load ".../asdf")` and not, say, `(load ".../asdf.x86f")` because this way your Lisp will load the compiled file if it is available and otherwise `asdf.lisp` if for some reason you didn't compile the code. 


Why the `#-:asdf`? After ASDF has been loaded it adds the symbol `:ASDF` to the [features list][*features*]. Our use of the _read-time conditional_  [Sharpsign Minus][sharpsign-minus] thus makes sure that ASDF isn't loaded a second time if it's already there. (So you can safely save and use an image with ASDF pre-loaded without changing your init file.) 

If you're using SBCL or OpenMCL or AllegroCL 7.0 or higher _don't_ add the line from above but use 
    
    (require :asdf)

instead. 

ASDF maintains a list of places where it will look for
{anchor definitions} _system definitions_ when it is asked to
load or compile a system. (System definitions are the files
ending with `.asd`.) This list is stored in the {anchor
*central-registry*} [special variable][]
`ASDF:*CENTRAL-REGISTRY*` and you can add new directories to
it. Open your initialization file once again and add the
following line _after_ the line which loads ASDF:

    (pushnew "/path/to/your/registry/" asdf:*central-registry* :test #'equal)


You can use a directory of your choice but you should make sure it exists. You can also add several of these lines with different directories so ASDF will look into each directory in turn until it has found a system definition. Use the directory `~/.asdf-install-dir/systems/` if you can't make a decision and make sure to create it. (Replace `~/` with an absolute path to your home directory because not all Lisps support the tilde notation.) We will call the directory you've chosen your _registry_ from now on. 

_Note:_ It is important that you add a _directory_ here, not a file, so make sure the [namestring][hs-namestring] ends with a slash! 

_Note:_ If you use ASDF alone the preferred way to deal with system definitions is to create symbolic links from the `.asd` files to your registry. However, you don't have to deal with this as ASDF-INSTALL will do that for you. 

_Note:_ The free "Personal Edition" of LispWorks doesn't read `~/.lispworks` on startup. You can circumvent this by putting something like 

    alias lispworks="/usr/local/lib/LispWorksPersonal/lispworks-personal-4300 -init ~/.lispworks"

into your `~/.bashrc` file. 

_Windows note:_ On Windows we can't use a central registry because Windows doesn't have symbolic links. We will use another mechanism (see [below][load-asdf-install]) to find system definitions, so you don't have to put the `PUSHNEW` line into your initialization file. 

{anchor install-asdf-install}

#### Installing ASDF-INSTALL

([Skip][load-asdf-install] this section if you use SBCL.) [Download ASDF-INSTALL][79] and put the `.lisp` and `.asd` file into a new directory `asdf-install` which can be located wherever you like. Now create a symlink to your `.asd` file from your [registry][80] folder: 
    
   [79]: http://cvs.sourceforge.net/viewcvs.py/cclan/asdf-install/

    cd /path/to/your/registry/
    ln -s /path/where/you/put/asdf-install/asdf-install.asd .    

For OpenMCL you don't have to download ASDF-INSTALL because it's already there - it's in `/path/to/ccl/tools/asdf-install/` where `/path/to/ccl/` is the directory where you installed OpenMCL. You have to provide the symlink, though. 

Now start your Lisp and issue the following command: 
    
    (asdf:operate 'asdf:compile-op :asdf-install)
    (asdf:operate 'asdf:load-op :asdf-install)

This will ask ASDF to locate the ASDF-INSTALL library, compile it, and finally load it. 

_Windows note:_ You can leave out the `ln` command. Now, _before_ you compile and load ASDF-INSTALL you have to put this line into your initialization file: 
    
    
    (pushnew "/path/where/you/unpacked/asdf-install/" asdf:*central-registry* :test #'equal)
    

and then either restart your Lisp or evaluate this expression in your current session. Afterwards, proceed with the two `ASDF:OPERATE` forms. 

{anchor load-asdf-install}

#### Loading ASDF-INSTALL automatically

Open your [initilization file][load-asdf] again and add this line at the end: 

    #-:asdf-install (asdf:operate 'asdf:load-op :asdf-install)    

This will instruct ASDF to load the (compiled) ASDF-INSTALL library whenever your Lisp starts up (unless ASDF-INSTALL is already available in your image). 

If you're using SBCL _don't_ add the line from above but use 
    
    (require :asdf-install)

instead. (Note: Try this from the REPL and check the messages to see whether SBCL really loads its own bundled version of ASDF-INSTALL. The "portable" version this document talks about is supposed to work with SBCL as well but in case of incompatibilities you're advised to rely on SBCL's version.) 

You're now ready to use ASDF-INSTALL. 

{anchor win-sym}
_Windows note:_ For Windows add the following line to end of the initialization file: 

    (pushnew 'asdf-install:sysdef-source-dir-search
              asdf:*system-definition-search-functions*)

As we [can't use][win-sym] the [central registry][*central-registry*], we're using a {anchor custom-search}customized search function instead. It'll scan all directories below each of the entries in [`*LOCATIONS*`][*locations*] until it finds a suitable system definition. Note that this is a sub-optimal solution because this will not necessarily find the newest one if you've installed several versions of the same library. Make sure to [uninstall][uninstall] older versions.   

{anchor mk:defsystem}

### Optional: Using MK:DEFSYSTEM instead of (or in addition to) ASDF

[MK:DEFSYSTEM][86] was written by Mark Kantrovitz in the early days of Common Lisp. It precedes ASDF and also works with almost all CL implementations you'll come across. Thanks to the efforts of Marco Antoniotti, ASDF-INSTALL can now also be used with MK:DEFSYSTEM which means that even if the library you want to use doesn't have an ASDF system definition you might be able to install it via ASDF-INSTALL. 

   [86]: http://www.cliki.net/mk-defsystem

The recommended setup is to use _both_ ASDF _and_ MK:DEFSYSTEM because this will significantly increase the number of libraries you can install with ASDF-INSTALL. 

To set up your Lisp environment for this you have to do the following (after reading the sections above): 

  * Get MK:DEFSYSTEM (version 3.4i or higher) from [CLOCC][87]. (You can grab a nightly snapshot or browse the CVS. You only need the file `defsystem.lisp` from within the `src/defsystem-3.x` directory.) 
  * To install MK:DEFSYSTEM evaluate the form 
    
   [87]: http://clocc.sourceforge.net/

    (load (compile-file "/path/to/defsystem.lisp"))

  * To load MK:DEFSYSTEM automatically each time you start your Lisp put the forms 

        #-:mk-defsystem (load "/path/to/defsystem")
        (mk:add-registry-location "/path/to/your/registry/")

    into your initialization file. 

  * Finally, replace the line 

        #-:asdf-install (asdf:operate 'asdf:load-op :asdf-install)

    from [above][load-asdf-install] with the line 

        #-:asdf-install (load "/path/to/asdf-install/load-asdf-install")    

This last step will ensure that ASDF-INSTALL will always be loaded on startup even if you only use MK:DEFSYSTEM and don't have ASDF available. 

The following sections should work for you no matter whether you use ASDF, MK:DEFSYSTEM, or both.   

{include resources/footer.md}