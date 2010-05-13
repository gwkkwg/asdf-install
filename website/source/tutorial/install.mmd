{include resources/header.md}
{set-property title "Installing a Library | ASDF-Install Tutorial"}

<div class="content">

{anchor how-to-install}

### How to install a library

Here and in the following sections we assume that you have
set up your environment as described in
[_Prerequisites_][section-prerequisites].

   [section-prerequisites]: setup.html

_Note:_ Of course, the fact that a library can be installed
with ASDF-INSTALL and that ASDF-INSTALL was ported to your
Lisp implementation doesn't necessary mean that the library
_itself_ will work with your Lisp! Check the library's docs
before you try to install it.

{anchor install-by-name}

#### Installing a library by name

The webpage [http://www.cliki.net/asdf-install][90] contains
a list of libraries which can automatically be downloaded and
installed via ASDF-INSTALL. Listed here are libraries which
are explicitely prepared to work with ASDF-INSTALL and where
the author decided to announce this via [CLiki][91]. This is
the preferred way to install a library via ASDF-INSTALL.

   [90]: http://www.cliki.net/asdf-install
   [91]: http://www.cliki.net/

You can click on the name of each library to get a
description. Use the library's name from the list to install
it. If, say, you want to install [CL-PPCRE][92] make sure
you're connected to the Internet and use this command:

 [92]: http://weitz.de/cl-ppcre/

    (asdf-install:install :cl-ppcre)

Then proceed with [_Where to store the library_][where] below. 

_Note:_ If you install a library by name, ASDF-INSTALL will
connect to the CLiki website and from there it'll be
redirected to the actual download location provided by the
library's author.

_Note:_ The argument to the `ASDF-INSTALL:INSTALL` function
is a [string designator][hs-string-designator], i.e. instead of `:CL-PPCRE` you
can also use `"cl-ppcre"`. CLiki is case-insensitive and
therefore case doesn't matter if you install a library by
name.

{anchor install-by-url}

#### Installing a library by URL

The list mentioned [above][install-by-name] is not necessary
complete, i.e. there might as well exist libraries which
aren't listed there but which can be installed via
ASDF-INSTALL.

In order to be _ASDF-installable_ a library has to contain a
[system definition][definitions] for ASDF. It also has to be
packaged in a certain way: It is assumed to come as a gzipped
tar archive (usually ending in `.tar.gz` or `.tgz`) which
unpacks into one directory possibly containing
sub-directories. The system definition has to have a name
corresponding to the name of the library (so if your library
is called "foobar" the system definition is supposed to be
`foobar.asd`) and has to reside in the top-level directory.

If this is the case you can download and install the library
directly by providing the download URL of the package like
so:

    (asdf-install:install "http://weitz.de/files/cl-ppcre.tar.gz")

Now proceed with [_Where to store the library_][where] below. 

_Note:_ Currently, ASDF-INSTALL only understands http. Other
protocols like ftp or https aren't supported.

_Note:_ It's obviously rather easy to make an existing
library ASDF-installable if it isn't already. If you come
across a library which you'd like to use but which isn't
listed on [http://www.cliki.net/asdf-install][98], it might
be worthwhile to kindly ask the library's author to change
this.

   [98]: http://www.cliki.net/asdf-install

{anchor installing-from-local-file}

#### Installing from a local file

The third way to install a library via ASDF-INSTALL is to use
a local tar archive (in the format described [in the last
section][install-by-url]). In this case you use the file's
[namestring][hs-namestring]

    (asdf-install:install "/path/to/library/library.tar.gz")

and afterwards carry on with the next section. 

_Note:_ For obvious reasons this namestring must not start
with `"http://"` although your operating system might
otherwise allow this.

{anchor where}

#### Where to store the library

ASDF-INSTALL will now ask you where the library should be
stored. (This can be [customized][*locations*].) In the
default configuration this'll look more or less like so:

    Install where?
    1) System-wide install:
       System in /usr/local/asdf-install/site-systems/
       Files in /usr/local/asdf-install/site/
    2) Personal installation: 
       System in /Users/gwking/.asdf-install-dir/systems/
       Files in /Users/gwking/.asdf-install-dir/site/ 
    0) Abort installation.     
    -->

Choose one of these options and enter the corresponding
number, then press the `Return` key. (Note that on Unix-like
systems you usually don't have write access in `/usr/local/`
unless you're `root`.) Choice 0 will always be assigned to
canceling the installation.

{anchor security}

#### The security check

If you don't install from a local file, ASDF-INSTALL will now
check the validity of the library. (This behaviour can be
[customized][*verify-gpg-signatures*].) Library authors are
supposed to crypto-sign their libraries and provide a file
with the (PGP) signature in the same place where the library
can be downloaded, i.e. if the library is at
`http://www.example.com/frob.tar.gz` then ASDF-INSTALL will
try to download the signature from
`http://www.example.com/frob.tar.gz.asc`.

ASDF-INSTALL will check 

  * if the signature exists on your computer, 
  * if there is a GPG trust relationship between the package signer and you (i.e. that the package comes from someone whose key you've signed, or someone else you have GPG trust with has signed), and 
  * if the signer is listed in your [personal list of valid suppliers of Lisp code][trusted-uids]. 

If all these tests succeed, ASDF-INSTALL will compile and
install the library and you can now [use
it][using-a-library]. (This will also happen instantly if you
have installed from a local file.)

If one of the checks fails, you'll most likely be confronted
with one of these situations:

    Downloading 157777 bytes from http://weitz.de/files//cl-ppcre.tgz ...
    Error: Server responded 404 for GET http://weitz.de/files//cl-ppcre.tgz.asc
      [condition type: DOWNLOAD-ERROR]
    
    Restart actions (select using :continue):
     0: Don't ckeck GPG signature for this package
     1: Return to Top Level (an "abort" restart).
     2: Abort entirely from this process.

There was no signature corresponding to this package. 
    
    Downloading 6365 bytes from http://files.b9.com//cl-base64-latest.tar.gz ...gpg: WARNING: using insecure memory!
    gpg: please see http://www.gnupg.org/faq.html for more information
    gpg: Signature made Thu 12 Jun 2003 04:06:04 PM CEST using DSA key ID C4A3823E
    gpg: Can't check signature: public key not found
    
    Error: No key found for key id 0x112ECDF2C4A3823E.  Try some command like
      gpg  --recv-keys 0x112ECDF2C4A3823E
      [condition type: KEY-NOT-FOUND]
    
    Restart actions (select using :continue):
     0: Don't ckeck GPG signature for this package
     1: Return to Top Level (an "abort" restart).
     2: Abort entirely from this process.

The library was signed but the signer's public key wasn't
found in your public keyring.

    Downloading 6365 bytes from http://files.b9.com//cl-base64-latest.tar.gz ...gpg: WARNING: using insecure memory!
    gpg: please see http://www.gnupg.org/faq.html for more information
    gpg: Signature made Thu 12 Jun 2003 04:06:04 PM CEST using DSA key ID C4A3823E
    gpg: Good signature from "Kevin M. Rosenberg <kmr@debian.org>"
    gpg:                 aka "Kevin Rosenberg <kevin@rosenberg.net>"
    gpg:                 aka "Kevin M. Rosenberg <kevin@b9.com>"
    gpg:                 aka "Kevin Marcus Rosenberg, M.D. <kevin@b9.com>"
    gpg: WARNING: This key is not certified with a trusted signature!
    gpg:          There is no indication that the signature belongs to the owner.
    Primary key fingerprint: D7A0 55B6 4768 3582 B10D  3F0C 112E CDF2 C4A3 823E
    
    Error: GPG warns that the key id 0x112ECDF2C4A3823E (Kevin M. Rosenberg <kmr@debian.org>) is not fully trusted
      [condition type: KEY-NOT-TRUSTED]
    
    Restart actions (select using :continue):
     0: Don't ckeck GPG signature for this package
     1: Return to Top Level (an "abort" restart).
     2: Abort entirely from this process.

The signer's key is in your public keyring but you have no GPG trust relationship with him. 

    Downloading 157777 bytes from http://weitz.de/files//cl-ppcre.tgz ...gpg: WARNING: using insecure memory!
    gpg: please see http://www.gnupg.org/faq.html for more information
    gpg: Signature made Fri 24 Oct 2003 11:22:11 AM CEST using DSA key ID 057958C6
    gpg: Good signature from "Dr. Edmund Weitz <edi@weitz.de>"
    
    Error: Dr. Edmund Weitz <edi@weitz.de> (key id 595FF045057958C6) is not on your package supplier list
      [condition type: AUTHOR-NOT-TRUSTED]
    
    Restart actions (select using :continue):
     0: Add to package supplier list
     1: Don't ckeck GPG signature for this package
     2: Return to Top Level (an "abort" restart).
     3: Abort entirely from this process.

The signer's key is in your public keyring, you have a GPG
trust relationship with him but the signer wasn't found in
your [list of valid suppliers of Lisp code][trusted-uids].

As you'll have noticed, in all these cases ASDF-INSTALL
offers the [restart][hs-restart] not to check the GPG signature in
this particular case. How you can select this restart depends
on your Lisp implementation but if you select it ASDF-INSTALL
will proceed compiling and installing the package without
further checks for this library.

In the last case (condition type `AUTHOR-NOT-TRUSTED`) you
are also offered another restart. If you select this one the
signer of the library will be added to your [package supplier
list][trusted-uids] and you won't be asked again if you
install another library signed by the same person.

_Note:_ You might be asking yourself if all this security
stuff is really necessary. Well, [CLiki][108], the website
where ASDF-INSTALL looks for the package URL if you install
by name, can be edited by _anyone_ so it would be fairly easy
for a malicious hacker to redirect you to a library which
once it's installed insults your boss by email or withdraws
US$ 100,000 from your bank account. You better make sure this
doesn't happen... See the [section about
customization][customizing-asdf-install] on how to (partly)
disable security checks.

   [108]: http://www.cliki.net/

_Note:_ If you're unsure about notions like _public keyring_
or _GPG trust relationship_, please read the [GnuPG
documentation][110]. It is beyond the scope of this text to
explain these terms.

 [110]: http://www.gnupg.org/documentation/index.html

{anchor using-a-library}

### How to use an installed library

After you've successfully executed `ASDF-INSTALL:INSTALL` you can immediately use the library you've just installed while you're still in the same Lisp session. If you quit your Lisp image and start it anew you have to reload the library. (Of course you _don't_ have to install it again!) This is done like so: 
    
    (asdf:operate 'asdf:load-op :library-name)

Here `:LIBRARY-NAME` is either the name you've used if you installed [by name][install-by-name] or it is the name of the main `.asd` file if you've installed [by URL][install-by-url] or [from a local file][installing-from-local-file]. If you're not sure about the name you have to use, you can list the contents of your [registry][] for all libraries which are available to you. So, if your registry looks like this 
    
    edi@bird:~ > ls ~/.asdf-install-dir/systems/
    cl-ppcre.asd  cl-ppcre-test.asd  cl-who.asd  html-template.asd

you can substitute `:LIBRARY-NAME` with one of `:CL-PPCRE`, `:CL-PPCRE-TEST`, `:CL-WHO`, or `:HTML-TEMPLATE`. (CL-PPCRE-TEST was most likely automatically installed when you installed [CL-PPCRE][115].) 

   [115]: http://weitz.de/cl-ppcre/

If you use SBCL you can, instead of calling `ASDF:OPERATE`, simply [`REQUIRE`][hs-require] the library: 

    (require :library-name)

{anchor dependencies}

### How ASDF-INSTALL resolves dependencies

Sometimes a library depends on one or more other libraries. This can be expressed within an ASDF [system definition][definitions]. If there's a dependency and the necessary libraries aren't already installed then ASDF-INSTALL will try to download the missing libraries [by name][install-by-name] and install them before it proceeds to install the main library. This of course requires that the missing libraries are also listed on [CLiki][119]. 

   [119]: http://www.cliki.net/asdf-install

You can for example from CMUCL issue the command 
    
    (asdf-install:install :osicat)
    
and watch how ASDF-INSTALL not only downloads and installs [Osicat][120] but also [UFFI][121].   
   


   [120]: http://common-lisp.net/project/osicat/
   [121]: http://uffi.b9.com/

</div>

{include resources/footer.md}


[dependencies]: #dependencies

