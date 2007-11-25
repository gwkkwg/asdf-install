{include resources/header.md}
{set-property title "Reference | ASDF-Install Tutorial"}

{anchor customizing-asdf-install}

### Customizing ASDF-INSTALL

When ASDF-INSTALL is loaded it [`LOAD`][hs-load]s the file `~/.asdf-install` if it's there. This file (which is obviously supposed to contain Lisp code) can be used to change the values of some [special variables][] which control ASDF-INSTALL's behaviour. Their names are [exported][hs-exported] from the `ASDF-INSTALL` [package][hs-package]. 

{anchor *gnu-tar-program*}

#### Special variable `*GNU-TAR-PROGRAM*`

The path to the GNU `tar` program as a string - the default is `"tar"`. Changing this variable has no effect if Cygwin is used. 

{anchor *proxy*}

#### Special variable `*PROXY*`

This variable is `NIL` by default but will be set to the value of the environment variable `$http_proxy` (if it's set) prior to loading `~/.asdf-install`. Set this to a non-`NIL` value if you need to go through an http proxy. 

{anchor *proxy-user*}

#### Special variable `*PROXY-USER*`

{anchor *proxy-passwd*}

#### Special variable `*PROXY-PASSWD*`

Use these variables if your [proxy][*proxy*] requires authentication. 

{anchor *cclan-mirror*}

#### Special variable `*CCLAN-MIRROR*`

This variable is set to `"http://ftp.linux.org.uk/pub/lisp/cclan/"` before `~/.asdf-install` is loaded. A couple of ASDF-installable libraries are available via [CCLAN][127] and with the help of this variable you can choose another CCLAN mirror from the list at [http://ww.telent.net/cclan-choose-mirror][128]. 

   [127]: http://www.cliki.net/cclan
   [128]: http://ww.telent.net/cclan-choose-mirror

{anchor *verify-gpg-signatures*}

#### Special variable `*VERIFY-GPG-SIGNATURES*`

This variable is set to `T` initially which means that there'll be a [security check][security] for each library which is not installed from a local file. You can set it to `NIL` which means no checks at all or to `:UNKNOWN-LOCATIONS` which means that only URLs which are not in [`*SAFE-URL-PREFIXES*`][*safe-url-prefixes*] are checked. Every other value behaves like `T`. 

_Note:_ This customization option is currently not supported in the SBCL version of ASDF-INSTALL. 

{anchor *safe-url-prefixes*}

#### Special variable `*SAFE-URL-PREFIXES*`

The value of this variable is `NIL` initially. It is supposed to be a list of strings which are "safe" URL prefixes, i.e. if a download URL begins with one of these strings there's no [security check][security]. The value of `*SAFE-URL-PREFIXES*` only matters if [`*VERIFY-GPG-SIGNATURES*`][*verify-gpg-signatures*] is set to `:UNKNOWN-LOCATIONS`. 


_Note:_ This customization option is currently not supported in the SBCL version of ASDF-INSTALL. 

{anchor *locations*}

#### Special variable `*LOCATIONS*`

The initial value of this variable (prior to loading `~/.asdf-install`) is 
    
    ((#p"/usr/local/asdf-install/site/"
      #p"/usr/local/asdf-install/site-systems/"
      "System-wide install")
     (#p"/home/edi/.asdf-install-dir/site/"
      #p"/home/edi/.asdf-install-dir/systems/"
      "Personal installation"))

where `/home/edi/` will obviously be replaced with your home directory. You'll notice that this corresponds to the [little menu][where] you see when ASDF-INSTALL starts to install a package. You can add elements to this list or replace it completely to get another menu. Each element is a list with three elements - a [pathname][hs-pathname] denoting the directory where the (unpacked) libraries will be stored, a pathname denoting a directory where [system definition][definitions] symlinks will be placed, and a string describing this particular choice. 

If you make changes to this value it is important that you also update [`ASDF:*CENTRAL-REGISTRY*`][*central-registry*] accordingly in your initialization file or ASDF-INSTALL won't find your system definitions (unless you are on Windows). See the [example][example] below. 

_Note:_ On SBCL the initial value of this variable is different - try it out yourself. 

{anchor *preferred-location*}

#### Special variable `*PREFERRED-LOCATION*`

This variable is initially `NIL`. If it is not `NIL` it should be a positive integer not greater than the length of [`*LOCATIONS*`][*locations*]. By setting this value you circumvent the [question][where] about where to install a library and ASDF-INSTALL will unconditionally use the corresponding entry from [`*LOCATIONS*`][*locations*]. Note that `1` (not `0`) means the first entry. 

_Note:_ This customization option is currently not supported in the SBCL version of ASDF-INSTALL. 

{anchor asdf-install-dir}

#### Environment variable `ASDF_INSTALL_DIR`

The value of this _environment variable_ determines the first element of the initial value of [`*LOCATIONS*`][*locations*], i.e. if it, say, contains the value `/usr/local/foo/`, then the first element of `*LOCATIONS*` is 
    

    (#p"/usr/local/foo/site/"
     #p"/usr/local/foo/site-systems/"
     "System-wide install")

If this variable is not set, the directory `/usr/local/asdf-install/` is used. Note that this variable affects ASDF-INSTALL's behaviour _before_ `~/.asdf-install` is loaded. 

_Note:_ On SBCL the value of `SBCL_HOME` is used instead. 

{anchor private-asdf-install-dir}

#### Environment variable `PRIVATE_ASDF_INSTALL_DIR`

The value of this _environment variable_ determines the second element of the initial value of [`*LOCATIONS*`][*locations*], i.e. if it, say, contains the value `frob/` and your username is `johndoe`, then the second element of `*LOCATIONS*` is 
    

    (#p"/home/johndoe/frob/site/"
     #p"/home/johndoe/frob/systems/"
     "Personal installation")

If this variable is not set, the value `.asdf-install-dir` (note the dot) is used. Note that this variable affects ASDF-INSTALL's behaviour _before_ `~/.asdf-install` is loaded. 

_Note:_ On SBCL the value `.sbcl` is used instead. 

{anchor example}

#### An example `.asdf-install` file

Here's a documented example for how the file `~/.asdf-install` could look like: 
    
    ;; use a http proxy
    (setq asdf-install:[*proxy*][*proxy*] "http://proxy.foo.com/")
    
    ;; use a CCLAN mirror in France
    (setq asdf-install:*cclan-mirror* "http://thingamy.com/cclan/")
    
    ;; only partial security checks
    (setq asdf-install:[*verify-gpg-signatures*][*verify-gpg-signatures*] :unknown-locations)
    
    ;; downloads from Kevin Rosenberg and from my own server don't have to be checked
    (setq asdf-install:*safe-url-prefixes*
            '("http://files.b9.com/" "http://weitz.de/files/"))
    
    ;; add a repository for unstable libraries
    (pushnew '(#p"/usr/local/lisp/unstable/site/"
               #p"/usr/local/lisp/unstable/systems/"
               "Install as unstable")
             asdf-install:*locations*
             :test #'equal)
    
    ;; make sure this is also known by ASDF
    (pushnew "/usr/local/lisp/unstable/systems/"
             asdf:*central-registry*
             :test #'equal)

{anchor trusted-uids}

### The list of trusted code suppliers

ASDF-INSTALL maintains a list of library authors you trust. This list is stored in a file `trusted-uids.lisp` and usually resides in the directory `~/.asdf-install-dir/` but this can be customized by changing the environment variable [`PRIVATE_ASDF_INSTALL_DIR`][private-asdf-install-dir]. You are not supposed to edit this file manually - new entries are added automatically whenever you choose the [corresponding restart][hs-restart] during the security check.   

{include resources/footer.md}

