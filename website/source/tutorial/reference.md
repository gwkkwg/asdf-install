{include header.md}
{set-property title "Reference | ASDF-Install Tutorial"}

### Customizing ASDF-INSTALL

When ASDF-INSTALL is loaded it [`LOAD`][122]s the file `~/.asdf-install` if it's there. This file (which is obviously supposed to contain Lisp code) can be used to change the values of some [special variables][123] which control ASDF-INSTALL's behaviour. Their names are [exported][124] from the `ASDF-INSTALL` [package][125]. 

   [122]: http://www.lispworks.com/reference/HyperSpec/Body/f_load.htm
   [123]: http://www.lispworks.com/reference/HyperSpec/Body/26_glo_s.htm#special_variable
   [124]: http://www.lispworks.com/reference/HyperSpec/Body/26_glo_e.htm#exported
   [125]: http://www.lispworks.com/reference/HyperSpec/Body/11_.htm

#### Special variable `*GNU-TAR-PROGRAM*`

The path to the GNU `tar` program as a string - the default is `"tar"`. Changing this variable has no effect if Cygwin is used. 

#### Special variable `*PROXY*`

This variable is `NIL` by default but will be set to the value of the environment variable `$http_proxy` (if it's set) prior to loading `~/.asdf-install`. Set this to a non-`NIL` value if you need to go through an http proxy. 

#### Special variable `*PROXY-USER*`

#### Special variable `*PROXY-PASSWD*`

Use these variables if your [proxy][126] requires authentication. 

   [126]: #*proxy*

#### Special variable `*CCLAN-MIRROR*`

This variable is set to `"http://ftp.linux.org.uk/pub/lisp/cclan/"` before `~/.asdf-install` is loaded. A couple of ASDF-installable libraries are available via [CCLAN][127] and with the help of this variable you can choose another CCLAN mirror from the list at [http://ww.telent.net/cclan-choose-mirror][128]. 

   [127]: http://www.cliki.net/cclan
   [128]: http://ww.telent.net/cclan-choose-mirror

{anchor *verify-gpg-signatures*}
#### Special variable `*VERIFY-GPG-SIGNATURES*`

This variable is set to `T` initially which means that there'll be a [security check][129] for each library which is not installed from a local file. You can set it to `NIL` which means no checks at all or to `:UNKNOWN-LOCATIONS` which means that only URLs which are not in [`*SAFE-URL-PREFIXES*`][130] are checked. Every other value behaves like `T`. 

   [129]: #security
   [130]: #*safe-url-prefixes*

_Note:_ This customization option is currently not supported in the SBCL version of ASDF-INSTALL. 

#### Special variable `*SAFE-URL-PREFIXES*`

The value of this variable is `NIL` initially. It is supposed to be a list of strings which are "safe" URL prefixes, i.e. if a download URL begins with one of these strings there's no [security check][131]. The value of `*SAFE-URL-PREFIXES*` only matters if [`*VERIFY-GPG-SIGNATURES*`][132] is set to `:UNKNOWN-LOCATIONS`. 

   [131]: #security
   [132]: #*verify-gpg-signatures*

_Note:_ This customization option is currently not supported in the SBCL version of ASDF-INSTALL. 

#### Special variable `*LOCATIONS*`

The initial value of this variable (prior to loading `~/.asdf-install`) is 
    
    
    ((#p"/usr/local/asdf-install/site/"
      #p"/usr/local/asdf-install/site-systems/"
      "System-wide install")
     (#p"/home/edi/.asdf-install-dir/site/"
      #p"/home/edi/.asdf-install-dir/systems/"
      "Personal installation"))
    

where `/home/edi/` will obviously be replaced with your home directory. You'll notice that this corresponds to the [little menu][133] you see when ASDF-INSTALL starts to install a package. You can add elements to this list or replace it completely to get another menu. Each element is a list with three elements - a [pathname][134] denoting the directory where the (unpacked) libraries will be stored, a pathname denoting a directory where [system definition][135] symlinks will be placed, and a string describing this particular choice. 

   [133]: #where
   [134]: http://www.lispworks.com/reference/HyperSpec/Body/19_b.htm
   [135]: #definition

If you make changes to this value it is important that you also update [`ASDF:*CENTRAL-REGISTRY*`][136] accordingly in your initialization file or ASDF-INSTALL won't find your system definitions (unless you are on Windows). See the [example][137] below. 

   [136]: #*central-registry*
   [137]: #example

_Note:_ On SBCL the initial value of this variable is different - try it out yourself. 

#### Special variable `*PREFERRED-LOCATION*`

This variable is initially `NIL`. If it is not `NIL` it should be a positive integer not greater than the length of [`*LOCATIONS*`][138]. By setting this value you circumvent the [question][139] about where to install a library and ASDF-INSTALL will unconditionally use the corresponding entry from [`*LOCATIONS*`][138]. Note that `1` (not `0`) means the first entry. 

   [138]: #*locations*
   [139]: #where

_Note:_ This customization option is currently not supported in the SBCL version of ASDF-INSTALL. 

#### Environment variable `ASDF_INSTALL_DIR`

The value of this _environment variable_ determines the first element of the initial value of [`*LOCATIONS*`][140], i.e. if it, say, contains the value `/usr/local/foo/`, then the first element of `*LOCATIONS*` is 
    
   [140]: #*locations*


    (#p"/usr/local/foo/site/"
     #p"/usr/local/foo/site-systems/"
     "System-wide install")
    

If this variable is not set, the directory `/usr/local/asdf-install/` is used. Note that this variable affects ASDF-INSTALL's behaviour _before_ `~/.asdf-install` is loaded. 

_Note:_ On SBCL the value of `SBCL_HOME` is used instead. 

#### Environment variable `PRIVATE_ASDF_INSTALL_DIR`

The value of this _environment variable_ determines the second element of the initial value of [`*LOCATIONS*`][141], i.e. if it, say, contains the value `frob/` and your username is `johndoe`, then the second element of `*LOCATIONS*` is 
    
   [141]: #*locations*


    (#p"/home/johndoe/frob/site/"
     #p"/home/johndoe/frob/systems/"
     "Personal installation")
    

If this variable is not set, the value `.asdf-install-dir` (note the dot) is used. Note that this variable affects ASDF-INSTALL's behaviour _before_ `~/.asdf-install` is loaded. 

_Note:_ On SBCL the value `.sbcl` is used instead. 

#### An example `.asdf-install` file

Here's a documented example for how the file `~/.asdf-install` could look like: 
    
    
    ;; use a http proxy
    (setq asdf-install:[*proxy*][142] "http://proxy.foo.com/")
    
    ;; use a CCLAN mirror in France
    (setq asdf-install:[*cclan-mirror*][143] "http://thingamy.com/cclan/")
    
    ;; only partial security checks
    (setq asdf-install:[*verify-gpg-signatures*][144] :unknown-locations)
    
    ;; downloads from Kevin Rosenberg and from my own server don't have to be checked
    (setq asdf-install:[*safe-url-prefixes*][145]
            '("http://files.b9.com/" "http://weitz.de/files/"))
    
    ;; add a repository for unstable libraries
    (pushnew '(#p"/usr/local/lisp/unstable/site/"
               #p"/usr/local/lisp/unstable/systems/"
               "Install as unstable")
             asdf-install:[*locations*][146]
             :test #'equal)
    
    ;; make sure this is also known by ASDF
    (pushnew "/usr/local/lisp/unstable/systems/"
             asdf:[*central-registry*][147]
             :test #'equal)
    

   [142]: #*proxy*
   [143]: #*cclan-mirror*
   [144]: #*verify-gpg-signatures*
   [145]: #*safe-url-prefixes*
   [146]: #*locations*
   [147]: #*central-registry*

  
   


### The list of trusted code suppliers

ASDF-INSTALL maintains a list of library authors you trust. This list is stored in a file `trusted-uids.lisp` and usually resides in the directory `~/.asdf-install-dir/` but this can be customized by changing the environment variable [`PRIVATE_ASDF_INSTALL_DIR`][148]. You are not supposed to edit this file manually - new entries are added automatically whenever you choose the [corresponding restart][149] during the security check.   
   


   [148]: #private-asdf-install-dir
   [149]: #restart


{include footer.md}

