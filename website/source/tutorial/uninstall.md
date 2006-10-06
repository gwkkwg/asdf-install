{include header.md}
{set-property title "Uninstall | ASDF-Install Tutorial"}

### How to uninstall a library

This is easy: 
    
    
    (asdf-install:uninstall [:library-name][150])
    

   [150]: #library-name

ASDF-INSTALL will ask you to confirm this and then it'll remove the library's source directory as well as the symbolic link to the [system definition][151] (if it exists). 

   [151]: #definition

_Windows note:_ Due to [the way systems are found][152] on Windows ASDF-INSTALL will propose to delete an arbitrary version of your library if you've installed several of them. Make sure to read what it is about to remove before you confirm.   
   
   [152]: #custom-search

{include footer.md}

