These projects use the "new" OpenTools API (available in Delphi 4 and newer).


To compile you may need to add the following entry: 
 - - - - - - - - - - - - - - - 
   $(DELPHI)\source\toolsapi
 - - - - - - - - - - - - - - - 
to the IDE library path (depends on IDE version).


If you encounter this error:
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
   [DCC Error] ToolsAPI.pas(20): F1026 File not found: 'DockForm.dcu'
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
then the "designide" package must be included. This can be accomplished by 
going to Component > Install Packages, and adding ";designide" to the 
end of the section titled "Runtime packages" and ensuring that "Build 
with runtime packages" is selected.


All the add-in functionality is in the source file "tsvnWizard.pas". 
This file references a conditional constant 'DLL_MODE' that enables to compile the 
add-in as BPL or as DLL at your own choice.


"TortoiseSVNaddinBPL.dpk" is a Delphi 5 package. Just build and install it.
(You may need to update it or create a new package for your particular IDE version).


"TortoiseSVNaddinDLL.dpr" is a DLL project. 
To install it you need to copy the generated DLL to any directory of your choice
and create the following registry key: 

  HKEY_CURRENT_USER\Software\Borland\Delphi\5.0\Experts

and add a string value (any name is fine) containing the path of the DLL.

If it's the case, replace "Delphi" with "C++Builder" and/or replace the version number.


For more information on the Open Tools API please check http://www.gexperts.org/opentools/ .

