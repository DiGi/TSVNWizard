unit UHelperFunctions;

interface

uses ToolsAPI, Classes;

function GetCurrentProject: IOTAProject;

procedure GetModuleFiles( FileList: TStrings; Module: IOTAModule);

function GetFilesForCmd(Project: IOTAProject; FileName: string): string;

implementation

uses SysUtils;

function GetCurrentProject: IOTAProject;
var
  ModServices: IOTAModuleServices;
  Module: IOTAModule;
  Project: IOTAProject;
  ProjectGroup: IOTAProjectGroup;
  i: Integer;
begin
  Result := nil;

  ModServices := BorlandIDEServices as IOTAModuleServices;
  if ModServices <> nil then
  begin
    for i := 0 to ModServices.ModuleCount - 1 do
    begin
      Module := ModServices.Modules[i];

      if Supports(Module, IOTAProjectGroup, ProjectGroup) then
      begin
        Result := ProjectGroup.ActiveProject;
        Exit;
      end
      else if Supports(Module, IOTAProject, Project) then
      begin // In the case of unbound packages, return the 1st
        if (Result = nil) then
          Result := Project;
      end;
    end;
  end;
end;

procedure GetModuleFiles( FileList: TStrings; Module: IOTAModule);
var
  FileEditor: IOTAEditor;
  I: integer;
begin
  if (Module <> nil) then
  begin
    for i:= 0 to Module.GetModuleFileCount-1 do
    begin
      try
        FileEditor:= Module.GetModuleFileEditor(i);
        if FileEditor <> nil then
        begin
          FileList.Add( FileEditor.GetFileName );
        end;
      except
        // hack for C++ Builder 5 OTA known issue: if the unit does not
        // have an associated form, calling GetModuleFileEditor(1) throws
        // access violation; calling GetModuleFileEditor(2) gets the .H file
        // (GetModuleFileCount returns 2, though)
        if (i = 1) and (Module.GetModuleFileCount = 2) then
        try
            FileEditor:= Module.GetModuleFileEditor(2);
            if FileEditor <> nil then
              FileList.Add( FileEditor.GetFileName );
        except
        end;
      end;
    end;
  end;
end;

function GetFilesForCmd(Project: IOTAProject; FileName: string): string;
var
  ItemList: TStringList;
  I: Integer;
  ModInfo: IOTAModuleInfo;
begin
  Result := '';

  ItemList := TStringList.Create;
  try
    ModInfo := Project.FindModuleInfo(FileName);
    if (ModInfo <> nil) then
    begin
      GetModuleFiles(ItemList, ModInfo.OpenModule);

      for I := 0 to ItemList.Count - 1 do
      begin
        Result := Result + ItemList[I];
        if (I < ItemList.Count - 1) then
          Result := Result + '*';
      end;
    end;
  finally
    ItemList.Free;
  end;
end;



end.
