(*
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)

unit UHelperFunctions;

interface

uses
  ToolsAPI
, Classes
;

function GetCurrentProject: IOTAProject;

procedure GetModuleFiles( FileList: TStrings; Module: IOTAModule);

function GetFilesForCmd(Project: IOTAProject; FileName: string): string;

function GetString(const Index: Integer) : string;

function GetDirectoriesFromTSVN(Project: IOTAProject): TStrings;

procedure SetDirectoriesToTSVN(Project: IOTAProject; Directories: TStrings);

implementation

uses
  SysUtils
, Windows
, IniFiles
, IOUtils
;

function GetDirectoriesFromTSVN(Project: IOTAProject): TStrings;
var
  ProjPath, ProjName: string;
  IniFile: TIniFile;
  IniPath: TStringList;
  I: Integer;
  RelPath, AbsPath: string;
  OldDir: string;
begin
  {
    Check for project-specific settings in a <ProjectName>.tsvn file.
    If this file exists, then load all values in the [AdditionalDirs] section
    and add the existing directories to the project path.

    This way one can configure additional directories which should be updated
    when the project is updated but are not part of the project itself like
    configuration or graphical files.
  }
  Result := TStringList.Create;
  try
    ProjPath := ExtractFilePath(Project.FileName);
    ProjName := ExtractFileName(Project.FileName);
    ProjName := Copy(ProjName, 1, Pos(ExtractFileExt(ProjName), ProjName) - 1);

    if (FileExists(ProjPath + ProjName + '.tsvn')) then
    begin
      // Load settings
      IniFile := TIniFile.Create(ProjPath + ProjName + '.tsvn');
      try
        IniPath := TStringList.Create;
        try
          IniFile.ReadSectionValues('AdditionalDirs', IniPath);
          for I := 0 to IniPath.Count - 1 do
          begin
            {
              Could be a relative path so check that first and get the full name.
              Otherwise DirectoryExists() will fail for a relative path.
            }
            RelPath := IniPath.ValueFromIndex[I];
            AbsPath := RelPath;
            if (TPath.IsRelativePath(RelPath)) then
            begin
              {
                Change the current working directory, because GetFullPath()
                uses this to get the full path.
                Change it back after the conversion to not break stuff.
              }
              OldDir := GetCurrentDir;
              try
                SetCurrentDir(ProjPath);
                AbsPath := TPath.GetFullPath(RelPath);
              finally
                SetCurrentDir(OldDir);
              end;
            end;

            if (DirectoryExists(AbsPath)) then
              Result.Add(RelPath);
          end;
        finally
          IniPath.Free;
        end;
      finally
        IniFile.Free;
      end;
    end;
  except
    on E:Exception do
    begin
      // ShowMessage(E.ClassName + ': ' + E.Message);
    end;
  end;
end;

procedure SetDirectoriesToTSVN(Project: IOTAProject; Directories: TStrings);
var
  ProjPath, ProjName: string;
  IniFile: TIniFile;
  I: Integer;
begin
  if (Directories = nil) then
    Exit;

  try
    ProjPath := ExtractFilePath(Project.FileName);
    ProjName := ExtractFileName(Project.FileName);
    ProjName := Copy(ProjName, 1, Pos(ExtractFileExt(ProjName), ProjName) - 1);

    // if there are no directories and the file exists remove it and don't leave
    // the empty file on disk
    if (Directories.Count = 0) and
       (FileExists(ProjPath + ProjName + '.tsvn')) then
    begin
      DeleteFile(PChar(ProjPath + ProjName + '.tsvn'));
      Exit;
    end;

    IniFile := TIniFile.Create(ProjPath + ProjName + '.tsvn');
    try
      IniFile.EraseSection('AdditionalDirs');
      for I := 0 to Directories.Count - 1 do
      begin
        IniFile.WriteString('AdditionalDirs', IntToStr(I), Directories.Strings[I]);
      end;
    finally
      IniFile.Free;
    end;
  except

  end;
end;


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

function GetString(const Index: Integer) : string;
var
  Buffer : array[0..255] of Char;
  ls : Integer;
begin
  Result := '';
  ls := LoadString(HInstance,
                   Index,
                   Buffer,
                   SizeOf(Buffer));
  if (ls <> 0) then
    Result := Buffer;
end;


function GetFilesForCmd(Project: IOTAProject; FileName: string): string;
var
  ItemList: TStringList;
  I: Integer;
  {$if CompilerVersion < 21} // pre Delphi2010
  ModInfo: IOTAModuleInfo;
  {$ifend}
begin
  Result := '';

  ItemList := TStringList.Create;
  try
    {
      Since Delphi2010 there is a dedicated method to get the associated files
      from a file (throw in a .pas and you get the .pas and the .dfm).
    }
    {$if CompilerVersion >= 21} // Delphi2010+
    Project.GetAssociatedFiles(FileName, ItemList);
    {$else}
    ModInfo := Project.FindModuleInfo(FileName);

    if (ModInfo <> nil) then
      GetModuleFiles(ItemList, ModInfo.OpenModule);
    {$ifend}

      for I := 0 to ItemList.Count - 1 do
      begin
        Result := Result + ItemList[I];
        if (I < ItemList.Count - 1) then
          Result := Result + '*';
      end;
  finally
    ItemList.Free;
  end;
end;



end.
