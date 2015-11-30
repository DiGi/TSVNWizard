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

procedure GetFileVersion(const AFileName: string; out AMajor, AMinor, ABuild: Integer);

function GetCurrentRevision(const ASVNExe, AFile: string): Integer;

function GetDirectoriesFromTSVN(Project: IOTAProject): TStrings;

procedure SetDirectoriesToTSVN(Project: IOTAProject; Directories: TStrings);

implementation

uses
  SysUtils
, Windows
, IniFiles
, IOUtils
, StrUtils
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
  I: Integer;
begin
  Result := nil;

  ModServices := BorlandIDEServices as IOTAModuleServices;
  if ModServices <> nil then
  begin
    for I := 0 to ModServices.ModuleCount - 1 do
    begin
      Module := ModServices.Modules[I];

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
    for I:= 0 to Module.GetModuleFileCount-1 do
    begin
      try
        FileEditor:= Module.GetModuleFileEditor(I);
        if FileEditor <> nil then
        begin
          FileList.Add( FileEditor.GetFileName );
        end;
      except
        // hack for C++ Builder 5 OTA known issue: if the unit does not
        // have an associated form, calling GetModuleFileEditor(1) throws
        // access violation; calling GetModuleFileEditor(2) gets the .H file
        // (GetModuleFileCount returns 2, though)
        if (I = 1) and (Module.GetModuleFileCount = 2) then
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

procedure GetFileVersion(const AFileName: string; out AMajor, AMinor, ABuild: Integer);
var
  VerInfoSize: DWORD;
  VerInfo: Pointer;
  VerValueSize: DWORD;
  VerValue: PVSFixedFileInfo;
  Dummy: DWORD;
begin
  try
    VerInfoSize := GetFileVersionInfoSize(PChar(AFileName), Dummy);
    try
      GetMem(VerInfo, VerInfoSize);
      GetFileVersionInfo(PChar(AFileName), 0, VerInfoSize, VerInfo);
      VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize);
      with VerValue^ do
      begin
        AMajor := dwFileVersionMS shr 16;
        AMinor := dwFileVersionMS and $FFFF;
        ABuild := dwFileVersionLS shr 16;
      end;
    finally
      FreeMem(VerInfo, VerInfoSize);
    end;
  except
    AMajor := -1;
    AMinor := -1;
    ABuild := -1;
  end;
end;

function GetCurrentRevision(const ASVNExe, AFile: string): Integer;
const
  MaxBufSize = 1024;
type
  TCharBuffer = array[0..MaxInt - 1] of AnsiChar;
var
  pBuf: ^TCharBuffer;
  BufSize: Cardinal;
  si: STARTUPINFO;
  sa: PSecurityAttributes;
  sd: PSECURITY_DESCRIPTOR;
  pi: PROCESS_INFORMATION;
  ReadSTDOut, WriteSTDOut: THandle;
  ExitCode: LongWord;
  ByteRead: LongWord;
  ByteAvail: LongWord;
  Str, Last: AnsiString;
  I: Integer;
  EndCR: Boolean;
begin
  Result := -1;

  if (Trim(ASVNExe) = '') then
    Exit;

  if (Trim(AFile) = '') then
    Exit;

  GetMem(sa, sizeof(SECURITY_ATTRIBUTES));
  if (Win32Platform = VER_PLATFORM_WIN32_NT) then
  begin
    GetMem(sd, sizeof(SECURITY_DESCRIPTOR));
    InitializeSecurityDescriptor(sd, SECURITY_DESCRIPTOR_REVISION);
    SetSecurityDescriptorDacl(sd, True, nil, False);
    sa.lpSecurityDescriptor := sd;
  end else
  begin
    sa.lpSecurityDescriptor := nil;
    sd := nil;
  end;
  sa.nLength := sizeof(SECURITY_ATTRIBUTES);
  sa.bInheritHandle := True;
  BufSize := MaxBufSize;
  pBuf := AllocMem(BufSize);

  if not (CreatePipe(ReadSTDOut, WriteSTDOut, sa, 0)) then //create stdout pipe
  begin
    raise Exception.Create('Error creating pipe!');
  end;

  GetStartupInfo(si);
  si.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
  si.wShowWindow := SW_HIDE;
  si.hStdOutput := WriteSTDOut;
//  si.hStdError := WriteSTDOut;

  if not (CreateProcess(nil,
                        // svn.exe info <file>
                        PChar(ASVNExe + ' info ' + AFile),
                        nil,
                        nil,
                        TRUE,
                        CREATE_NEW_CONSOLE,
                        nil,
                        nil,
                        si,
                        pi)) then
  begin
    raise Exception.Create('');
  end;

  ExitCode := STILL_ACTIVE;
  try
    repeat
      GetExitCodeProcess(pi.hProcess, ExitCode);
      PeekNamedPipe(ReadSTDOut, pBuf, BufSize, @ByteRead, @ByteAvail, nil);

        if (ByteRead <> 0) then
      begin
        if (BufSize < ByteAvail) then
        begin
          BufSize := ByteAvail;
          ReallocMem(pBuf, BufSize);
        end;

        FillChar(pBuf^, BufSize, #0); //empty the buffer
        ReadFile(ReadSTDOut, pBuf^, BufSize, ByteRead, nil); //read the stdout pipe
        Str := Last; //take the begin of the line (if exists)
        I := 0;
        while (I < ByteRead) do
        begin
          case pBuf^[I] of
            #0: Inc(I);
            #10, #13:
              begin
                Inc(I);
                if not (EndCR and (pBuf^[I - 1] = #10)) then
                begin
                  if (I < ByteRead) and (pBuf^[I - 1] = #13) and (pBuf^[I] = #10) then
                  begin
                    // CRLF
                    Inc(I);
                  end
                  else
                  begin
                    // LF
                  end;

                  if (StartsText('Last Changed Rev', Str)) then
                  begin
                    // Last Changed Rev: 67452
                    Result := StrToIntDef(Copy(Str, 19, MaxInt), -1);
                  end;

                  Str := '';
                end;
              end;
          else
            begin
              Str := Str + pBuf^[I]; // add a character
              Inc(I);
            end;
          end;
        end;
        EndCR := (pBuf^[I - 1] = #13);
        Last := Str; // no CRLF found in the rest, maybe in the next output
      end;

      if ExitCode <> STILL_ACTIVE then
        Break;
    until (ExitCode <> STILL_ACTIVE);
  finally
    if (ExitCode = STILL_ACTIVE) then
      TerminateProcess(pi.hProcess, 0);
    CloseHandle(pi.hThread);
    CloseHandle(pi.hProcess);

    CloseHandle(ReadSTDOut);
    CloseHandle(WriteSTDOut);

    FreeMem(pBuf);
    if (Win32Platform = VER_PLATFORM_WIN32_NT) then
      FreeMem(sd);
    FreeMem(sa);
  end;
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
