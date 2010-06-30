unit tsvnWizard;

{$R 'icons.res'}
{$R 'Strings.res'}

interface

uses
  ToolsAPI, SysUtils, Windows, Dialogs, Menus, Registry, ShellApi,
  Classes, Controls, Graphics, ImgList, ExtCtrls, ActnList, XMLIntf;

const
  SVN_PROJECT_EXPLORER = 0;
  SVN_LOG_PROJECT = 1;
  SVN_LOG_FILE = 2;
  SVN_CHECK_MODIFICATIONS = 3;
  SVN_ADD = 4;                                                             
  SVN_UPDATE = 5;
  SVN_COMMIT = 6;
  SVN_DIFF = 7;
  SVN_REVERT = 8;
  SVN_REPOSITORY_BROWSER = 9;
  SVN_EDIT_CONFLICT = 10;
  SVN_CONFLICT_OK = 11;
  SVN_CREATE_PATCH = 12;
  SVN_USE_PATCH = 13;
  SVN_CLEAN = 14;
  SVN_IMPORT = 15;
  SVN_CHECKOUT = 16;
  SVN_SETTINGS = 17;
  SVN_ABOUT = 18;
  SVN_VERB_COUNT = 19;

var
  TSVNPath: string;
  TMergePath: string;
  Bitmaps: array[0..SVN_VERB_COUNT-1] of TBitmap;
  Actions: array[0..SVN_VERB_COUNT-1] of TAction;

type
  TProjectMenuTimer = class;

  TTortoiseSVN = class(TNotifierObject, IOTANotifier, IOTAWizard,
                       INTAProjectMenuCreatorNotifier)
  strict private
    IsPopup: Boolean;
    IsProject: Boolean;
    IsEditor: Boolean;
    CmdFiles: string;
    Timer: TTimer;
    TSvnMenu: TMenuItem;
    IsDirectory: Boolean;

    function GetVerb(Index: Integer): string;
    function GetVerbState(Index: Integer): Word;
    
    procedure Tick( sender: TObject );
    procedure DiffClick( sender: TObject );
    procedure LogClick(Sender : TObject);
    procedure ConflictClick(Sender: TObject);
    procedure ConflictOkClick(Sender: TObject);
    procedure ExecuteVerb(Index: Integer);

    procedure UpdateAction( sender: TObject );
    procedure ExecuteAction( sender: TObject );

    procedure GetCurrentModuleFileList( fileList: TStrings );

    function FindMenu(Item: TComponent): TMenu;
  private
    ProjectMenuTimer: TProjectMenuTimer;
    
    procedure TSVNMenuClick( Sender: TObject );
    procedure CreateMenu; overload;
    procedure CreateMenu(Parent: TMenuItem; const Ident: string = ''); overload;

    ///  <summary>
    ///  Returns the path for the TortoiseSVN command depending on the files in
    ///  a project. This includes entries such as ..\..\MyUnit.pas.
    ///  </summary>
    function GetPathForProject(Project: IOTAProject): string;

    procedure GetFiles(Files: TStringList);

    class procedure TSVNExec( Params: string );
    class procedure TSVNMergeExec( Params: string );
  public
    constructor Create;
    destructor Destroy; override;

    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;
    procedure Execute;

    { INTAProjectMenuCreatorNotifier }

    { The result will be inserted into the project manager local menu. Menu
      may have child menus. }
    function AddMenu(const Ident: string): TMenuItem;

    { Return True if you wish to install a project manager menu item for this
      ident.  In cases where the project manager node is a file Ident will be
      a fully qualified file name. }
    function CanHandle(const Ident: string): Boolean;
  end;

  TIdeNotifier = class(TNotifierObject, IOTAIDENotifier)
  protected
    { This procedure is called for many various file operations within the
      IDE }
    procedure FileNotification(NotifyCode: TOTAFileNotification;
      const FileName: string; var Cancel: Boolean);

    { This function is called immediately before the compiler is invoked.
      Set Cancel to True to cancel the compile }
    procedure BeforeCompile(const Project: IOTAProject; var Cancel: Boolean); overload;

    { This procedure is called immediately following a compile.  Succeeded
      will be true if the compile was successful }
    procedure AfterCompile(Succeeded: Boolean); overload;

    class function RegisterPopup(Module: IOTAModule): Boolean; overload;

    class function RegisterPopup(View: IOTAEditView): Boolean; overload;
  end;

  TModuleArray = array of IOTAModule;

  TModuleNotifier = class(TModuleNotifierObject, IOTAModuleNotifier)
  strict private
    _FileList: TStringList;
    _Filename: string;
    _Notifier: Integer;
    _Module: IOTAModule;
  protected
    { This procedure is called immediately after the item is successfully saved.
      This is not called for IOTAWizards }
    procedure AfterSave;
  public
    constructor Create(Filename: string; Module: IOTAModule);
    destructor Destroy; override;
  end;

  TProjectNotifier = class(TModuleNotifierObject, IOTAProjectNotifier)
  strict private
    FModuleCount: Integer;
    FModules: TModuleArray;
    FProject: IOTAProject;
    FFileName: String;

    procedure SetModuleCount(const Value: Integer);
  private
    function GetModule(Index: Integer): IOTAModule;
    procedure SetModule(Index: Integer; const Value: IOTAModule);
  protected
    { IOTAModuleNotifier }

    { User has renamed the module }
    procedure ModuleRenamed(const NewName: string); overload;

    { IOTAProjectNotifier }

    { This notifier will be called when a file/module is added to the project }
    procedure ModuleAdded(const AFileName: string);

    { This notifier will be called when a file/module is removed from the project }
    procedure ModuleRemoved(const AFileName: string);

    { This notifier will be called when a file/module is renamed in the project }
    procedure ModuleRenamed(const AOldFileName, ANewFileName: string); overload;

    constructor Create(const FileName: string);   

    property ModuleCount: Integer read FModuleCount write SetModuleCount;

    property Modules: TModuleArray read FModules write FModules;

    property Module[Index: Integer]: IOTAModule read GetModule write SetModule;

    property Project: IOTAProject read FProject write FProject;
  end;

  TProjectMenuTimer = class(TTimer)
  private
    procedure TimerTick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;

{$IFNDEF DLL_MODE}

procedure Register;

{$ELSE}

function InitWizard(const BorlandIDEServices: IBorlandIDEServices;
  RegisterProc: TWizardRegisterProc;
  var Terminate: TWizardTerminateProc): Boolean; stdcall;

{$ENDIF}

implementation

uses TypInfo, Contnrs;

var
  MenuCreatorNotifier: Integer = -1;
  IDENotifierIndex   : Integer = -1;
  NotifierList : TStringList;
  TortoiseSVN: TTortoiseSVN;
  EditPopup: TPopupMenu;
  EditMenuItem: TMenuItem;
  ImgIdx: array[0..SVN_VERB_COUNT] of Integer;

function GetBitmapName(Index: Integer): string;
begin
  case Index of
    SVN_PROJECT_EXPLORER:
      Result:= 'explorer';
    SVN_LOG_PROJECT,
    SVN_LOG_FILE:
      Result:= 'log';
    SVN_CHECK_MODIFICATIONS:
      Result:= 'check';
    SVN_ADD:
      Result:= 'add';
    SVN_UPDATE:
      Result:= 'update';
    SVN_COMMIT:
      Result:= 'commit';
    SVN_DIFF:
      Result:= 'diff';
    SVN_REVERT:
      Result:= 'revert';
    SVN_REPOSITORY_BROWSER:
      Result:= 'repository';
    SVN_SETTINGS:
      Result:= 'settings';
    SVN_ABOUT:
      Result:= 'about';
    SVN_EDIT_CONFLICT:
      Result := 'edconflict';
    SVN_CONFLICT_OK:
      Result := 'conflictok';
    SVN_CREATE_PATCH:
      Result := 'crpatch';
    SVN_USE_PATCH:
      Result := 'usepatch';
    SVN_CLEAN:
      Result := 'clean';
    SVN_IMPORT:
      Result := 'import';
    SVN_CHECKOUT:
      Result := 'checkout';
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

procedure GetModuleFiles( fileList: TStrings; Module: IOTAModule);
var
  FileEditor: IOTAEditor;
  i: integer;
begin
  if (Module <> nil) then
  begin
    for i:= 0 to Module.GetModuleFileCount-1 do
    begin
      try
        FileEditor:= Module.GetModuleFileEditor(i);
        if FileEditor <> nil then
        begin
          fileList.Add( FileEditor.GetFileName );
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
                fileList.Add( FileEditor.GetFileName );
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

procedure TTortoiseSVN.GetCurrentModuleFileList( FileList: TStrings );
var
  ModServices: IOTAModuleServices;
  Module: IOTAModule;
  Project: IOTAProject;
  ModInfo: IOTAModuleInfo;
  FileName: string;
begin
  FileList.Clear;

  if (IsPopup) and (not IsEditor) then
  begin
    Project := (BorlandIDEServices as IOTAProjectManager).GetCurrentSelection(FileName);

    ModInfo := Project.FindModuleInfo(FileName);
    if (ModInfo <> nil) then
    begin
      GetModuleFiles(FileList, ModInfo.OpenModule);
    end;
  end else
  begin
    ModServices := BorlandIDEServices as IOTAModuleServices;
    if (ModServices <> nil) then
    begin
      Module := ModServices.CurrentModule;
      GetModuleFiles(FileList, Module);
    end;
  end;
end;

procedure TTortoiseSVN.GetFiles(Files: TStringList);
var
  Ident: string;
  Project: IOTAProject;
  ItemList: TStringList;
  ModInfo: IOTAModuleInfo;
begin
  if (IsPopup) and (not IsEditor) then
  begin
    {
      The call is from the Popup and a file is selected
    }
    Ident := '';
    Project := (BorlandIDEServices as IOTAProjectManager).GetCurrentSelection(Ident);

    ItemList := TStringList.Create;
    try
      ModInfo := Project.FindModuleInfo(Ident);
      if (ModInfo <> nil) then
      begin
        GetModuleFiles(ItemList, ModInfo.OpenModule);

        Files.AddStrings(ItemList);
      end else
      begin
        if (DirectoryExists(Ident)) then
          Files.Add(Ident);
      end;
    finally
      ItemList.Free;
    end;
  end else
  begin
    GetCurrentModuleFileList(Files);
  end;
end;

procedure GetModifiedItems( ItemList: TStrings );
var
  ModServices: IOTAModuleServices;
  Module: IOTAModule;
  FileEditor: IOTAEditor;
  i, j: integer;
begin
  ModServices := BorlandIDEServices as IOTAModuleServices;
  
  if ModServices <> nil then
  begin
    for i:= 0 to ModServices.GetModuleCount-1 do
    begin
      Module:= ModServices.GetModule(i);
      if Module <> nil then begin
        for j:= 0 to Module.GetModuleFileCount-1 do
        begin
          try
            FileEditor:= Module.GetModuleFileEditor(j);
            if (FileEditor <> nil) and FileEditor.Modified then
              ItemList.Add(Module.FileName);
          except
            // hack for C++ Builder 5 OTA known issue: if the unit does not
            // have an associated form, calling GetModuleFileEditor(1) throws
            // access violation; calling GetModuleFileEditor(2) gets the .H file
            // (GetModuleFileCount returns 2, though)
            if (j = 1) and (Module.GetModuleFileCount = 2) then
            try
              FileEditor:= Module.GetModuleFileEditor(2);
              if (FileEditor <> nil) and FileEditor.Modified then
                  ItemList.Add(Module.FileName);
            except
            end;
          end;
        end;
      end;
    end;
  end;
end;

function TTortoiseSVN.AddMenu(const Ident: string): TMenuItem;
begin
  {
    Get's created every time a user right-clicks a file or project.
  }
  Result := TMenuItem.Create(nil);
  Result.Name := 'Submenu';
  Result.Caption := 'TortoiseSVN';
  Result.OnClick := TSVNMenuClick;

  if (SameText(Ident, sFileContainer)) then
  begin
    CreateMenu(Result, sFileContainer);
    Result.Tag := 1;
  end
  else if (SameText(Ident, sProjectContainer)) then
  begin
    CreateMenu(Result, sProjectContainer);
    Result.Tag := 2;
  end
  else if (SameText(Ident, sDirectoryContainer)) then
  begin
    CreateMenu(Result, sDirectoryContainer);
    Result.Tag := 8;
  end;

  // Disable for now - didn't work properly
  // ProjectMenuTimer := TProjectMenuTimer.Create(Result);
end;

function TTortoiseSVN.CanHandle(const Ident: string): Boolean;
begin
  Result := SameText(Ident, sFileContainer) or
            SameText(Ident, sProjectContainer) or
            SameText(Ident, sDirectoryContainer);
end;

procedure TTortoiseSVN.ConflictClick(Sender: TObject);
var
  Files: TStringList;
  Item: TComponent;
begin
  if (Sender is TComponent) then
  begin
    Item := TComponent(Sender);

    Files := TStringList.Create;
    try
      GetFiles(Files);

      if (Files.Count > 1) then
        TSVNExec( '/command:conflicteditor /notempfile /path:' + AnsiQuotedStr( Files[Item.Tag], '"' ) )
      else if (Files.Count = 1) then
        TSVNExec( '/command:conflicteditor /notempfile /path:' + AnsiQuotedStr( Files[0], '"' ) );
    finally
      Files.Free;
    end;
  end;
end;

procedure TTortoiseSVN.ConflictOkClick(Sender: TObject);
var
  Files: TStringList;
  Item: TComponent;
begin
  if (Sender is TComponent) then
  begin
    Item := TComponent(Sender);

    Files := TStringList.Create;
    try
      GetFiles(Files);

      if (Files.Count > 1) then
        TSVNExec( '/command:resolve /notempfile /path:' + AnsiQuotedStr( Files[Item.Tag], '"' ) )
      else if (Files.Count = 1) then
        TSVNExec( '/command:resolve /notempfile /path:' + AnsiQuotedStr( Files[0], '"' ) );
    finally
      Files.Free;
    end;
  end;
end;

constructor TTortoiseSVN.Create;
var
  Reg: TRegistry;
  I: Integer;
// defines for 64-bit registry access, copied from Windows include file
// (older IDE versions won't find them otherwise)
const
  KEY_WOW64_64KEY = $0100;
  KEY_WOW64_32KEY = $0200;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKeyReadOnly( '\SOFTWARE\TortoiseSVN' ) then
    begin
      TSVNPath   := Reg.ReadString( 'ProcPath' );
      TMergePath := Reg.ReadString( 'TMergePath' );
    end
    else
    begin
      //try 64 bit registry
      Reg.Access := Reg.Access or KEY_WOW64_64KEY;
      if Reg.OpenKeyReadOnly( '\SOFTWARE\TortoiseSVN' ) then
      begin
        TSVNPath   := Reg.ReadString( 'ProcPath' );
        TMergePath := Reg.ReadString( 'TMergePath' );
      end
      else begin
          //try WOW64 bit registry
          Reg.Access := Reg.Access or KEY_WOW64_32KEY;
          if Reg.OpenKeyReadOnly( '\SOFTWARE\TortoiseSVN' ) then
          begin
            TSVNPath   := Reg.ReadString( 'ProcPath' );
            TMergePath := Reg.ReadString( 'TMergePath' );
          end;
      end;
    end;
  finally
    Reg.CloseKey;
    Reg.Free;
  end;

  TSvnMenu:= nil;

  Timer := TTimer.Create(nil);
  Timer.Interval := 200;
  Timer.OnTimer := Tick;
  Timer.Enabled := True;

  TortoiseSVN := Self;

  for I := 0 to SVN_VERB_COUNT do
  begin
    ImgIdx[I] := -1;
  end;
end;

procedure TTortoiseSVN.CreateMenu(Parent: TMenuItem; const Ident: string = '');
var
  Item: TMenuItem;
  I: Integer;
  Menu: TMenu;
begin
  if (Parent = nil) then Exit;

  Menu := FindMenu(Parent);

  for I := 0 to SVN_VERB_COUNT - 1 do
  begin
    if (Ident <> '') then
    begin
      // Ignore the project specific entries for the file container
      if (SameText(Ident, sFileContainer)) and
         (I in [SVN_PROJECT_EXPLORER, SVN_LOG_PROJECT, SVN_REPOSITORY_BROWSER, SVN_IMPORT, SVN_CHECKOUT, SVN_CLEAN, SVN_USE_PATCH]) then
        Continue;

      // Ignore the project and some file specific entries for the directory container
      if (SameText(Ident, sDirectoryContainer)) and
         (I in [SVN_PROJECT_EXPLORER, SVN_LOG_PROJECT, SVN_REPOSITORY_BROWSER, SVN_IMPORT, SVN_CHECKOUT, SVN_CLEAN, SVN_USE_PATCH, SVN_CONFLICT_OK, SVN_EDIT_CONFLICT]) then
        Continue;

      // Ignore the file specific entries for the project container
      if (SameText(Ident, sProjectContainer)) and
         (I in [SVN_LOG_FILE, SVN_DIFF, SVN_CONFLICT_OK, SVN_EDIT_CONFLICT]) then
        Continue;

      // Ignore about and settings in the popup
      if (I in [SVN_ABOUT, SVN_SETTINGS]) then
        Continue;
    end;

    if (Bitmaps[I] = nil) then
    begin
      Bitmaps[I] := TBitmap.Create;
      try
        Bitmaps[I].LoadFromResourceName( HInstance, getBitmapName(i) );
      except
      end;
    end;

    if (Actions[I] = nil) then
    begin
      Actions[I] := TAction.Create(nil);
      Actions[I].ActionList := (BorlandIDEServices as INTAServices).ActionList;
      Actions[I].Caption := GetVerb(I);
      Actions[I].Hint := GetVerb(I);

      if (Bitmaps[I].Width = 16) and (Bitmaps[I].height = 16) then
      begin
        Actions[I].ImageIndex := (BorlandIDEServices as INTAServices).AddMasked(Bitmaps[I], clBlack);
      end;

      Actions[I].OnUpdate:= UpdateAction;
      Actions[I].OnExecute:= ExecuteAction;
      Actions[I].Tag := I;
    end;

    Item := TMenuItem.Create(Parent);
    if (I <> SVN_DIFF) and
       (I <> SVN_LOG_FILE) and
       (I <> SVN_EDIT_CONFLICT) and
       (I <> SVN_CONFLICT_OK) then
    begin
      Item.Action := Actions[I];
    end
    else
    begin
      if (Item.ImageIndex = -1) then
      begin
        if (Menu <> nil) then
          Item.ImageIndex := Menu.Images.AddMasked(Bitmaps[I], clBlack)
      end;
    end;
    Item.Tag := I;

    Parent.Add(Item);
  end;
end;

procedure TTortoiseSVN.Tick(Sender: TObject);
var
  Intf: INTAServices;
  I: Integer;
begin
  if (BorlandIDEServices.QueryInterface(INTAServices, Intf) = S_OK) then
  begin
    Self.CreateMenu;
    Timer.Free;
    Timer := nil;
  end;

  for I := 0 to (BorlandIDEServices as IOTAModuleServices).ModuleCount - 1 do
  begin
    TIdeNotifier.RegisterPopup((BorlandIDEServices as IOTAModuleServices).Modules[I]);
  end;
end;

procedure TTortoiseSVN.TSVNMenuClick( Sender: TObject );
var
  ItemList, Files: TStringList;
  I: integer;
  Diff, Log, Item, Conflict, ConflictOk: TMenuItem;
  Ident: string;
  Parent: TMenuItem;
  Project: IOTAProject;
  ModInfo: IOTAModuleInfo;
begin
  // update the diff item and submenu; the diff action is handled by the
  // menu item itself, not by the action list
  // the 'log file' item behaves in a similar way

  if (Sender is TMenuItem) then
    Parent := TMenuItem(Sender)
  else
    Exit;

  IsPopup := (Parent.Tag > 0);

  IsProject := (Parent.Tag and 2) = 2;

  IsEditor := (Parent.Tag and 4) = 4;

  IsDirectory := (Parent.Tag and 4) = 4;

  Diff := nil; Log := nil; Conflict := nil; ConflictOk := nil;

  for I := 0 to Parent.Count - 1 do
  begin
    if (Parent.Items[I].Tag = SVN_DIFF) then
    begin
      Diff := Parent.Items[I];
      Diff.Action:= nil;
      Diff.OnClick:= nil;
      Diff.Enabled:= False;
      Diff.Clear;
    end
    else if (Parent.Items[I].Tag = SVN_LOG_FILE) then
    begin
      Log := Parent.Items[I];
      Log.Action := nil;
      Log.OnClick := nil;
      Log.Enabled := False;
      Log.Clear();
    end
    else if (Parent.Items[I].Tag = SVN_EDIT_CONFLICT) then
    begin
      Conflict := Parent.Items[I];
      Conflict.Action := nil;
      Conflict.OnClick := nil;
      Conflict.Enabled := False;
      Conflict.Clear();
    end
    else if (Parent.Items[I].Tag = SVN_CONFLICT_OK) then
    begin
      ConflictOk := Parent.Items[I];
      ConflictOk.Action := nil;
      ConflictOk.OnClick := nil;
      ConflictOk.Enabled := False;
      ConflictOk.Clear();
    end;
  end;

  Files := TStringList.create;

  if (IsPopup) and (not IsEditor) then
  begin
    {
      The call is from the Popup and a file is selected
    }
    Ident := '';
    Project := (BorlandIDEServices as IOTAProjectManager).GetCurrentSelection(Ident);

    ItemList := TStringList.Create;
    try
      ModInfo := Project.FindModuleInfo(Ident);
      if (ModInfo <> nil) then
      begin
        GetModuleFiles(ItemList, ModInfo.OpenModule);

        Files.AddStrings(ItemList);
      end else
      begin
        if (DirectoryExists(Ident)) then
          Files.Add(Ident);
      end;
    finally
      ItemList.Free;
    end;
  end else
  begin
    GetCurrentModuleFileList(Files);
  end;

  CmdFiles := '';
  for I := 0 to Files.Count - 1 do
  begin
    CmdFiles := CmdFiles + Files[I];
    if (I < Files.Count - 1) then
      CmdFiles := CmdFiles + '*';
  end;

  if Files.Count > 0 then
  begin
    if (Diff <> nil) then
    begin
      Diff.Enabled:= True;
      Diff.Caption:= GetString(SVN_DIFF);
      if (not IsPopup) then
      begin
        Diff.ImageIndex := Actions[SVN_DIFF].ImageIndex;
      end;
    end;

    if (Log <> nil) then
    begin
      Log.Enabled := True;
      Log.Caption := GetString(SVN_LOG_FILE);
      if (not IsPopup) then
        Log.ImageIndex := Actions[SVN_LOG_FILE].ImageIndex;
    end;

    if (Conflict <> nil) then
    begin
      Conflict.Enabled := True;
      Conflict.Caption := GetString(SVN_EDIT_CONFLICT);
      if (not IsPopup) then
        Conflict.ImageIndex := Actions[SVN_EDIT_CONFLICT].ImageIndex;
    end;

    if (ConflictOk <> nil) then
    begin
      ConflictOk.Enabled := True;
      ConflictOk.Caption := GetString(SVN_CONFLICT_OK);
      if (not IsPopup) then
        ConflictOk.ImageIndex := Actions[SVN_CONFLICT_OK].ImageIndex;
    end;

    if Files.Count > 1 then
    begin
      for I := 0 to Files.Count - 1 do begin
        if (Diff <> nil) then
        begin
          Item := TMenuItem.Create(diff);
          Item.Caption:= ExtractFileName( files[i] );
          Item.OnClick:= DiffClick;
          Item.Tag:= I;
          Diff.Add(Item);
        end;

        if (Log <> nil) then
        begin
          Item := TMenuItem.Create(log);
          Item.Caption := ExtractFileName( files[i] );
          Item.OnClick := LogClick;
          Item.Tag := I;
          Log.Add(Item);
        end;

        if (Conflict <> nil) then
        begin
          Item := TMenuItem.Create(log);
          Item.Caption := ExtractFileName( files[i] );
          Item.OnClick := ConflictClick;
          Item.Tag := I;
          Conflict.Add(Item);
        end;

        if (ConflictOk <> nil) then
        begin
          Item := TMenuItem.Create(log);
          Item.Caption := ExtractFileName( files[i] );
          Item.OnClick := ConflictOkClick;
          Item.Tag := I;
          ConflictOk.Add(Item);
        end;
      end;
    end else
    begin  // files.Count = 1
      if (Diff <> nil) then
      begin
        Diff.Caption:= GetString(SVN_DIFF) + ' ' + ExtractFileName( Files[0] );
        Diff.OnClick:= DiffClick;
      end;

      if (Log <> nil) then
      begin
        Log.Caption := GetString(SVN_LOG_FILE) + ' ' + ExtractFileName( Files[0] );
        Log.OnClick := LogClick;
      end;

      if (Conflict <> nil) then
      begin
        Conflict.Caption := GetString(SVN_EDIT_CONFLICT) + ' ' + ExtractFileName( Files[0] );
        Conflict.OnClick := ConflictClick;
      end;

      if (ConflictOk <> nil) then
      begin
        ConflictOk.Caption := GetString(SVN_CONFLICT_OK) + ' ' + ExtractFileName( Files[0] );
        ConflictOk.OnClick := ConflictOkClick;
      end;
    end;
  end;
  Files.free;
end;

class procedure TTortoiseSVN.TSVNMergeExec(params: string);
var
  cmdLine: AnsiString;
begin
  cmdLine:= AnsiString( TMergePath + ' ' + params );
  WinExec( pansichar(cmdLine), SW_SHOW );
end;

procedure TTortoiseSVN.DiffClick( Sender: TObject );
var
  Files: TStringList;
  Item: TComponent;
begin
  if (Sender is TComponent) then
  begin
    Item := TComponent(Sender);

    Files := TStringList.Create;
    try
      GetFiles(Files);

      if (Files.Count > 1) then
        TSVNExec( '/command:diff /notempfile /path:' + AnsiQuotedStr( Files[Item.Tag], '"' ) )
      else if (Files.Count = 1) then
        TSVNExec( '/command:diff /notempfile /path:' + AnsiQuotedStr( Files[0], '"' ) );
    finally
      Files.Free;
    end;
  end;
end;

procedure TTortoiseSVN.LogClick(Sender : TObject);
var
  Files : TStringList;
  Item  : TComponent;
begin
  if (Sender is TComponent) then
  begin
    Item  := TComponent(Sender);

    Files := TStringList.Create;
    try
      GetFiles(Files);

      if (Files.Count > 1) then
        TSVNExec('/command:log /notempfile /path:' + AnsiQuotedStr(Files[Item.Tag], '"'))
      else if (Files.Count = 1) then
        TSVNExec('/command:log /notempfile /path:' + AnsiQuotedStr(Files[0], '"'));
    finally
      Files.Free;
    end;
  end;
end;

procedure TTortoiseSVN.CreateMenu;
var
  MainMenu: TMainMenu;
  ProjManager: IOTAProjectManager;
  Services: IOTAServices;
begin
  if (TSvnMenu <> nil) then exit;

  TSvnMenu := TMenuItem.Create(nil);
  TSvnMenu.Caption := 'TortoiseSVN';
  TSvnMenu.Name := 'TortoiseSVNMain';
  TSvnMenu.OnClick := TSVNMenuClick;

  CreateMenu(TSvnMenu);

  MainMenu := (BorlandIDEServices as INTAServices).MainMenu;
  MainMenu.Items.Insert(MainMenu.Items.Count-1, TSvnMenu);

  if Supports(BorlandIDEServices, IOTAProjectManager, ProjManager) then
  begin
    MenuCreatorNotifier := ProjManager.AddMenuCreatorNotifier(Self);
  end;

  if Supports(BorlandIDEServices, IOTAServices, Services) then
  begin
    IDENotifierIndex := Services.AddNotifier(TIdeNotifier.Create);
  end;
end;

destructor TTortoiseSVN.Destroy;
var
  I: Integer;
begin
  if (TSvnMenu <> nil) then
  begin
    TSvnMenu.free;
  end;

  for I := Low(Actions) to High(Actions) do
  begin
    try
      if (Actions[I] <> nil) then
        Actions[I].Free;
    except
    end;
  end;

  for I := Low(Bitmaps) to High(Bitmaps) do
  begin
    try
      if (Bitmaps[I] <> nil) then
        Bitmaps[I].Free;
    except
    end;
  end;

  TortoiseSVN := nil;

  inherited;
end;

function TTortoiseSVN.GetVerb(Index: Integer): string;
begin
  Result := GetString(index);
  Exit;
end;

const vsEnabled = 1;

function TTortoiseSVN.GetVerbState(Index: Integer): Word;
begin
  Result:= 0;
  case index of
    SVN_PROJECT_EXPLORER:
      if GetCurrentProject <> nil then
        Result:= vsEnabled;
    SVN_LOG_PROJECT:
      if GetCurrentProject <> nil then
        Result:= vsEnabled;
    SVN_LOG_FILE:
      // this verb state is updated by the menu itself
      ;
    SVN_CHECK_MODIFICATIONS:
      if GetCurrentProject <> nil then
        Result:= vsEnabled;
    SVN_ADD:
      if GetCurrentProject <> nil then
        Result:= vsEnabled;
    SVN_UPDATE:
      if GetCurrentProject <> nil then
        Result:= vsEnabled;
    SVN_COMMIT:
      if GetCurrentProject <> nil then
        Result:= vsEnabled;
    SVN_DIFF:
      // this verb state is updated by the menu itself
      ;
    SVN_REVERT:
      if GetCurrentProject <> nil then
        Result:= vsEnabled;
    SVN_REPOSITORY_BROWSER:
      Result:= vsEnabled;
    SVN_SETTINGS:
      Result:= vsEnabled;
    SVN_ABOUT:
      Result:= vsEnabled;
    SVN_EDIT_CONFLICT,
    SVN_CONFLICT_OK:
      // these verb's state is updated by the menu itself
      ;
    SVN_CREATE_PATCH:
      Result := vsEnabled;
    SVN_USE_PATCH:
      Result := vsEnabled;
    SVN_CLEAN:
      Result := vsEnabled;
    SVN_IMPORT:
      Result := vsEnabled;
    SVN_CHECKOUT:
      Result := vsEnabled;
  end;
end;

class procedure TTortoiseSVN.TSVNExec( Params: string );
var
  CmdLine: AnsiString;
begin
  CmdLine := AnsiString(TSVNPath + ' ' + params );
  WinExec( pansichar(cmdLine), SW_SHOW );
end;

procedure TTortoiseSVN.ExecuteVerb(Index: Integer);
var
  Project: IOTAProject;
  ItemList: TStringList;
  ModifiedItems: boolean;
  ModifiedItemsMessage: string;
  I: integer;
  Response: Word;
  FileName, Cmd: string;
begin
  Project := GetCurrentProject();

  case index of
    SVN_PROJECT_EXPLORER:
      if (not IsPopup) or (IsProject) then
      begin
        {
          If it's a project which is accessed via the popup menu, then that
          project needs to be loaded and not the popup of the current file
          in the editor.
        }
        if (IsProject) then
        begin
          Project := (BorlandIDEServices as IOTAProjectManager).GetCurrentSelection(FileName);
        end;

        if (Project <> nil) then
          ShellExecute( 0, 'open', pchar( ExtractFilePath(Project.GetFileName) ), '', '', SW_SHOWNORMAL );
      end;
    SVN_LOG_PROJECT:
      if (not IsPopup) or (IsProject) then
      begin
        {
          If it's a project which is accessed via the popup menu, then that
          project needs to be loaded and not the popup of the current file
          in the editor.
        }
        if (IsProject) then
        begin
          Project := (BorlandIDEServices as IOTAProjectManager).GetCurrentSelection(FileName);
        end;

        if (Project <> nil) then
          TSVNExec( '/command:log /notempfile /path:' + AnsiQuotedStr( GetPathForProject(Project), '"' ) );
      end;
    SVN_LOG_FILE:
        // this verb is handled by its menu item
        ;
    SVN_CHECK_MODIFICATIONS:
      if ((not IsPopup) or (IsProject)) and (not IsEditor) then
      begin
        {
          If it's a project which is accessed via the popup menu, then that
          project needs to be loaded and not the popup of the current file
          in the editor.
        }
        if (IsProject) then
        begin
          Project := (BorlandIDEServices as IOTAProjectManager).GetCurrentSelection(FileName);
        end;

        if (Project <> nil) then
          TSVNExec( '/command:repostatus /notempfile /path:' + AnsiQuotedStr( GetPathForProject(Project), '"' ) );
      end else
      begin
        {
          The call is from the Popup and a file is selected
        }
        Cmd := '/command:repostatus /notempfile /path:' + AnsiQuotedStr(CmdFiles, '"');

        TSVNExec(Cmd);
      end;
    SVN_ADD:
      if (not IsPopup) or (IsProject) then
      begin
        {
          If it's a project which is accessed via the popup menu, then that
          project needs to be loaded and not the popup of the current file
          in the editor.
        }
        if (IsProject) then
        begin
          Project := (BorlandIDEServices as IOTAProjectManager).GetCurrentSelection(FileName);
        end;

        if (Project <> nil) then
          TSVNExec( '/command:add /notempfile /path:' + AnsiQuotedStr( ExtractFilePath(Project.GetFileName), '"' ) );
      end else
      begin
        {
          The call is from the Popup and a file is selected
        }
        Cmd := '/command:add /notempfile /path:' + AnsiQuotedStr(CmdFiles, '"');

        TSVNExec(Cmd);
      end;
    SVN_UPDATE:
      if (not IsPopup) or (IsProject) then
      begin
        {
          If it's a project which is accessed via the popup menu, then that
          project needs to be loaded and not the popup of the current file
          in the editor.
        }
        if (IsProject) then
        begin
          Project := (BorlandIDEServices as IOTAProjectManager).GetCurrentSelection(FileName);
        end;

        if (Project <> nil) then
        begin
          ItemList := TStringList.Create;
          try
            GetModifiedItems(ItemList);
            ModifiedItems := (ItemList.Count > 0);

            if ModifiedItems then
            begin
              modifiedItemsMessage := GetString(25) + #13#10#13#10;
              for i:= 0 to ItemList.Count-1 do
                modifiedItemsMessage:= modifiedItemsMessage + '    ' + ItemList[i] + #13#10;
              modifiedItemsMessage:= modifiedItemsMessage + #13#10 + GetString(26);
            end;
          finally
            ItemList.Free;
          end;

          if ModifiedItems then
          begin
            response:= MessageDlg( modifiedItemsMessage, mtWarning, [mbYes, mbNo, mbCancel], 0 );
            if response = mrYes then
              (BorlandIDEServices as IOTAModuleServices).saveAll
            else if response = mrCancel then
              Exit;
          end;

          TSVNExec( '/command:update /notempfile /path:' + AnsiQuotedStr( GetPathForProject(Project), '"' ) );
        end;
      end else
      begin
        {
          The call is from the Popup and a file is selected
        }
        Cmd := '/command:update /notempfile /path:' + AnsiQuotedStr(CmdFiles, '"');

        TSVNExec(Cmd);
      end;
    SVN_COMMIT:
      if (not IsPopup) or (IsProject) then
      begin
        {
          If it's a project which is accessed via the popup menu, then that
          project needs to be loaded and not the popup of the current file
          in the editor.
        }
        if (IsProject) then
        begin
          Project := (BorlandIDEServices as IOTAProjectManager).GetCurrentSelection(FileName);
        end;
            
        if (Project <> nil) then
        begin
          ItemList := TStringList.Create;
          try
            GetModifiedItems(ItemList);
            ModifiedItems := ItemList.Count > 0;

            if ModifiedItems then
            begin
              modifiedItemsMessage := GetString(25) + #13#10#13#10;
              for i:= 0 to ItemList.Count-1 do
                modifiedItemsMessage:= modifiedItemsMessage + '    ' + ItemList[i] + #13#10;
              modifiedItemsMessage:= modifiedItemsMessage + #13#10 + GetString(27);
            end;
          finally
            ItemList.Free;
          end;
          
          if ModifiedItems then
          begin
            response:= MessageDlg( modifiedItemsMessage, mtWarning, [mbYes, mbNo, mbCancel], 0 );
            if response = mrYes then
              (BorlandIDEServices as IOTAModuleServices).saveAll
            else if response = mrCancel then
              Exit;
          end;

          TSVNExec( '/command:commit /notempfile /path:' + AnsiQuotedStr( GetPathForProject(Project), '"' ) );
        end;
      end else
      begin
        {
          The call is from the Popup and a file is selected
        }
        Cmd := '/command:commit /notempfile /path:' + AnsiQuotedStr(CmdFiles, '"');

        TSVNExec(Cmd);
      end;
    SVN_DIFF:
        // this verb is handled by its menu item
        ;
    SVN_REVERT:
      if (not IsPopup) or (IsProject) then
      begin
        {
          If it's a project which is accessed via the popup menu, then that
          project needs to be loaded and not the popup of the current file
          in the editor.
        }
        if (IsProject) then
        begin
          Project := (BorlandIDEServices as IOTAProjectManager).GetCurrentSelection(FileName);
        end;
            
        if (Project <> nil) then
          TSVNExec( '/command:revert /notempfile /path:' + AnsiQuotedStr( GetPathForProject(Project), '"' ) );
      end else
      begin
        {
          The call is from the Popup and a file is selected
        }
        Cmd := '/command:revert /notempfile /path:' + AnsiQuotedStr(CmdFiles, '"');

        TSVNExec(Cmd);
      end;
    SVN_REPOSITORY_BROWSER:
      if (not IsPopup) or (IsProject) then
      begin
        {
          If it's a project which is accessed via the popup menu, then that
          project needs to be loaded and not the popup of the current file
          in the editor.
        }
        if (IsProject) then
        begin
          Project := (BorlandIDEServices as IOTAProjectManager).GetCurrentSelection(FileName);
        end;

        if (Project <> nil) then
          TSVNExec( '/command:repobrowser /notempfile /path:' + AnsiQuotedStr( ExtractFilePath(project.GetFileName), '"' ) )
        else
          TSVNExec( '/command:repobrowser' );
      end;
    SVN_SETTINGS:
        TSVNExec( '/command:settings' );
    SVN_ABOUT:
        TSVNExec( '/command:about' );
    SVN_EDIT_CONFLICT,
    SVN_CONFLICT_OK:
        // these verbs are handled by their menu item
        ;
    SVN_CREATE_PATCH:
      if (not IsPopup) or (IsProject) then
      begin
        {
          If it's a project which is accessed via the popup menu, then that
          project needs to be loaded and not the popup of the current file
          in the editor.
        }
        if (IsProject) then
        begin
          Project := (BorlandIDEServices as IOTAProjectManager).GetCurrentSelection(FileName);
        end;
            
        if (Project <> nil) then
          TSVNExec( '/command:createpatch /notempfile /path:' + AnsiQuotedStr( ExtractFilePath(Project.GetFileName), '"' ) );
      end else
      begin
        {
          The call is from the Popup and a file is selected
        }
        Cmd := '/command:createpatch /notempfile /path:' + AnsiQuotedStr(CmdFiles, '"');

        TSVNExec(Cmd);
      end;
    SVN_USE_PATCH:
      if (not IsPopup) or (IsProject) then
      begin
        {
          If it's a project which is accessed via the popup menu, then that
          project needs to be loaded and not the popup of the current file
          in the editor.
        }
        if (IsProject) then
        begin
          Project := (BorlandIDEServices as IOTAProjectManager).GetCurrentSelection(FileName);
        end;

        if (Project <> nil) then
          TSVNMergeExec( '/patchpath:' + AnsiQuotedStr( ExtractFilePath(Project.GetFileName), '"' ) );
      end;
    SVN_CLEAN:
      if (not IsPopup) or (IsProject) then
      begin
        {
          If it's a project which is accessed via the popup menu, then that
          project needs to be loaded and not the popup of the current file
          in the editor.
        }
        if (IsProject) then
        begin
          Project := (BorlandIDEServices as IOTAProjectManager).GetCurrentSelection(FileName);
        end;

        if (Project <> nil) then
          TSVNExec( '/command:cleanup /notempfile /path:' + AnsiQuotedStr( ExtractFilePath(Project.GetFileName), '"' ) )
      end;
    SVN_IMPORT:
        if (Project <> nil) then
          TSVNExec( '/command:import /notempfile /path:' + AnsiQuotedStr( ExtractFilePath(Project.GetFileName), '"' ) );
    SVN_CHECKOUT:
        if (Project <> nil) then
          TSVNExec( '/command:checkout /notempfile /path:' + AnsiQuotedStr( ExtractFilePath(Project.GetFileName), '"' ) )
        else
          TSVNExec( '/command:checkout /notempfile' );
  end;
end;

function TTortoiseSVN.FindMenu(Item: TComponent): TMenu;
begin
  if (Item = nil) then
  begin
    Result := nil;
    Exit;
  end;

  if (Item is TMenu) then
  begin
    Result := TMenu(Item);
    Exit;
  end else
    Result := FindMenu(Item.Owner);
end;

procedure TTortoiseSVN.UpdateAction( sender: TObject );
var action: TAction;
begin
  action:= sender as TAction;
  action.Enabled := getVerbState( action.tag ) = vsEnabled;
end;

procedure TTortoiseSVN.ExecuteAction( sender: TObject );
var action: TAction;
begin
  action:= sender as TAction;
  executeVerb( action.tag );
end;


function TTortoiseSVN.GetIDString: string;
begin
  result:= 'Subversion.TortoiseSVN';
end;

function TTortoiseSVN.GetName: string;
begin
  result:= 'TortoiseSVN add-in';
end;

function TTortoiseSVN.GetPathForProject(Project: IOTAProject): string;
var
  I: Integer;
  Path: TStringList;
  ModInfo: IOTAModuleInfo;
  FilePath: string;
begin
  Path := TStringList.Create;
  try
    Path.Add(ExtractFilePath(Project.FileName));

    for I := 0 to Project.GetModuleCount - 1 do
    begin
      ModInfo := Project.GetModule(I);
      if (ModInfo.ModuleType <> omtPackageImport) and
         (ModInfo.ModuleType <> omtTypeLib) then
      begin
        FilePath := ExtractFilePath(ModInfo.FileName);
        if (Path.IndexOf(FilePath) = -1) then
          Path.Add(FilePath);
      end;
    end;

    Result := '';
    for I := 0 to Path.Count - 1 do
    begin
      Result := Result + Path.Strings[I];
      if (I < Path.Count - 1) then
        Result := Result + '*';
    end;
  finally
    Path.Free;
  end;
end;

function TTortoiseSVN.GetState: TWizardState;
begin
  result:= [wsEnabled];
end;

procedure TTortoiseSVN.Execute;
begin
  // empty
end;

{$IFNDEF DLL_MODE}

procedure Register;
begin
  RegisterPackageWizard(TTortoiseSVN.create);
end;

{$ELSE}

var wizardID: integer;

procedure FinalizeWizard;
var
  WizardServices: IOTAWizardServices;
begin
  Assert(Assigned(BorlandIDEServices));

  WizardServices := BorlandIDEServices as IOTAWizardServices;
  Assert(Assigned(WizardServices));

  WizardServices.RemoveWizard(wizardID);
end;

function InitWizard(const BorlandIDEServices: IBorlandIDEServices;
  RegisterProc: TWizardRegisterProc;
  var Terminate: TWizardTerminateProc): Boolean; stdcall;
var
  WizardServices: IOTAWizardServices;
begin
  Assert(BorlandIDEServices <> nil);
  Assert(ToolsAPI.BorlandIDEServices = BorlandIDEServices);

  Terminate := FinalizeWizard;

  WizardServices := BorlandIDEServices as IOTAWizardServices;
  Assert(Assigned(WizardServices));

  wizardID:= WizardServices.AddWizard(TTortoiseSVN.Create as IOTAWizard);

  result:= wizardID >= 0;
end;


exports
  InitWizard name WizardEntryPoint;

{$ENDIF}

{ TIdeNotifier }

procedure TIdeNotifier.AfterCompile(Succeeded: Boolean);
begin
  // empty
end;

procedure TIdeNotifier.BeforeCompile(const Project: IOTAProject;
  var Cancel: Boolean);
begin
  // empty
end;

function IsProjectFile(const FileName: string; var Project: IOTAProject): Boolean;
var
  Module  : IOTAModule;
  ProjectGroup : IOTAProjectGroup;
begin
  Module := (BorlandIDEServices as IOTAModuleServices).FindModule(FileName);
  Result := Supports(Module, IOTAProject, Project) and
            not Supports(Module, IOTAProjectGroup, ProjectGroup);
end;

procedure TIdeNotifier.FileNotification(NotifyCode: TOTAFileNotification;
  const FileName: string; var Cancel: Boolean);
var
  Project  : IOTAProject;
  Module   : IOTAModule;
  I, Index : Integer;
  Notifier : TProjectNotifier;
begin
  case NotifyCode of
    ofnFileOpened:
    begin
      if IsProjectFile(FileName, Project) then
      begin
        Notifier := TProjectNotifier.Create(FileName);
        Notifier.Project := Project;
        Notifier.ModuleCount := Project.GetModuleFileCount;
        for I := 0 to Notifier.ModuleCount - 1 do
        begin
          Notifier.Module[I] := Project.ModuleFileEditors[I].Module;
        end;

        Index := Project.AddNotifier(Notifier as IOTAProjectNotifier);
        if (Index >= 0) then
          NotifierList.AddObject(FileName, Pointer(Index));
      end else
      begin
        Module := (BorlandIDEServices as IOTAModuleServices).FindModule(FileName);
        if not Assigned(Module) then Exit;

        RegisterPopup(Module);
      end;
    end;
    ofnProjectDesktopLoad:
    begin
      // FileName = *.dsk
      for I := 0 to (BorlandIDEServices as IOTAModuleServices).ModuleCount - 1 do
      begin
        Module := (BorlandIDEServices as IOTAModuleServices).Modules[I];

        RegisterPopup(Module);
      end;
    end;
    ofnFileClosing :
    begin
      if NotifierList.Find(FileName, I) then
      begin
        Index := Integer(NotifierList.Objects[I]);
        NotifierList.Delete(I);
        Module := (BorlandIDEServices as IOTAModuleServices).FindModule(FileName);
        Module.RemoveNotifier(Index);
      end;
    end;
  end;
end;


class function TIdeNotifier.RegisterPopup(Module: IOTAModule): Boolean;
var
  I, K: Integer;
  Editor: IOTAEditor;
  SourceEditor: IOTASourceEditor;
begin
  Result := False;
  
  for I := 0 to Module.GetModuleFileCount - 1 do
  begin
    Editor := nil;
    try
      Editor := Module.GetModuleFileEditor(I);
    except
    end;

    if Assigned(Editor) and Supports(Editor, IOTASourceEditor, SourceEditor) then
    begin
      for K := 0 to SourceEditor.EditViewCount - 1 do
      begin
        Result := RegisterPopup(SourceEditor.EditViews[K]);
        if (Result) then Exit;
      end;
    end;
  end;
end;

class function TIdeNotifier.RegisterPopup(View: IOTAEditView): Boolean;
begin
  if (Assigned(EditMenuItem)) then
  begin
    Result := True;
    Exit;
  end;
  
  if (EditPopup = nil) then
  begin
    try
      EditPopup := (View.GetEditWindow.Form.FindComponent('EditorLocalMenu') as TPopupMenu);
    except
    end;
  end;

  if (not Assigned(EditPopup)) then
  begin
    Result := False;
    Exit;
  end;

  if (EditMenuItem = nil) then
  begin
    EditMenuItem := TMenuItem.Create(EditPopup);
    EditMenuItem .Caption := 'TortoiseSVN';
    EditMenuItem.Visible := True;
    EditMenuItem.Tag := 4 or 1;
    EditMenuItem.OnClick := TortoiseSVN.TSVNMenuClick;
      
    TortoiseSVN.CreateMenu(EditMenuItem, sFileContainer);
    EditPopup.Items.Add(EditMenuItem);
  end;
  Result := True;
end;

{ TProjectNotifier }

constructor TProjectNotifier.Create(const FileName: string);
begin
  inherited Create;

  FFileName := FileName;
end;

function TProjectNotifier.GetModule(Index: Integer): IOTAModule;
begin
  Result := FModules[Index];
end;

procedure TProjectNotifier.ModuleRenamed(const AOldFileName,
  ANewFileName: string);
var
  I, Index : Integer;
begin
  if NotifierList.Find(AOldFileName, I) then
  begin
    Index := Integer(NotifierList.Objects[I]);
    NotifierList.Delete(I);
    NotifierList.AddObject(ANewFileName, Pointer(Index));
    FFileName := ANewFileName;
  end;
end;

procedure TProjectNotifier.ModuleRenamed(const NewName: string);
begin
  ModuleRenamed(FFileName, NewName);
end;

procedure RemoveIDENotifier;
var
  Services : IOTAServices;
begin
  if IDENotifierIndex > -1 then
  begin
    Services := BorlandIDEServices as IOTAServices;
    Assert(Assigned(Services), 'IOTAServices not available');
    Services.RemoveNotifier(IDENotifierIndex);
    IDENotifierIndex := -1;
  end;
end;

procedure FinalizeNotifiers;
var
  I, Index : Integer;
  ModServices : IOTAModuleServices;
  Module : IOTAModule;
begin
  if not Assigned(NotifierList) then Exit;
  ModServices := BorlandIDEServices as IOTAModuleServices;

  try
    Assert(Assigned(ModServices), 'IOTAModuleServices not available');

    for I := 0 to NotifierList.Count -1 do
    begin
      Index := Integer(NotifierList.Objects[I]);
      Module := ModServices.FindModule(NotifierList[I]);
      if Assigned(Module) then
      begin
        Module.RemoveNotifier(Index);
      end;
    end;
  finally
    FreeAndNil(NotifierList);
  end;
end; 

procedure TProjectNotifier.SetModule(Index: Integer; const Value: IOTAModule);
begin
  FModules[Index] := Value;
end;

procedure TProjectNotifier.SetModuleCount(const Value: Integer);
begin
  FModuleCount := Value;
  SetLength(FModules, FModuleCount);
end;

procedure TProjectNotifier.ModuleAdded(const AFileName: string);
var
  ModInfo: IOTAModuleInfo;
  Module: IOTAModule;
begin
  {
    After adding the module, register a notifier to check for changes on the file
    and be able to ask if the file should be added to the SVN.
  }
  ModInfo := FProject.FindModuleInfo(AFileName);
  if (ModInfo <> nil) then
  begin
    Module := ModInfo.OpenModule;
    TModuleNotifier.Create(AFileName, Module);
  end;
end;

procedure TProjectNotifier.ModuleRemoved(const AFileName: string);
var
  Cmd: string;
begin
  if (MessageDlg(Format(GetString(29), [ExtractFileName(AFileName)]), mtConfirmation, [mbYes,mbNo], 0) <> mrYes) then
    Exit;

  Cmd := '/command:remove /notempfile /path:' + AnsiQuotedStr(GetFilesForCmd(FProject, AFileName), '"');

  TTortoiseSVN.TSVNExec(Cmd);
end;

{ TModuleNotifier }

procedure TModuleNotifier.AfterSave;
var
  I: Integer;
  Cmd: string;
begin
  if (MessageDlg(Format(GetString(28), [ExtractFileName(_Filename)]), mtConfirmation, [mbYes,mbNo], 0) <> mrYes) then
  begin
    {
      Always remove notifier after asking for adding the file and don't ask
      every time a file is saved.
    }
    _Module.RemoveNotifier(_Notifier);

    Exit;
  end;

  GetModuleFiles(_FileList, _Module);

  Cmd := '';
  for I := 0 to _FileList.Count - 1 do
  begin
    Cmd := Cmd + _FileList[I];
    if (I < _FileList.Count - 1) then
      Cmd := Cmd + '*';
  end;

  Cmd := '/command:add /notempfile /path:' + AnsiQuotedStr(Cmd, '"');

  TTortoiseSVN.TSVNExec(Cmd);

  _Module.RemoveNotifier(_Notifier);
  _Notifier := -1;
end;

constructor TModuleNotifier.Create(Filename: string; Module: IOTAModule);
begin
  inherited Create;

  _Filename := Filename;
  _Module := Module;
  _Notifier := _Module.AddNotifier(Self);
  _FileList := TStringList.Create;
end;

destructor TModuleNotifier.Destroy;
begin
  try
    _FileList.Free;
  except
  end;

  try
    if (_Notifier <> -1) then
    begin
      _Module.RemoveNotifier(_Notifier);
      _Notifier := -1;
    end;
  except
  end;

  inherited Destroy;
end;

{ TProjectMenuTimer }

constructor TProjectMenuTimer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Self.OnTimer := Self.TimerTick;
  Self.Interval := 1;
  Self.Enabled := True;
end;

procedure TProjectMenuTimer.TimerTick(Sender: TObject);
var
  I: Integer;
  Item: TMenuItem;
begin
  Self.Enabled := False;

  if (Owner is TMenuItem) then
  begin
    for I := 0 to TMenuItem(Owner).Count - 1 do
    begin
      Item := TMenuItem(Owner).Items[I];
      if (Item.Tag = SVN_DIFF) or
         (Item.Tag = SVN_LOG_FILE) or
         (Item.Tag = SVN_EDIT_CONFLICT) or
         (Item.Tag = SVN_CONFLICT_OK) then
      begin
        if (Item.ImageIndex = -1) and
           (Item.GetImageList <> nil) then
        begin
          if (ImgIdx[Item.Tag] = -1) then
            ImgIdx[Item.Tag] := Item.GetImageList.AddMasked(Bitmaps[Item.Tag], clBlack);
          Item.ImageIndex := ImgIdx[Item.Tag];
        end;
      end;
    end;
  end;

  TortoiseSVN.ProjectMenuTimer.Free;
  TortoiseSVN.ProjectMenuTimer := nil;
end;

initialization
  NotifierList := TStringList.Create;
  NotifierList.Sorted := True;

finalization
  try
    if (MenuCreatorNotifier <> -1) then
    begin
      (BorlandIDEServices as IOTAProjectManager).RemoveMenuCreatorNotifier(MenuCreatorNotifier);
    end;
  except
  end;

  try
    RemoveIDENotifier;
  except
  end;

  try
    FinalizeNotifiers;
  except
  end;

  try
    if (EditPopup <> nil) and
       (EditMenuItem <> nil) and
       (EditPopup.Items.IndexOf(EditMenuItem) > -1) then
    begin
      EditPopup.Items.Remove(EditMenuItem);
      EditPopup := nil;
    end;
  except
  end;

  try
    if (EditMenuItem <> nil) then
    begin
      EditMenuItem.Free;
      EditMenuItem := nil;
    end;
  except
  end;

end.

