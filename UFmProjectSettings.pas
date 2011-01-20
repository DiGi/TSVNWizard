unit UFmProjectSettings;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ToolsAPI;

type
  TFmProjectSettings = class(TForm)
    LbDirectories: TListBox;
    LblDirectories: TLabel;
    BtnAdd: TButton;
    BtnRemove: TButton;
    BtnOk: TButton;
    BtnCancel: TButton;
    OpenDialog: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure BtnRemoveClick(Sender: TObject);
    procedure BtnAddClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnOkClick(Sender: TObject);
  private
    { Private-Deklarationen }
    _Project: IOTAProject;
    procedure SetProject(const Value: IOTAProject);
  public
    { Public-Deklarationen }
    property Project: IOTAProject read _Project write SetProject;
  end;

var
  FmProjectSettings: TFmProjectSettings;

implementation

uses UHelperFunctions;

{$R *.dfm}

procedure TFmProjectSettings.BtnAddClick(Sender: TObject);
var
  ToAdd: string;
begin
  if (OpenDialog.Execute) then
  begin
    ToAdd := ExtractFilePath(OpenDialog.FileName);
    if (LbDirectories.Items.IndexOf(ToAdd) = -1) then
      LbDirectories.Items.Add(ToAdd);
  end;
end;

procedure TFmProjectSettings.BtnCancelClick(Sender: TObject);
begin
  Self.Close;
end;

procedure TFmProjectSettings.BtnOkClick(Sender: TObject);
begin
  SetDirectoriesToTSVN(_Project, LbDirectories.Items);
  
  Self.Close;
end;

procedure TFmProjectSettings.BtnRemoveClick(Sender: TObject);
begin
  if (LbDirectories.ItemIndex > -1) then
  begin
    LbDirectories.Items.Delete(LbDirectories.ItemIndex);
  end;
end;

procedure TFmProjectSettings.FormCreate(Sender: TObject);
begin
  Self.Caption           := GetString(31);
  LblDirectories.Caption := GetString(32);
  BtnOk.Caption          := GetString(33);
  BtnCancel.Caption      := GetString(34);
  OpenDialog.Title       := GetString(35);
  OpenDialog.FileName    := GetString(36);
end;

procedure TFmProjectSettings.SetProject(const Value: IOTAProject);
begin
  _Project := Value;

  LbDirectories.Items.Clear;
  LbDirectories.Items.AddStrings(GetDirectoriesFromTSVN(_Project));
end;

end.
