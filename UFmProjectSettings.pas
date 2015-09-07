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

uses UHelperFunctions, StrUtils;

{ Maybe helpful?
function PathIsRelativeA(lpszPath: PAnsiChar): BOOL; stdcall; external 'shlwapi.dll' name 'PathIsRelativeA';
function PathIsRelativeW(lpszPath: PWideChar): BOOL; stdcall; external 'shlwapi.dll' name 'PathIsRelativeW';
}

{$R *.dfm}

procedure TFmProjectSettings.BtnAddClick(Sender: TObject);
var
  ToAdd, ToAddRel: string;
  Answer: Integer;
begin
  if (OpenDialog.Execute) then
  begin
    ToAdd := ExtractFilePath(OpenDialog.FileName);

    // If the path is the project' (sub-)directory than it's already in the list!
    if (StartsStr(ExtractFilePath(_Project.FileName), ToAdd)) then
    begin
      MessageDlg(GetString(38), mtWarning, [mbOk], 0);
      Exit;
    end;

    // Make the path a relative one, so the structure within the SVN stays intact
    ToAddRel := ExtractRelativePath(ExtractFilePath(_Project.FileName), ToAdd);

    {
      Check if the path was not added. Check the relative and the absolute one
      to not add a directory twice.
    }
    if (LbDirectories.Items.IndexOf(ToAdd) = -1) and (LbDirectories.Items.IndexOf(ToAddRel) = -1) then
    begin
      // If the path' differ, than it is a relative one...
      if (ToAddRel <> ToAdd) then
      begin
        // Ask if the relative path should be chosen
        Answer := MessageDlg(Format(GetString(37), [ToAddRel]),
                   mtConfirmation, [mbYes, mbNo, mbCancel], 0);

        case Answer of
          mrYes: LbDirectories.Items.Add(ToAddRel);
          mrNo:  LbDirectories.Items.Add(ToAdd);
        end;
      end else
      begin
        // ...otherwise just add the absolute one
        LbDirectories.Items.Add(ToAdd);
      end;
    end;
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
