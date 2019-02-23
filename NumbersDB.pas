unit NumbersDB;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  FMX.StdCtrls, FMX.Edit, FMX.Controls.Presentation, FMX.ListView,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  FireDAC.Stan.Async, FireDAC.DApt, Data.Bind.EngExt, FMX.Bind.DBEngExt,
  System.Rtti, System.Bindings.Outputs, FMX.Bind.Editors, Data.Bind.Components,
  Data.Bind.DBScope, Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client,
  FMX.Objects, FMX.Ani;

type
  TNumbersDBForm = class(TForm)
    ListViewNumbersSidebar: TListView;
    EditNumber: TEdit;
    SpeedButtonSaveNumber: TSpeedButton;
    FDTableNumbers: TFDTable;
    FDTableNumbersid: TFDAutoIncField;
    FDTableNumberstype_id: TIntegerField;
    FDTableNumbersnumber: TIntegerField;
    FDTableNumberssent_cnt: TIntegerField;
    BindingsList1: TBindingsList;
    LinkListControlToField1: TLinkListControlToField;
    BindSourceDB1: TBindSourceDB;
    RectangleHeader: TRectangle;
    StyleBookNumbersForm: TStyleBook;
    FloatAnimationNumberInserting: TFloatAnimation;
    Button1: TButton;
    procedure SpeedButtonSaveNumberClick(Sender: TObject);
    procedure ListViewNumbersSidebarDeletingItem(Sender: TObject;
      AIndex: Integer; var ACanDelete: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure EditNumberKeyDown(Sender: TObject; var Key: Word;
      var KeyChar: Char; Shift: TShiftState);
    procedure FloatAnimationNumberInsertingFinish(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ListViewNumbersSidebarPullRefresh(Sender: TObject);
  private
    procedure SaveNumber;
    function check4Dublicate(p_number: String): Boolean;
    procedure deleteRecord(p_id: String);
    { Private declarations }
  public
    { Public declarations }
    procedure Activate;
  end;

var
  NumbersDBForm: TNumbersDBForm;

implementation

{$R *.fmx}

uses main, DataModule;

procedure TNumbersDBForm.Activate;
begin
  self.show;
end;

procedure TNumbersDBForm.EditNumberKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if Key = vkReturn then
  begin
    self.SaveNumber;
  end;
end;

procedure TNumbersDBForm.FloatAnimationNumberInsertingFinish(Sender: TObject);
begin
  EditNumber.Text := '';
end;

procedure TNumbersDBForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := TCloseAction.caFree;
end;

procedure TNumbersDBForm.FormCreate(Sender: TObject);
begin
  self.FDTableNumbers.Active := True;
end;

procedure TNumbersDBForm.ListViewNumbersSidebarDeletingItem(Sender: TObject;
  AIndex: Integer; var ACanDelete: Boolean);
var
  vID: String;
begin
  FDTableNumbers.Close;
  vID := TListItemText(ListViewNumbersSidebar.Items[AIndex].View.FindDrawable
    ('textID')).Text;
  self.deleteRecord(vID);
  FDTableNumbers.Active := True;
end;

procedure TNumbersDBForm.ListViewNumbersSidebarPullRefresh(Sender: TObject);
begin
  self.ListViewNumbersSidebar.PullRefreshWait := True;
  self.FDTableNumbers.Refresh;
  self.ListViewNumbersSidebar.PullRefreshWait := False;
end;

procedure TNumbersDBForm.SpeedButtonSaveNumberClick(Sender: TObject);
begin
  self.SaveNumber;
end;

procedure TNumbersDBForm.SaveNumber;
begin
  if not EditNumber.Text.IsEmpty then
  begin
    if self.check4Dublicate(EditNumber.Text) = False then
    begin
      SpeedButtonSaveNumber.ImageIndex := 1;
      with FDTableNumbers do
      begin
        Insert;
        fieldByName('number').AsString := EditNumber.Text;
        fieldByName('sent_cnt').AsInteger := 0;
        Post;
      end;
      FDTableNumbers.Refresh;
      SpeedButtonSaveNumber.ImageIndex := 0;
      FloatAnimationNumberInserting.Start;
    end
    else
    begin
      ShowMessage('This number is already exists in database');
    end;
  end
  else
  begin
    ShowMessage('Fill phone number!');
  end;
end;

procedure TNumbersDBForm.Button1Click(Sender: TObject);
begin
  self.Close;
end;

function TNumbersDBForm.check4Dublicate(p_number: String): Boolean;
begin
  with DM.FDQueryCustom do
  begin
    Close;
    SQL.Clear;
    SQL.Add('select count(1) as c from numbers t where t.number="' +
      p_number + '"');
    Active := True;
    if fieldByName('c').AsInteger > 0 then
      Result := True
    else
      Result := False;
  end;
end;

procedure TNumbersDBForm.deleteRecord(p_id: String);
begin
  with DM.FDQueryCustom do
  begin
    Close;
    SQL.Clear;
    SQL.Add('delete from numbers t where t.id=' + p_id);
    ExecSQL;
  end;
end;

end.
