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
  FMX.Objects, FMX.Ani, FMX.DialogService, System.Permissions,
  Androidapi.JNI.App, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.Os,
  Androidapi.Helpers, Androidapi.JNIBridge, Androidapi.JNI.JavaTypes, Androidapi.JNI.Net, Androidapi.JNI.Provider,
  FMX.AddressBook.Types, FMX.AddressBook;

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
    Button2: TButton;
    AddressBook1: TAddressBook;
    procedure SpeedButtonSaveNumberClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure EditNumberKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure FloatAnimationNumberInsertingFinish(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ListViewNumbersSidebarPullRefresh(Sender: TObject);
    procedure ListViewNumbersSidebarItemClick(const Sender: TObject; const AItem: TListViewItem);
    procedure Button2Click(Sender: TObject);
  private
    procedure SaveNumber(pNumber: String);
    function check4Dublicate(p_number: String): Boolean;
    procedure deleteRecord(p_id: String);
    procedure checkContactsPermission;
    procedure FillContactList;
    { Private declarations }
  public
    { Public declarations }
    lReadContactPermission: Boolean;
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

procedure TNumbersDBForm.EditNumberKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if Key = vkReturn then
  begin
    self.SaveNumber(EditNumber.Text);
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
  self.lReadContactPermission := false;
end;

procedure TNumbersDBForm.ListViewNumbersSidebarItemClick(const Sender: TObject; const AItem: TListViewItem);
var
  vID: String;
  DialogResult: Boolean;
begin
  TDialogService.MessageDialog('Do you want to delete this record?', TMsgDlgType.mtConfirmation,
    [TMsgDlgBtn.mbNo, TMsgDlgBtn.mbYes], TMsgDlgBtn.mbCancel, 0,
    procedure(const AResult: TModalResult)
    begin
      if AResult = mrYes then
      begin
        ListViewNumbersSidebar.BeginUpdate;
        vID := TListItemText(AItem.View.DrawableByName('textID')).Text;
        FDTableNumbers.Close;
        self.deleteRecord(vID);
        FDTableNumbers.Active := True;
        ListViewNumbersSidebar.EndUpdate;
      end;
    end);
end;

procedure TNumbersDBForm.ListViewNumbersSidebarPullRefresh(Sender: TObject);
begin
  self.ListViewNumbersSidebar.PullRefreshWait := True;
  self.FDTableNumbers.Refresh;
  self.ListViewNumbersSidebar.PullRefreshWait := false;
end;

procedure TNumbersDBForm.SpeedButtonSaveNumberClick(Sender: TObject);
begin
  self.SaveNumber(EditNumber.Text);
end;

procedure TNumbersDBForm.SaveNumber(pNumber: String);
begin
  if not pNumber.IsEmpty then
  begin
    if self.check4Dublicate(pNumber) = false then
    begin
      SpeedButtonSaveNumber.ImageIndex := 1;
      with FDTableNumbers do
      begin
        Insert;
        fieldByName('number').AsString := pNumber;
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

procedure TNumbersDBForm.Button2Click(Sender: TObject);
begin
  self.checkContactsPermission;
  if self.lReadContactPermission = True then
  begin
    self.FillContactList;
  end
  else
  begin
    ShowMessage('Without access to read your contact list, app can not import numbers of your contact list');
  end;
end;

procedure TNumbersDBForm.FillContactList;
var
  I: Integer;
  Contacts: TAddressBookContacts;
begin
  Contacts := TAddressBookContacts.Create;
  try
    AddressBook1.AllContacts(AddressBook1.DefaultSource, Contacts);
    for I := 0 to Contacts.Count - 1 do
      self.SaveNumber(Contacts.Items[I].Phones.First.Number)
  finally
    Contacts.Free;
  end;
end;

function TNumbersDBForm.check4Dublicate(p_number: String): Boolean;
begin
  with DM.FDQueryCustom do
  begin
    Close;
    SQL.Clear;
    SQL.Add('select count(1) as c from numbers t where t.number="' + p_number + '"');
    Active := True;
    if fieldByName('c').AsInteger > 0 then
      Result := True
    else
      Result := false;
  end;
end;

procedure TNumbersDBForm.deleteRecord(p_id: String);
begin
  if p_id.Length > 0 then
    with DM.FDQueryCustom do
    begin
      Close;
      SQL.Clear;
      SQL.Add('delete from numbers where id=' + p_id);
      ExecSQL;
    end;
end;

procedure TNumbersDBForm.checkContactsPermission;
begin
  PermissionsService.RequestPermissions([JStringToString(TJManifest_permission.JavaClass.READ_CONTACTS)],
    procedure(const APermissions: TArray<string>; const AGrantResults: TArray<TPermissionStatus>)
    begin
      if (Length(AGrantResults) = 1) and (AGrantResults[0] = TPermissionStatus.Granted) then
        lReadContactPermission := True
      else
      begin
        lReadContactPermission := false;
      end;
    end)
end;

end.
