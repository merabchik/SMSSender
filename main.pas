unit main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  System.Messaging,
  Androidapi.JNI,
  Androidapi.JNIBridge,
  Androidapi.JNI.Os,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.WebKit,
  FMX.Helpers.Android,
  Androidapi.JNI.Net,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Telephony,
  Androidapi.JNI.App,
  Androidapi.Helpers,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.ScrollBox,
  FMX.Memo,
  IOUtils, FMX.Advertising, FMX.MultiView, FMX.Edit, System.ImageList, FMX.ImgList, FMX.ListView.Types, FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base, FMX.ListView, FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS,
  FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, FireDAC.Stan.Async, FireDAC.DApt, Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client, Data.Bind.EngExt, FMX.Bind.DBEngExt,
  System.Rtti, System.Bindings.Outputs, FMX.Bind.Editors, Data.Bind.Components, Data.Bind.DBScope;

type
  TPermissionStatus = record

  end;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Button2: TButton;
    Panel1: TPanel;
    Label1: TLabel;
    BannerAd1: TBannerAd;
    MultiView: TMultiView;
    EditNumber: TEdit;
    Panel2: TPanel;
    ImageList1: TImageList;
    SpeedButtonSaveNumber: TSpeedButton;
    ListViewNumbersSidebar: TListView;
    FDTableNumbers: TFDTable;
    FDTableNumbersid: TFDAutoIncField;
    FDTableNumberstype_id: TIntegerField;
    FDTableNumbersnumber: TIntegerField;
    FDTableNumberssent_cnt: TIntegerField;
    BindSourceDB1: TBindSourceDB;
    BindingsList1: TBindingsList;
    LinkListControlToField1: TLinkListControlToField;
    SidebarButton: TButton;
    procedure Button2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SpeedButtonSaveNumberClick(Sender: TObject);
    procedure ListViewNumbersSidebarDeleteItem(Sender: TObject; AIndex: Integer);
    procedure FormCreate(Sender: TObject);
  private
    procedure SendSMS(target, messagestr: string);
    procedure Split(Delimiter: Char; Str: string; ListOfStrings: TStrings);
    procedure doSMSPermission;
    procedure SMSPermissionRequestResult(ARequestCode: Integer; APermissions: TJavaObjectArray<JString>; AGrantResults: TJavaArray<Integer>);
    { Private declarations }
  public
    { Public declarations }
    SMSPermissionGranted: Integer;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses DataModule, HelperUnit

{$IFDEF ANDROID}
    ,
  Androidapi.JNI.Widget,
{$ENDIF}
  FMX.DialogService;

{$IFDEF ANDROID}

procedure Toast(const Msg: string; Duration: Integer);
begin
  CallInUiThread(
    procedure
    begin
      TJToast.JavaClass.makeText(TAndroidHelper.Context, StrToJCharSequence(Msg), Duration).show
    end);
end;
{$ENDIF}

procedure TForm1.FormCreate(Sender: TObject);
begin
  SMSPermissionGranted := 22;
  // TMessageManager.DefaultManager.SubscribeToMessage(self.SMSPermissionRequestResult, self.doSMSPermission);
end;

procedure TForm1.doSMSPermission;
begin
  if TAndroidHelper.Context.checkSelfPermission(StringToJString('android.permission.SEND_SMS')) = TJPackageManager.JavaClass.PERMISSION_DENIED then
  begin
    SMSPermissionGranted := 0;
    TAndroidHelper.Activity.requestPermissions(CreateJavaStringArray(['android.permission.SEND_SMS']), 5);
  end
  else
    SMSPermissionGranted := 1;
end;

procedure TForm1.SMSPermissionRequestResult(ARequestCode: Integer; APermissions: TJavaObjectArray<JString>; AGrantResults: TJavaArray<Integer>);
begin
  if (AGrantResults.Length > 0) and (AGrantResults[0] = TJPackageManager.JavaClass.PERMISSION_GRANTED) then
    Toast('User granted permission', TJToast.JavaClass.LENGTH_SHORT)
  else
    Toast('User denied permission', TJToast.JavaClass.LENGTH_SHORT);
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  OutPutList, numbers: TStringList;
  I: Integer;
  filename: string;

begin
  self.doSMSPermission;
  if SMSPermissionGranted = 1 then
  begin
    filename := TPath.GetPublicPath + PathDelim + 'numbers.txt';
    // numbers := '593004003,599200652';
    // ShowMessage(filename);
    // exit;
    OutPutList := TStringList.create;
    numbers := TStringList.create;
    try
      numbers.LoadFromFile(filename);
      Split(',', numbers.Text, OutPutList);
      for I := 0 to OutPutList.Count - 1 do
      begin
        self.SendSMS(OutPutList[I], Memo1.Text);
        Label1.Text := 'Sended SMS: ' + IntToStr(I + 1);
      end;
    Except
      ShowMessage('Please read app description on play.google.com/store/apps/details?id=com.mchikvaidze.SMSSender');
    end;
    numbers.Free;
    OutPutList.Free;
  end;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  self.BannerAd1.AdUnitID := 'ca-app-pub-6537744019921634/4591765968';
  self.BannerAd1.LoadAd;
end;

procedure TForm1.ListViewNumbersSidebarDeleteItem(Sender: TObject; AIndex: Integer);
begin
  FDTableNumbers.Delete;
  FDTableNumbers.Refresh;
end;

procedure TForm1.SendSMS(target, messagestr: string);
var
  smsManager: JSmsManager;
  smsTo: JString;
begin
  smsManager := TJSmsManager.JavaClass.getDefault;
  smsTo := StringToJString(target);
  smsManager.sendTextMessage(smsTo, nil, StringToJString(messagestr), nil, nil);
end;

procedure TForm1.SpeedButtonSaveNumberClick(Sender: TObject);
begin

  if not EditNumber.Text.IsEmpty then
  begin
    SpeedButtonSaveNumber.ImageIndex := 1;
    with FDTableNumbers do
    begin
      Insert;
      fieldByName('number').AsInteger := EditNumber.Text.ToInteger;
      Post;
    end;
    FDTableNumbers.Refresh;
    SpeedButtonSaveNumber.ImageIndex := 0;
  end
  else
  begin
    ShowMessage('Fill phone number!');
  end;
end;

procedure TForm1.Split(Delimiter: Char; Str: string; ListOfStrings: TStrings);
begin
  ListOfStrings.Clear;
  ListOfStrings.Delimiter := Delimiter;
  ListOfStrings.StrictDelimiter := True; // Requires D2006 or newer.
  ListOfStrings.DelimitedText := Str;
end;

end.
