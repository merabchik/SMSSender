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
  IOUtils, FMX.Advertising, FMX.MultiView, FMX.Edit, System.ImageList,
  FMX.ImgList, FMX.ListView.Types, FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base, FMX.ListView,
  System.Permissions;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    ButtonSendSMS: TButton;
    Panel1: TPanel;
    Label1: TLabel;
    BannerAd1: TBannerAd;
    MultiView: TMultiView;
    ImageList1: TImageList;
    ListViewNumbersSidebar: TListView;
    SidebarButton: TButton;
    procedure ButtonSendSMSClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure SendSMS(target, messagestr: string);
    procedure Split(Delimiter: Char; Str: string; ListOfStrings: TStrings);
    procedure doSMSPermission;
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
      TJToast.JavaClass.makeText(TAndroidHelper.Context,
        StrToJCharSequence(Msg), Duration).show
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
  PermissionsService.requestPermissions
    ([JStringToString(TJManifest_permission.JavaClass.SEND_SMS)],
    procedure(const APermissions: TArray<string>;
      const AGrantResults: TArray<TPermissionStatus>)
    begin
      if (Length(AGrantResults) = 1) and
        (AGrantResults[0] = TPermissionStatus.Granted) then
        ButtonSendSMS.Enabled := True
      else
      begin
        ButtonSendSMS.Enabled := False;
        TDialogService.ShowMessage('SMS send permission not granted');
      end;
    end);
end;

procedure TForm1.ButtonSendSMSClick(Sender: TObject);
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
      ShowMessage
        ('Please read app description on play.google.com/store/apps/details?id=com.mchikvaidze.SMSSender');
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

procedure TForm1.SendSMS(target, messagestr: string);
var
  smsManager: JSmsManager;
  smsTo: JString;
begin
  smsManager := TJSmsManager.JavaClass.getDefault;
  smsTo := StringToJString(target);
  smsManager.sendTextMessage(smsTo, nil, StringToJString(messagestr), nil, nil);
end;

procedure TForm1.Split(Delimiter: Char; Str: string; ListOfStrings: TStrings);
begin
  ListOfStrings.Clear;
  ListOfStrings.Delimiter := Delimiter;
  ListOfStrings.StrictDelimiter := True; // Requires D2006 or newer.
  ListOfStrings.DelimitedText := Str;
end;

end.
