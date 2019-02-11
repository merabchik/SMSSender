unit HelperUnit;

interface

uses
  FMX.Types, FMX.Controls, System.SysUtils, System.Classes, RegularExpressions,
  IdHTTP, System.IOUtils
{$IFDEF ANDROID}
    , Androidapi.JNI.Speech,
  Androidapi.JNI.Os,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.WebKit,
  FMX.Helpers.Android,
  Androidapi.JNI.Net,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Telephony,
  Androidapi.JNI.App,
  Androidapi.Helpers
{$ENDIF}
{$IFDEF IOS}
    , iOSapi.Foundation, FMX.Helpers.iOS, System.iOS.Sensors
{$ENDIF IOS}
    ;

type
  THelperUnit = class(TObject)
  private
  public
    function ValidEmail(const emailAddress: string): Boolean;
{$IFDEF ANDROID}
    function FetchSms: string;
    procedure AndroidCheckAndRequestSEND_SMS;
    procedure AndroidCheckAndRequestInternetPermission;
    procedure AndroidCheckAndRequestLocationPermission;
    procedure AndroidCheckAndRequestStatePermission;
    procedure AndroidCheckAndRequestStoragePermission;
    procedure AndroidCheckAndRequestGetAccountPermission;
{$ENDIF}
  end;

const
  PERMISSION_REQUEST_CODE: Integer = 123;

implementation

uses DataModule;

function THelperUnit.ValidEmail(const emailAddress: string): Boolean;
var
  RegEx: TRegEx;
begin
  RegEx := TRegEx.Create('^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\.[a-zA-Z0-9-.]*[a-zA-Z0-9]+$');
  Result := RegEx.Match(emailAddress).Success;
end;

{$IFDEF ANDROID}

procedure THelperUnit.AndroidCheckAndRequestLocationPermission;
begin
  // android.permission.ACCESS_FINE_LOCATION
  if TANdroidHelper.Context.checkSelfPermission(StringToJString('android.permission.ACCESS_FINE_LOCATION')) = TJPackageManager.JavaClass.PERMISSION_DENIED
  then
    TANdroidHelper.Activity.requestPermissions(CreateJavaStringArray(['android.permission.ACCESS_FINE_LOCATION']), PERMISSION_REQUEST_CODE);
end;

procedure THelperUnit.AndroidCheckAndRequestInternetPermission;
begin
  // android.permission.INTERNET
  if TANdroidHelper.Context.checkSelfPermission(StringToJString('android.permission.INTERNET')) = TJPackageManager.JavaClass.PERMISSION_DENIED then
    TANdroidHelper.Activity.requestPermissions(CreateJavaStringArray(['android.permission.INTERNET']), PERMISSION_REQUEST_CODE);
end;

procedure THelperUnit.AndroidCheckAndRequestSEND_SMS;
begin
  // android.permission.SEND_SMS
  if TANdroidHelper.Context.checkSelfPermission(StringToJString('android.permission.SEND_SMS')) = TJPackageManager.JavaClass.PERMISSION_DENIED then
    TANdroidHelper.Activity.requestPermissions(CreateJavaStringArray(['android.permission.SEND_SMS']), PERMISSION_REQUEST_CODE);
end;

procedure THelperUnit.AndroidCheckAndRequestStoragePermission;
begin
  // android.permission.READ_EXTERNAL_STORAGE
  if TANdroidHelper.Context.checkSelfPermission(StringToJString('android.permission.READ_EXTERNAL_STORAGE')) = TJPackageManager.JavaClass.PERMISSION_DENIED
  then
    TANdroidHelper.Activity.requestPermissions(CreateJavaStringArray(['android.permission.READ_EXTERNAL_STORAGE']), PERMISSION_REQUEST_CODE);
  // android.permission.WRITE_EXTERNAL_STORAGE
  if TANdroidHelper.Context.checkSelfPermission(StringToJString('android.permission.WRITE_EXTERNAL_STORAGE')) = TJPackageManager.JavaClass.PERMISSION_DENIED
  then
    TANdroidHelper.Activity.requestPermissions(CreateJavaStringArray(['android.permission.WRITE_EXTERNAL_STORAGE']), PERMISSION_REQUEST_CODE);
end;

procedure THelperUnit.AndroidCheckAndRequestGetAccountPermission;
begin
  // android.permission.WRITE_EXTERNAL_STORAGE
  if TANdroidHelper.Context.checkSelfPermission(StringToJString('android.permission.GET_ACCOUNTS')) = TJPackageManager.JavaClass.PERMISSION_DENIED
  then
    TANdroidHelper.Activity.requestPermissions(CreateJavaStringArray(['android.permission.GET_ACCOUNTS']), PERMISSION_REQUEST_CODE);
end;

procedure THelperUnit.AndroidCheckAndRequestStatePermission;
begin
  // android.permission.READ_PHONE_STATE
  if TANdroidHelper.Context.checkSelfPermission(StringToJString('android.permission.READ_PHONE_STATE')) = TJPackageManager.JavaClass.PERMISSION_DENIED
  then
    TANdroidHelper.Activity.requestPermissions(CreateJavaStringArray(['android.permission.READ_PHONE_STATE']), PERMISSION_REQUEST_CODE);
end;

function THelperUnit.FetchSms: string;
var
  cursor: JCursor;
  uri: Jnet_Uri;
  address, person, msgdatesent, protocol, msgread, msgstatus, msgtype, msgreplypathpresent, subject, body, servicecenter, locked: string;
  msgunixtimestampms: int64;
  addressidx, personidx, msgdateidx, msgdatesentidx, protocolidx, msgreadidx, msgstatusidx, msgtypeidx, msgreplypathpresentidx, subjectidx, bodyidx,
    servicecenteridx, lockedidx: Integer;
begin
  uri := StrToJURI('content://sms/inbox');
  cursor := TANdroidHelper.Activity.getContentResolver.query(uri, nil, nil, nil, nil);
  addressidx := cursor.getColumnIndex(StringToJString('address'));
  personidx := cursor.getColumnIndex(StringToJString('person'));
  msgdateidx := cursor.getColumnIndex(StringToJString('date'));
  msgdatesentidx := cursor.getColumnIndex(StringToJString('date_sent'));
  protocolidx := cursor.getColumnIndex(StringToJString('protocol'));
  msgreadidx := cursor.getColumnIndex(StringToJString('read'));
  msgstatusidx := cursor.getColumnIndex(StringToJString('status'));
  msgtypeidx := cursor.getColumnIndex(StringToJString('type'));
  msgreplypathpresentidx := cursor.getColumnIndex(StringToJString('reply_path_present'));
  subjectidx := cursor.getColumnIndex(StringToJString('subject'));
  bodyidx := cursor.getColumnIndex(StringToJString('body'));
  servicecenteridx := cursor.getColumnIndex(StringToJString('service_center'));
  lockedidx := cursor.getColumnIndex(StringToJString('locked'));

  while (cursor.moveToNext) do
  begin
    address := JStringToString(cursor.getString(addressidx));
    person := JStringToString(cursor.getString(personidx));
    msgunixtimestampms := cursor.getLong(msgdateidx);
    msgdatesent := JStringToString(cursor.getString(msgdatesentidx));
    protocol := JStringToString(cursor.getString(protocolidx));
    msgread := JStringToString(cursor.getString(msgreadidx));
    msgstatus := JStringToString(cursor.getString(msgstatusidx));
    msgtype := JStringToString(cursor.getString(msgtypeidx));
    msgreplypathpresent := JStringToString(cursor.getString(msgreplypathpresentidx));
    subject := JStringToString(cursor.getString(subjectidx));
    body := JStringToString(cursor.getString(bodyidx));
    servicecenter := JStringToString(cursor.getString(servicecenteridx));
    locked := JStringToString(cursor.getString(lockedidx));
    Result := IntToStr(trunc(msgunixtimestampms / 1000)) + ' ' + address + ' ' + body;
  end;
end;

{$ENDIF}

end.
