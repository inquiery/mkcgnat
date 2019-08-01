unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  DividerBevel, character, IniFiles, math;

const
  Err_InvalidIPAddress = 'Endereço de IP (%s) inválido.';

type
  EInvalidIPAddress = class(Exception);

  TAddrOctets = array[0..3] of Byte;

  TAddrRange = class(TCollectionItem)
  private
    FAddrFrom: String;
    FAddrTo: String;
    FAddrFromOctets: TAddrOctets;
    FAddrToOctets: TAddrOctets;
    FAddrFromInt: UInt32;
    FAddrToInt: UInt32;
    FQty: UInt32;
    FMark: String;
    FMarks: TStrings;

    FIndex: Integer;

    procedure SetAddrFrom(Value: String);
    procedure SetAddrTo(Value: String);
    procedure SetMark(Value: String);
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;

    // Seta FIndex para 0, reinicializando o método Pick
    procedure Reset;
    // Retorna um endereço baseado em FIndex e o incrementa.
    // A próxima chamada a "Pick" retornará o próximo endereço.
    function Pick(var PickedAddr: String): Boolean;

    property AddrFrom: String read FAddrFrom write SetAddrFrom;
    property AddrFromOctets: TAddrOctets read FAddrFromOctets;
    property AddrFromInt: UInt32 read FAddrFromInt;
    property AddrTo: String read FAddrTo write SetAddrTo;
    property AddrToOctets: TAddrOctets read FAddrToOctets;
    property AddrToInt: UInt32 read FAddrToInt;
    property Qty: UInt32 read FQty;
    property Mark: String read FMark write SetMark;
    property Marks: TStrings read FMarks;
  end;

  TAddrRanges = class(TCollection)
  private
    FRangeIndex: Integer;
    FPickedFrom: TAddrRange; // the last TAddrRange instance from which the address was picked when "Pick" is called.
    function GetItems(Index: Integer): TAddrRange;
    procedure SetItems(Index: Integer; Value: TAddrRange);
  public
    constructor Create;
    function Add: TAddrRange;
    function AddRange(AFrom, ATo, AMark: String): TAddrRange;
    function GetQty: Integer;

    procedure Reset;
    function Pick(var PickedAddr: String): Boolean;

    procedure LoadFromString(S: String);
    function SaveAsString: String;

    property Items[Index: Integer]: TAddrRange read GetItems write SetItems; default;
    property PickedFrom: TAddrRange read FPickedFrom;
  end;

  { TfrmMain }

  TfrmMain = class(TForm)
    Bt_PublicAdd: TButton;
    Bt_PublicEdit: TButton;
    Bt_PublicRemove: TButton;
    Bt_Generate: TButton;
    Bt_Save: TButton;
    Cb_Lookup: TCheckBox;
    Ed_PublicMark: TEdit;
    Ed_Division: TComboBox;
    DividerBevel1: TDividerBevel;
    DividerBevel2: TDividerBevel;
    Ed_LocalFrom: TEdit;
    Ed_PublicFrom: TEdit;
    Ed_LocalTo: TEdit;
    Ed_PublicTo: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Ed_Ranges: TListView;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Lb_Ports: TLabel;
    Lb_LocalQty: TLabel;
    Lb_PublicNeeded: TLabel;
    Lb_PublicQty: TLabel;
    Lb_PublicCGNATs: TLabel;
    Ed_Script: TMemo;
    Lb_ScriptLines: TLabel;
    SaveDialog: TSaveDialog;
    procedure Bt_PublicAddClick(Sender: TObject);
    procedure Bt_PublicEditClick(Sender: TObject);
    procedure Bt_GenerateClick(Sender: TObject);
    procedure Bt_PublicRemoveClick(Sender: TObject);
    procedure Bt_SaveClick(Sender: TObject);
    procedure Ed_LocalChange(Sender: TObject);
    procedure Ed_RangesClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    Ranges: TAddrRanges;
    function CheckPublic: Boolean;
    procedure Sumerize;
  public
  end;

const
  CPerPublic: array[0..5] of Integer = (4, 8, 16, 32, 64, 128);
  CMask: array[0..5] of Integer = (30, 29, 28, 27, 26, 25);

var
  frmMain: TfrmMain;
  AppDir: String;

implementation

function IsInteger(Value: String): Boolean;
var
  I: Integer;
begin
  Result := Length(Value) > 0;

  for I := 1 to Length(Value) do begin
    if not IsNumber(Value[I]) then begin
      Result := False;
      Exit;
    end;
  end;
end;

function AddressToOctets(Address: String; var Octets: TAddrOctets): Boolean;
var
  OctetN, CharPos: Integer;
  S: String;
begin
  Result := False;

  OctetN := 0;
  CharPos := Pos('.', Address);
  while (CharPos > 0) do begin
    S := Copy(Address, 1, CharPos - 1);
    Delete(Address, 1, CharPos);

    if IsInteger(S) and (OctetN <= 3) then
      Octets[OctetN] := StrToInt(S)
    else
      Exit;

    CharPos := Pos('.', Address);
    if (CharPos = 0) and (Length(Address) > 0) then
      CharPos := Length(Address) + 1;
    Inc(OctetN);
  end;

  Result := (OctetN = 4) and (Address = '');
end;

function OctetsToInt(Octets: TAddrOctets): UInt32;
begin
  Result := (Octets[0] * 16777216) + (Octets[1] * 65536) + (Octets[2] * 256) + Octets[3];
end;

function AddressToInt(Address: String; var Int: UInt32): Boolean;
var
  Octets: TAddrOctets;
begin
  Octets[0] := 0;
  FillChar(Octets, SizeOf(TAddrOctets), 0);
  Result := AddressToOctets(Address, Octets);
  if Result then
    Int := OctetsToInt(Octets);
end;

function IntToAddress(Int: UInt32): String;
begin
  Result := Format('%d.%d.%d.%d', [hi(hi(Int)), lo(hi(Int)), hi(lo(Int)), lo(lo(Int))]);
end;

function IntAddrToIntNetwork(IntAddr: UInt32; Prefix: Integer): UInt32;
begin
  Result := IntAddr and Trunc(intpower(2, 32) - intpower(2, 32 - Prefix));
end;

{ TAddrRange }

constructor TAddrRange.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);

  FIndex := 0;

  FMarks := TStringList.Create;
end;

destructor TAddrRange.Destroy;
begin
  FMarks.Free;
  inherited Destroy;
end;

procedure TAddrRange.SetAddrFrom(Value: String);
begin
  if AddressToOctets(Value, FAddrFromOctets) then begin
    FAddrFrom := Value;
    FAddrFromInt := OctetsToInt(FAddrFromOctets);
    FQty := Abs(FAddrToInt - FAddrFromInt) + 1;
  end
  else
    raise EInvalidIPAddress.CreateFmt(Err_InvalidIPAddress, [Value]);
end;

procedure TAddrRange.SetAddrTo(Value: String);
begin
  if AddressToOctets(Value, FAddrToOctets) then begin
    FAddrTo := Value;
    FAddrToInt := OctetsToInt(FAddrToOctets);
    FQty := Abs(FAddrToInt - FAddrFromInt) + 1;
  end
  else
    raise EInvalidIPAddress.CreateFmt(Err_InvalidIPAddress, [Value]);
end;

procedure TAddrRange.SetMark(Value: String);
var
  CharPos: Integer;
  sMark: String;
begin
  if Value <> FMark then begin
    FMark := Value;

    FMarks.Clear;

    CharPos := Pos(';', Value);
    if CharPos = 0 then
      FMarks.Add(FMark);
    while CharPos > 0 do begin
      sMark := Copy(Value, 1, CharPos - 1);
      FMarks.Add(sMark);
      system.Delete(Value, 1, CharPos);
      CharPos := Pos(';', Value);
      if (CharPos = 0) and (Length(Value) > 0) then
        CharPos := Length(Value) + 1;
    end;
  end;
end;

procedure TAddrRange.Reset;
begin
  FIndex := 0;
end;

function TAddrRange.Pick(var PickedAddr: String): Boolean;
begin
  Result := False;
  PickedAddr := '';

  if FAddrFromInt > FAddrToInt then begin
    if FAddrFromInt - FIndex >= FAddrToInt then
      PickedAddr := IntToAddress(FAddrFromInt - FIndex);
  end
  else begin
    if FAddrFromInt + FIndex <= FAddrToInt then
      PickedAddr := IntToAddress(FAddrFromInt + FIndex);
  end;

  Result := PickedAddr <> '';
  if Result then
    Inc(FIndex);
end;

{ TAddrRanges }

constructor TAddrRanges.Create;
begin
  inherited Create(TAddrRange);

  FRangeIndex := 0;
end;

function TAddrRanges.Add: TAddrRange;
begin
  Result := TAddrRange(inherited Add);
end;

function TAddrRanges.AddRange(AFrom, ATo, AMark: String): TAddrRange;
var
  AFromOctets, AToOctets: TAddrOctets;
  AFromInt, AToInt: UInt32;
begin
  // somente pra suprimir aviso do compilador
  AFromOctets[0] := 0;
  AToOctets[0] := 0;
  // ****

  if AddressToOctets(AFrom, AFromOctets) then
    AFromInt := OctetsToInt(AFromOctets)
  else
    raise EInvalidIPAddress.CreateFmt(Err_InvalidIPAddress, [AFrom]);

  if AddressToOctets(ATo, AToOctets) then
    AToInt := OctetsToInt(AToOctets)
  else
    raise EInvalidIPAddress.CreateFmt(Err_InvalidIPAddress, [ATo]);

  Result := Add;

  Result.FAddrFrom := AFrom;
  Result.FAddrFromOctets := AFromOctets;
  Result.FAddrFromInt := AFromInt;

  Result.FAddrTo := ATo;
  Result.FAddrToOctets := AToOctets;
  Result.FAddrToInt := AToInt;

  Result.FQty := Abs(AToInt - AFromInt) + 1;

  Result.SetMark(AMark);
end;

function TAddrRanges.GetItems(Index: Integer): TAddrRange;
begin
  Result := TAddrRange(inherited GetItem(Index));
end;

procedure TAddrRanges.SetItems(Index: Integer; Value: TAddrRange);
begin
  inherited SetItem(Index, Value);
end;

function TAddrRanges.GetQty: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    Result := Result + Items[I].FQty;
end;

procedure TAddrRanges.LoadFromString(S: String);
var
  CharPos1, CharPos2, CharPos3: Integer;
  Item, sFrom, sTo, sMark: String;
begin
  CharPos1 := Pos(',', S);
  if (CharPos1 = 0) and (Length(S) > 0) then
    CharPos1 := Length(S) + 1;
  while (CharPos1 > 0) do begin
    Item := Copy(S, 1, CharPos1 - 1);
    //S := Copy(S, CharPos1 + 1);
    system.Delete(S, 1, CharPos1);

    CharPos2 := Pos('-', Item);
    if CharPos2 > 0 then begin
      sFrom := Copy(Item, 1, CharPos2 - 1);
      sTo := Copy(Item, CharPos2 + 1);
    end
    else begin
      sFrom := Item;
      sTo := Item;
    end;

    sMark := '';

    CharPos2 := Pos('[', sTo);
    if CharPos2 > 0 then begin
      CharPos3 := Pos(']', sTo);
      if CharPos3 > 0 then
        sMark := Copy(sTo, CharPos2 + 1, CharPos3 - CharPos2 - 1);
      sTo := Copy(sTo, 1, CharPos2 - 1);
    end;

    AddRange(sFrom, sTo, sMark);

    CharPos1 := Pos(',', S);
    if (CharPos1 = 0) and (Length(S) > 0) then
      CharPos1 := Length(S) + 1;
  end;
end;

function TAddrRanges.SaveAsString: String;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Count - 1 do begin
    if I > 0 then
      Result := Result + ',';
    Result := Result +
      Items[I].FAddrFrom + '-' + Items[I].FAddrTo;
    if Items[I].FMark <> '' then
      Result := Result + '[' + Items[I].FMark + ']';
  end;
end;

procedure TAddrRanges.Reset;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].Reset;
  FRangeIndex := 0;
end;

function TAddrRanges.Pick(var PickedAddr: String): Boolean;
  function DoPick(var P: String; ATry: Integer): Boolean;
  begin
    Result := False;
    P := '';
    if Count > 0 then begin
      Result := Items[FRangeIndex].Pick(P);
      if Result then
        FPickedFrom := Items[FRangeIndex];

      if FRangeIndex = Count - 1 then
        FRangeIndex := 0
      else
        Inc(FRangeIndex);

      if not Result and (ATry <= Count) then
        Result := DoPick(P, ATry + 1);
    end;
  end;
begin
  FPickedFrom := nil;
  Result := False;
  PickedAddr := '';
  Result := DoPick(PickedAddr, 0);
end;


{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
var
  Ini: TIniFile;
  I: Integer;
  Item: TListItem;
begin
  AppDir := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName));

  Ranges := TAddrRanges.Create;

  Ini := TIniFile.Create(AppDir + 'cgnat_rules.ini');

  Ed_LocalFrom.Text := Ini.ReadString('Local', 'From', '');
  Ed_LocalTo.Text := Ini.ReadString('Local', 'To', '');
  Ed_Division.ItemIndex := Ed_Division.Items.IndexOf(Ini.ReadString('Local', 'Division', ''));
  if Ed_Division.ItemIndex < 0 then
    Ed_Division.ItemIndex := 3;

  Ranges.LoadFromString(Ini.ReadString('Public', 'Ranges', ''));
  for I := 0 to Ranges.Count - 1 do begin
    Item := Ed_Ranges.Items.Add;
    Item.Data := Ranges[I];
    Item.Caption := Ranges[I].AddrFrom;
    Item.SubItems.Add(Ranges[I].AddrTo);
    Item.SubItems.Add(Ranges[I].Mark);
    Item.SubItems.Add(IntToStr(Ranges[I].Qty));
  end;

  Cb_Lookup.Checked := Ini.ReadBool('General', 'GenerateLookupRules', False);

  Sumerize;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(AppDir + 'cgnat_rules.ini');

  Ini.WriteString('Local', 'From', Ed_LocalFrom.Text);
  Ini.WriteString('Local', 'To', Ed_LocalTo.Text);
  Ini.WriteString('Local', 'Division', Ed_Division.Text);
  Ini.WriteString('Public', 'Ranges', Ranges.SaveAsString);
  Ini.WriteBool('General', 'GenerateLookupRules', Cb_Lookup.Checked);

  Ini.Free;
end;

procedure TfrmMain.Ed_LocalChange(Sender: TObject);
begin
  Sumerize;
end;

procedure TfrmMain.Ed_RangesClick(Sender: TObject);
var
  Range: TAddrRange;
begin
  Bt_PublicEdit.Enabled := Ed_Ranges.Selected <> nil;
  Bt_PublicRemove.Enabled := Ed_Ranges.Selected <> nil;

  if Ed_Ranges.Selected <> nil then begin
    Range := TAddrRange(Ed_Ranges.Selected.Data);
    Ed_PublicFrom.Text := Range.AddrFrom;
    Ed_PublicTo.Text := Range.AddrTo;
    Ed_PublicMark.Text := Range.Mark;
  end;
end;

procedure TfrmMain.Bt_GenerateClick(Sender: TObject);
var
  Picked, chain: String;
  PickedUses, PerPublic, Prefix, PortsPerIP, FirstPort, Port: Integer;
  FromInt, ToInt, LocalI: UInt32;
  StrLookup: TStrings;

  procedure CreateLookup;
  var
    markI: Integer;
  begin
    if Cb_Lookup.Checked then begin
      for markI := 0 to Ranges.PickedFrom.Marks.Count - 1 do
        StrLookup.Add(Format('/ip route rule add src-address=%s/%d table="%s" action=lookup comment="cgnat"', [IntToAddress(IntAddrToIntNetwork(LocalI, Prefix)), Prefix, Ranges.PickedFrom.Marks[markI]]));
    end;
  end;

  procedure WorkAddress;
  var
    LocalIP: String;
  begin
    LocalIP := IntToAddress(LocalI);

    //Ed_Script.Lines.Add(Format('%d: %s %s -> %s [%d-%d]', [PickedUses, IntToAddress(IntAddrToIntNetwork(LocalI, Prefix)) + '/' + IntToStr(Prefix), LocalIP, Picked, Port, Port + PortsPerIP - 1]));

    chain := Format('zz-cgnat%s/%d', [IntToAddress(IntAddrToIntNetwork(LocalI, Prefix)), Prefix]);
    Ed_Script.Lines.Add(Format('/ip firewall nat add chain=%s comment="cgnat" src-address=%s protocol=tcp action=src-nat to-addresses=%s to-ports=%d-%d', [chain, LocalIP, Picked, Port, Port + PortsPerIP - 1]));
    Ed_Script.Lines.Add(Format('/ip firewall nat add chain=%s comment="cgnat" src-address=%s protocol=udp action=src-nat to-addresses=%s to-ports=%d-%d', [chain, LocalIP, Picked, Port, Port + PortsPerIP - 1]));
    Ed_Script.Lines.Add(Format('/ip firewall nat add chain=%s comment="cgnat" src-address=%s protocol=icmp action=src-nat to-addresses=%s', [chain, LocalIP, Picked]));

    if PickedUses < PerPublic then begin
      // if (LocalI = ToInt)  se é a última vez que esse procedure for ser executado, LocalI=ToInt, então, tem que chamar CreateLookup pela última vez
      if (LocalI = ToInt) then
        CreateLookup;

      Port := Port + PortsPerIP;
      Inc(PickedUses);
    end
    else begin
      CreateLookup;

      PickedUses := 1;
      Port := FirstPort;
      Ranges.Pick(Picked);
    end;
  end;
begin
  // para suprimir aviso do compilador
  FromInt := 0;
  ToInt := 0;
  // ****

  if Ed_Division.ItemIndex >= 0 then begin
    PerPublic := CPerPublic[Ed_Division.ItemIndex];
    Prefix := CMask[Ed_Division.ItemIndex];
    PortsPerIP := (65536-1025) div PerPublic;
    FirstPort := 65536 - (PortsPerIP * PerPublic);

    if not AddressToInt(Ed_LocalFrom.Text, FromInt) then
      MessageDlg('Atenção', 'Endereço privado inicial inválido.', mtWarning, [mbOk], 0)
    else if not AddressToInt(Ed_LocalTo.Text, ToInt) then
      MessageDlg('Atenção', 'Endereço privado final inválido.', mtWarning, [mbOk], 0)
    else if Ranges.GetQty * PerPublic < ceil((Abs(ToInt - FromInt) + 1) / PerPublic) then
      MessageDlg('Atenção', 'A quantidade de IPs públicos nos ranges definidos não é suficiente para todos os endereços no range de IPs privados.', mtWarning, [mbOk], 0)
    else begin
      Picked := '';

      Ranges.Reset;
      StrLookup := TStringList.Create;
      Ed_Script.Lines.BeginUpdate;
      try
        Ed_Script.Lines.Clear;
        Ed_Script.Lines.Add('/ip firewall nat remove [find where comment="cgnat"]');
        Ed_Script.Lines.Add('/ip route rule remove [find where comment="cgnat"]');
        Ed_Script.Lines.Add(Format('/ip firewall nat add chain=srcnat comment="cgnat" src-address=%s-%s action=jump jump-target="zz-cgnat"', [IntToAddress(FromInt), IntToAddress(ToInt)]));

        Ranges.Pick(Picked);
        PickedUses := 1;
        Port := FirstPort;

        if ToInt < FromInt then begin
          LocalI := FromInt;
          while LocalI >= ToInt do begin
            Ed_Script.Lines.Add(Format('/ip firewall nat add chain="zz-cgnat" comment="cgnat" src-address=%s/%d action=jump jump-target="zz-cgnat%s/%d"', [IntToAddress(LocalI), Prefix, IntToAddress(LocalI), Prefix]));
            Dec(LocalI, PerPublic);
          end;
          for LocalI := FromInt downto ToInt do
            WorkAddress
        end
        else begin
          LocalI := FromInt;
          while LocalI <= ToInt do begin
            Ed_Script.Lines.Add(Format('/ip firewall nat add chain="zz-cgnat" comment="cgnat" src-address=%s/%d action=jump jump-target="zz-cgnat%s/%d"', [IntToAddress(LocalI), Prefix, IntToAddress(LocalI), Prefix]));
            Inc(LocalI, PerPublic);
          end;
          for LocalI := FromInt to ToInt do
            WorkAddress;
        end;

        Ed_Script.Lines.AddStrings(StrLookup);
      finally
        Ed_Script.Lines.EndUpdate;
        StrLookup.Free;
      end;

      Lb_ScriptLines.Caption := FormatFloat(',#0', Ed_Script.Lines.Count);
    end;
  end
  else
    MessageDlg('Atenção', 'Selecione a razão de divisão de endereços para gerar.', mtWarning, [mbOk], 0)
end;

procedure TfrmMain.Bt_PublicRemoveClick(Sender: TObject);
begin
  if Ed_Ranges.Selected <> nil then begin
    TAddrRange(Ed_Ranges.Selected.Data).Free;
    Ed_Ranges.Selected.Delete;

    Bt_PublicEdit.Enabled := False;
    Bt_PublicRemove.Enabled := False;

    Sumerize;
  end;
end;

procedure TfrmMain.Bt_SaveClick(Sender: TObject);
var
  FileName: String;
begin
  if SaveDialog.Execute then begin
    FileName := SaveDialog.FileName;;
    if (Pos('.', ExtractFileName(FileName)) = 0) and (SaveDialog.FilterIndex = 0) then
      FileName := FileName + '.rsc';

    if not FileExists(FileName) or (MessageDlg('Salvar', 'Arquivo de destino já existente. Sobreescrever?', mtWarning, [mbYes, mbNo], 0) = mrYes) then begin
      Ed_Script.Lines.SaveToFile(FileName);
    end;
  end;
end;

function TfrmMain.CheckPublic: Boolean;
var
  Octets: TAddrOctets;
begin
  Octets[0] := 0; // <-- suprimir aviso do compilador

  Result :=
    AddressToOctets(Ed_PublicFrom.Text, Octets) and
    AddressToOctets(Ed_PublicTo.Text, Octets);
end;

procedure TfrmMain.Bt_PublicEditClick(Sender: TObject);
var
  Range: TAddrRange;
begin
  if (Ed_Ranges.Selected <> nil) and CheckPublic then begin
    Range := TAddrRange(Ed_Ranges.Selected.Data);
    Range.AddrFrom := Ed_PublicFrom.Text;
    Range.AddrTo := Ed_PublicTo.Text;
    Range.Mark := Ed_PublicMark.Text;
    Ed_Ranges.Selected.Caption := Range.AddrFrom;
    Ed_Ranges.Selected.SubItems[0] := Range.AddrTo;
    Ed_Ranges.Selected.SubItems[1] := Range.Mark;
    Ed_Ranges.Selected.SubItems[2] := IntToStr(Range.Qty);

    Sumerize;
  end;
end;

procedure TfrmMain.Bt_PublicAddClick(Sender: TObject);
var
  Range: TAddrRange;
  Item: TListItem;
begin
  if CheckPublic then begin
    Range := Ranges.AddRange(Ed_PublicFrom.Text, Ed_PublicTo.Text, Ed_PublicMark.Text);

    Item := Ed_Ranges.Items.Add;
    Item.Data := Range;
    Item.Caption := Range.AddrFrom;
    Item.SubItems.Add(Range.AddrTo);
    Item.SubItems.Add(Range.Mark);
    Item.SubItems.Add(IntToStr(Range.Qty));

    Sumerize;
  end;
end;

procedure TfrmMain.Sumerize;
var
  FromInt, ToInt: UInt32;
  LocalQty, PerPublic: Integer;
begin
  if Ed_Division.ItemIndex >= 0 then
    PerPublic := CPerPublic[Ed_Division.ItemIndex]
  else
    PerPublic := 0;

  FromInt := 0;
  ToInt := 0;
  if AddressToInt(Ed_LocalFrom.Text, FromInt) and AddressToInt(Ed_LocalTo.Text, ToInt) then begin
    LocalQty := Abs(ToInt - FromInt) + 1;
    Lb_LocalQty.Caption := FormatFloat(',#0', LocalQty);
    if PerPublic > 0 then begin
      Lb_PublicNeeded.Caption := FormatFloat(',#0', ceil(LocalQty / PerPublic));
      Lb_Ports.Caption := FormatFloat(',#0', (65536-1025) div PerPublic);
    end
    else begin
      Lb_PublicNeeded.Caption := '';
      Lb_Ports.Caption := '';
    end;
  end;

  Lb_PublicQty.Caption := FormatFloat(',#0', Ranges.GetQty);
  if PerPublic > 0 then
    Lb_PublicCGNATs.Caption := FormatFloat(',#0', Ranges.GetQty * PerPublic)
  else
    Lb_PublicCGNATs.Caption := '';
end;

end.
