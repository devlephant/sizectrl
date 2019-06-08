unit main;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface
{$IFDEF VER80}
  {$DEFINE VER3D}
{$ENDIF}
{$IFDEF VER90}
  {$DEFINE VER3D}
{$ENDIF}
{$IFDEF VER100}
  {$DEFINE VER3D}
{$ENDIF}
{$IFNDEF VER80} {$IFNDEF VER90} {$DEFINE VER3U} {$ENDIF} {$ENDIF}
{$IFDEF VER3U} {$IFNDEF VER100} {$DEFINE VER3UP} {$ENDIF} {$ENDIF}
uses
{$IFnDEF FPC}
  Windows,
  Messages,
{$ELSE}
  LCLIntf, LCLType,
{$ENDIF}
SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ComCtrls, StdCtrls, Menus,
  {$IFNDEF Linux}
  CommCtrl,
  {$ENDIF}
  imageselect,
  {$IFnDEF FPC}
  Vcl.Buttons,
  {$ELSE}
  Buttons,
{$ENDIF}
  {$IFDEF VER3UP} {$IFnDEF FPC}UITypes, {$ENDIF} Types, {$ENDIF}
 {$IFDEF FPC}Spin{$ELSE} Vcl.Samples.Spin{$ENDIF},
 SizeControl;

type

  { TMainForm }

  TMainForm = class(TForm)
    ComboBox3: TComboBox;
    Label38: TLabel;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    EnableSizeControl1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    StatusBar1: TStatusBar;
    sizePops: TPopupMenu;
    MenuItem1: TMenuItem;
    sets: TScrollBox;
    Panel6: TPanel;
    Label7: TLabel;
    SpinEdit1: TSpinEdit;
    Panel7: TPanel;
    Label8: TLabel;
    ComboBox2: TComboBox;
    Panel9: TPanel;
    Label12: TLabel;
    SpinEdit4: TSpinEdit;
    Panel10: TPanel;
    Label13: TLabel;
    SpinEdit5: TSpinEdit;
    Panel11: TPanel;
    Label14: TLabel;
    BitBtn8: TBitBtn;
    Panel12: TPanel;
    Label15: TLabel;
    BitBtn7: TBitBtn;
    Panel13: TPanel;
    Label16: TLabel;
    SpinEdit3: TSpinEdit;
    Panel15: TPanel;
    Label18: TLabel;
    BitBtn6: TBitBtn;
    Panel16: TPanel;
    Label19: TLabel;
    BitBtn5: TBitBtn;
    Panel17: TPanel;
    Label20: TLabel;
    BitBtn4: TBitBtn;
    Panel18: TPanel;
    Label21: TLabel;
    BitBtn3: TBitBtn;
    Panel19: TPanel;
    Label22: TLabel;
    BitBtn2: TBitBtn;
    Panel20: TPanel;
    Label23: TLabel;
    BitBtn1: TBitBtn;
    Panel21: TPanel;
    Label24: TLabel;
    SpinEdit2: TSpinEdit;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    cbSizeMove: TCheckBox;
    Panel3: TPanel;
    Panel1: TPanel;
    Memo1: TMemo;
    Label1: TLabel;
    Panel2: TPanel;
    Label2: TLabel;
    Label3: TLabel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Edit1: TEdit;
    RadioGroup1: TRadioGroup;
    CheckBox1: TCheckBox;
    Label4: TLabel;
    CheckBox5: TCheckBox;
    Panel4: TPanel;
    ListView1: TListView;
    Button1: TButton;
    Panel5: TPanel;
    Label5: TLabel;
    ComboBox1: TComboBox;
    ColorDialog1: TColorDialog;
    Panel8: TPanel;
    lap8: TLabel;
    ComboBox4: TComboBox;
    Panel14: TPanel;
    Label6: TLabel;
    Panel22: TPanel;
    Label9: TLabel;
    Panel23: TPanel;
    Label10: TLabel;
    Panel24: TPanel;
    Label11: TLabel;
    Panel25: TPanel;
    Label17: TLabel;
    BitBtn9: TBitBtn;
    BitBtn10: TBitBtn;
    ComboBox5: TComboBox;
    ComboBox6: TComboBox;
    ComboBox7: TComboBox;
    Panel26: TPanel;
    Label25: TLabel;
    ComboBox8: TComboBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    Label26: TLabel;
    Panel27: TPanel;
    Label27: TLabel;
    SpinEdit6: TSpinEdit;
    Panel28: TPanel;
    Label28: TLabel;
    SpinEdit7: TSpinEdit;
    Label29: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Panel29: TPanel;
    Label32: TLabel;
    BitBtn11: TBitBtn;
    Panel30: TPanel;
    Label33: TLabel;
    BitBtn12: TBitBtn;
    Panel31: TPanel;
    Label34: TLabel;
    BitBtn13: TBitBtn;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Label35: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    Button5: TButton;
    CheckBox8: TCheckBox;
    procedure Button5Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure EnableSizeControl1Click(Sender: TObject);
    procedure cbSizeMoveClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure sizePopsPopup(Sender: TObject);
    procedure FormClick(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
    procedure SpinEdit2Change(Sender: TObject);
    procedure SpinEdit3Change(Sender: TObject);
    procedure SpinEdit4Change(Sender: TObject);
    procedure SpinEdit5Change(Sender: TObject);
    procedure Panel3Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure BitBtn7Click(Sender: TObject);
    procedure BitBtn8Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure BitBtn6Click(Sender: TObject);
    procedure BitBtn10Click(Sender: TObject);
    procedure BitBtn9Click(Sender: TObject);
    procedure ComboBox5Change(Sender: TObject);
    procedure ComboBox6Change(Sender: TObject);
    procedure ComboBox7Change(Sender: TObject);
    procedure ComboBox4Change(Sender: TObject);
    procedure ComboBox8Change(Sender: TObject);
    procedure CheckBox6Click(Sender: TObject);
    procedure CheckBox7Click(Sender: TObject);
    procedure Label26MouseEnter(Sender: TObject);
    procedure Label26MouseLeave(Sender: TObject);
    procedure SpinEdit6Change(Sender: TObject);
    procedure SpinEdit7Change(Sender: TObject);
    procedure Panel3Resize(Sender: TObject);
    procedure BitBtn11Click(Sender: TObject);
    procedure BitBtn13Click(Sender: TObject);
    procedure BitBtn12Click(Sender: TObject);
    procedure CheckBox8Click(Sender: TObject);
  private
    SizeCtrl: TSizeCtrl;
    procedure SizeCtrlDuring(Sender: TObject; dx, dy: integer; State: TSCState);
    {$IFDEF FPC}
    procedure SizeCtrlEnd(Sender: TObject; {%H-}State: TSCState);
    {$ELSE}
    procedure SizeCtrlEnd(Sender: TObject; State: TSCState);
    {$ENDIF}
    procedure SizeCtrlTargetChange(Sender: TObject);
    procedure SizeCtrlMouseDown(Sender: TObject;
      Target: TControl; TargetPt: TPoint; var handled: boolean);
    procedure SizeCtrlSetCursor(Sender: TObject;
      Target: TControl; TargetPt: TPoint; var handled: boolean);
    {$IFDEF FPC}
     procedure SizeCtrlKeyDown(Sender: TObject; var Key: Word;
      {%H-}Shift: TShiftState);
    {$ELSE}
    procedure SizeCtrlKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    {$ENDIF}
  protected
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;
  pv: boolean = true;
  GRIDSIZE: integer = 8; //try changing this too.

  //PanelSizeCaption
const
  pCollapsed: string = 'Show Properties';
  pExpanded: string = 'Hide';
implementation
{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

//RegComponents: A simple recursive procedure which registers with SizeCtrl1
//all the visible controls contained by aParent except 'tagged' controls ...
procedure RegComponents(aParent: TWinControl; SizeCtrl: TSizeCtrl);
var
  i: integer;
begin
  for i := 0 to aParent.ControlCount -1 do
  begin
    //In this demo, Tag = 1 prevents a control becoming a SizeCtrl target ...
    if aParent.Controls[i].Tag <> 1 then
      SizeCtrl.RegisterControl(aParent.Controls[i]);
    if aParent.Controls[i] is TWinControl then
      RegComponents(TWinControl(aParent.Controls[i]), SizeCtrl);
  end;
end;
//------------------------------------------------------------------------------

procedure UnregComponents(aParent: TWinControl; SizeCtrl: TSizeCtrl);
var
  i: integer;
begin

  for i := 0 to aParent.ControlCount -1 do
  begin
    SizeCtrl.UnRegisterControl(aParent.Controls[i]);
    if aParent.Controls[i] is TWinControl then
      UnregComponents(TWinControl(aParent.Controls[i]), SizeCtrl);
  end;
end;
//------------------------------------------------------------------------------

function selectColor(aColor: TColor): TColor;
begin
   MainForm.ColorDialog1.Color := aColor;
   if MainForm.ColorDialog1.Execute then
    Result := MainForm.ColorDialog1.Color
   else Result := aColor;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  //All of this would normally be done in the IDE's Object Inspector
  //if SizeCtrl was installed into the IDE.
  SizeCtrl := TSizeCtrl.Create(Panel1);
  SizeCtrl.HoverBtnColor := clYellow;
  SizeCtrl.HoverBtnFrameColor := clNavy;
  SizeCtrl.SelectionKey := VK_CONTROL;
  SizeCtrl.movePanelCanvas.Brush.Style := bsClear;
  SizeCtrl.OnTargetChange := SizeCtrlTargetChange;
  SizeCtrl.OnDuringSizeMove := SizeCtrlDuring;
  SizeCtrl.OnEndSizeMove := SizeCtrlEnd;
  SizeCtrl.GridSize := GRIDSIZE;
  SizeCtrl.PopupMenu := sizePops;
  //to override behaviour of Pagecontrols so new pages can be selected ...
  SizeCtrl.OnMouseDown := SizeCtrlMouseDown;
  SizeCtrl.OnSetCursor := SizeCtrlSetCursor;
  SizeCtrl.OnKeyDown := SizeCtrlKeyDown;

  RegComponents(Panel1, SizeCtrl);
  SizeCtrl.Enabled := true;
  //ALSO, TRY OUT TSizeCtrl.movePanelCanvas
  //IT GIVES ACCES TO THE SIZING FRAME STYLE!!!
end;

procedure TMainForm.Button5Click(Sender: TObject);
var f: TFOrm;
begin
    f := TForm.Create(Panel1);
    f.Parent := Panel1;
    f.show;
    SIZEctrl.RegisterControl(f);

end;

//------------------------------------------------------------------------------

procedure TMainForm.FormClick(Sender: TObject);
begin
  SizeCtrl.UnRegisterAll;
  RegComponents(TWinControl(Sender), SizeCtrl);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  //it's important to disable SizeCtrl before destroying the form
  //because any SizeCtrl registered controls needs to be 'unhooked' ...
  SizeCtrl.Enabled := false;
end;
//------------------------------------------------------------------------------

procedure TMainForm.Exit1Click(Sender: TObject);
begin
  close;
end;
//------------------------------------------------------------------------------

procedure TMainForm.EnableSizeControl1Click(Sender: TObject);
begin
  cbSizeMove.Checked := not cbSizeMove.Checked;
end;
//------------------------------------------------------------------------------

procedure TMainForm.cbSizeMoveClick(Sender: TObject);
begin
  SizeCtrl.Enabled := cbSizeMove.Checked;
  EnableSizeControl1.Checked := cbSizeMove.Checked;

  //Now, just in case the visible controls have changed (ie a new Tabsheet was
  //made visible) while SizeCtrl was disabled ...

  ActiveControl := nil;
  invalidate; //fixup grid painting on the form
end;
procedure TMainForm.CheckBox2Click(Sender: TObject);
begin
  SizeCtrl.ShowGrid := TCheckBox(Sender).Checked;
end;

procedure TMainForm.CheckBox3Click(Sender: TObject);
begin
   SizeCtrl.MoveOnly := TCheckBox(Sender).Checked;
end;

procedure TMainForm.CheckBox4Click(Sender: TObject);
begin
  SizeCtrl.MultiTargetResize := TCheckBox(Sender).Checked;
end;

procedure TMainForm.CheckBox6Click(Sender: TObject);
begin
  if TCheckBox(Sender).Checked then
     SizeCtrl.CarryFrame := TCarryFrameType.Show
  else
    SizeCtrl.CarryFrame := TCarryFrameType.Hide;
end;

procedure TMainForm.CheckBox7Click(Sender: TObject);
begin
  SizeCtrl.ApplySizes := TCheckBox(Sender).Checked;
end;

procedure TMainForm.CheckBox8Click(Sender: TObject);
begin
  SizeCtrl.AlignToGrid := TCheckBox(Sender).Checked;
end;

procedure TMainForm.ComboBox1Change(Sender: TObject);
begin
  SizeCtrl.BtnCount := TSizeCtrlBtnCount( TCombobox(Sender).ItemIndex );
end;

procedure TMainForm.ComboBox2Change(Sender: TObject);
begin
  SizeCtrl.BtnShape := TSizeBtnShapeType( TCombobox(Sender).ItemIndex );
end;

procedure TMainForm.ComboBox4Change(Sender: TObject);
begin
  SizeCtrl.ResizeFrameType := TReSizeFrameType( TCombobox(Sender).ItemIndex );
end;

procedure TMainForm.ComboBox5Change(Sender: TObject);
begin
  SizeCtrl.movePanelCanvas.Pen.Style :=  TPenStyle( TCombobox(Sender).ItemIndex);
end;

procedure TMainForm.ComboBox6Change(Sender: TObject);
begin
  sizeCtrl.movePanelCanvas.Pen.Mode := TPenMode( TCombobox(Sender).ItemIndex );
end;

procedure TMainForm.ComboBox7Change(Sender: TObject);
begin
  SizeCtrl.movePanelCanvas.Brush.Style := TBrushStyle( TCombobox(Sender).ItemIndex );
end;

procedure TMainForm.ComboBox8Change(Sender: TObject);
begin
  SizeCtrl.ResizeIgnoreMethod := TReSizeHideType( TCombobox(Sender).ItemIndex );
end;

//------------------------------------------------------------------------------

procedure TMainForm.FormResize(Sender: TObject);
begin
  //if SizeCtrl has targets selected, and they are moved or resized
  //independently of SizeCtrl, then SizeCtrl must be 'updated' ...
  SizeCtrl.Update;
end;

procedure TMainForm.Label26MouseEnter(Sender: TObject);
var c: TControlCanvas;
begin
    c := TControlCanvas.Create;
    c.Control := Panel3;
    if sets.Visible then
    begin
      Label26.Caption := 'p';
      Label26.Left := (Panel3.Width div 2) + (c.TextWidth(pExpanded) div 2) + 3;
    end
    else
    begin
      Label26.Caption := 'q';
      Label26.Left := (Panel3.Width div 2) + (c.TextWidth(pCollapsed) div 2) + 3;
    end;
    FreeAndNil(c);
end;

procedure TMainForm.Label26MouseLeave(Sender: TObject);
var c: TControlCanvas;
begin
    c := TControlCanvas.Create;
    c.Control := Panel3;
    if sets.Visible then
    begin
      Label26.Caption := 'r';
      Label26.Left := (Panel3.Width div 2) + (c.TextWidth(pExpanded) div 2) + 3;
    end
    else
    begin
      Label26.Caption := 's';
      Label26.Left := (Panel3.Width div 2) + (c.TextWidth(pCollapsed) div 2) + 3;
    end;
    FreeAndNil(c);
end;

//------------------------------------------------------------------------------
// Pretty much everything below demonstrates optional features .
//------------------------------------------------------------------------------


//Button1Click() demonstrates that OnClick events of SizeCtrl 'registered'
//controls are disabled when SizeCtrl is enabled.

//(nb: This doesn't work in Delphi 3 so OnClick events would have to be
//blocked manually to prevent Alt+Key shortcuts responding).
procedure TMainForm.BitBtn10Click(Sender: TObject);
begin
  SizeCtrl.movePanelCanvas.Brush.Color := selectColor( SizeCtrl.movePanelCanvas.Brush.Color  );
  ComboBox7.ItemIndex := integer( SizeCtrl.movePanelCanvas.Brush.Style );
end;

procedure TMainForm.BitBtn11Click(Sender: TObject);
begin
   ImSelect.Execute( SizeCtrl.HoverBtnImage );
end;

procedure TMainForm.BitBtn12Click(Sender: TObject);
begin
  SizeCtrl.HoverBtnFrameColor := selectColor( SizeCtrl.HoverBtnFrameColor );
end;

procedure TMainForm.BitBtn13Click(Sender: TObject);
begin
  SizeCtrl.HoverBtnColor := selectColor( SizeCtrl.HoverBtnColor );
end;

procedure TMainForm.BitBtn1Click(Sender: TObject);
begin
  SizeCtrl.BtnColor := selectColor( SizeCtrl.BtnColor );
end;

procedure TMainForm.BitBtn2Click(Sender: TObject);
begin
  SizeCtrl.BtnFrameColor := selectColor( SizeCtrl.BtnFrameColor );
end;

procedure TMainForm.BitBtn3Click(Sender: TObject);
begin
  SizeCtrl.DisabledBtnColor := selectColor(SizeCtrl.DisabledBtnColor);
end;

procedure TMainForm.BitBtn4Click(Sender: TObject);
begin
  SizeCtrl.DisabledBtnFrameColor := selectColor( SizeCtrl.DisabledBtnFrameColor   );
end;

procedure TMainForm.BitBtn5Click(Sender: TObject);
begin
  ImSelect.Execute( SizeCtrl.BtnImage );
end;

procedure TMainForm.BitBtn6Click(Sender: TObject);
begin
  ImSelect.Execute( SizeCtrl.DisabledBtnImage );
end;

procedure TMainForm.BitBtn7Click(Sender: TObject);
begin
  SizeCtrl.GridColor := selectColor( SizeCtrl.GridColor );
end;

procedure TMainForm.BitBtn8Click(Sender: TObject);
begin
  SizeCtrl.GridColorContrast := selectColor( SizeCtrl.GridColorContrast );
end;

procedure TMainForm.BitBtn9Click(Sender: TObject);
begin
  SizeCtrl.movePanelCanvas.Pen.Color := selectColor( SizeCtrl.movePanelCanvas.Pen.Color  );
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  ShowMessage('Button1 pressed.');
end;
//------------------------------------------------------------------------------

//Paints a grid on the form (if GRIDSIZE > 1) ...
procedure TMainForm.FormPaint(Sender: TObject);
var
  i,j: integer;
begin
  if (GRIDSIZE > 1) and SizeCtrl.Enabled then
    for i := 0 to width div GRIDSIZE do
      for j := 0 to height div GRIDSIZE do
        canvas.Pixels[i*GRIDSIZE, j*GRIDSIZE] := clGray;
end;

//------------------------------------------------------------------------------
// Give some basic feedback as to Size/Move changes ...
// (nb: While this demo only displays one target's properties, there may be
// any number of targets.)
//------------------------------------------------------------------------------

//1. Whenever a target changes ...
procedure TMainForm.SizeCtrlTargetChange(Sender: TObject);
begin
  if SizeCtrl.TargetCount = 0 then
    StatusBar1.SimpleText := ''
  else with SizeCtrl.Targets[0] do StatusBar1.SimpleText :=
    format('  %s -  left:%d  top:%d, width:%d  height:%d',
      [Name,left,top,width,height]);
end;
procedure TMainForm.SpinEdit1Change(Sender: TObject);
begin
    SizeCtrl.BtnSize := TSpinEdit(Sender).Value;
end;

procedure TMainForm.SpinEdit2Change(Sender: TObject);
begin
  {$IFDEF VER3UP}
  SizeCtrl.BtnAlphaBlend := TSpinEdit(Sender).Value;
  {$ENDIF}
end;

procedure TMainForm.SpinEdit3Change(Sender: TObject);
begin
  SizeCtrl.GridSize := TSpinEdit(Sender).Value;
end;

procedure TMainForm.SpinEdit4Change(Sender: TObject);
begin
  SizeCtrl.Constraints.MinHeight := TSpinEdit(Sender).Value;;
end;

procedure TMainForm.SpinEdit5Change(Sender: TObject);
begin
  SizeCtrl.Constraints.MinWidth := TSpinEdit(Sender).Value;
end;

procedure TMainForm.SpinEdit6Change(Sender: TObject);
begin
  SizeCtrl.Constraints.MaxWidth := TSpinEdit(Sender).Value;
end;

procedure TMainForm.SpinEdit7Change(Sender: TObject);
begin
  SizeCtrl.Constraints.MaxHeight := TSpinEdit(Sender).Value;
end;

//------------------------------------------------------------------------------

//2. During target resizing or moving ...
procedure TMainForm.SizeCtrlDuring(Sender: TObject; dx,dy: integer; State: TSCState);
var C: TControl;
begin
  C := SizeCtrl.Targets[0];
  if not Assigned(C) then Exit;
    if State = scsMoving then
      StatusBar1.SimpleText := format('  %s -  left:%d  top:%d, width:%d  height:%d',
        [C.Name, C.Left+dx, C.Top+dy, C.Width, C.Height])
    else {State = scsSizing}
      StatusBar1.SimpleText := format('  %s -  left:%d  top:%d, width:%d  height:%d',
        [C.Name, C.Left, C.top, C.Width+dx, C.Height+dy]);
end;
//------------------------------------------------------------------------------

//3. Once target resizing or moving has finished ...
procedure TMainForm.SizeCtrlEnd(Sender: TObject; State: TSCState);
begin
  with SizeCtrl do
    if TargetCount = 0 then StatusBar1.SimpleText := ''
    else with Targets[0] do StatusBar1.SimpleText :=
      format('  %s -  left:%d  top:%d, width:%d  height:%d',
        [Name,left,top,width,height]);
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

//The TPageControl.IndexOfTabAt() method is not available in older Delphi
//compilers. Therefore, I've included the following function
//so this demo works all the way back to Delphi 3 ...
{$IFNDEF VER3UP}
function My_IndexOfTabAt(PageControl: TPageControl; X, Y: Integer): Integer;
var
  HitTest: TTCHitTestInfo;
begin
  Result := -1;
  if PtInRect(PageControl.ClientRect, Point(X, Y)) then
    with HitTest do
    begin
      pt.X := X;
      pt.Y := Y;
{$WARNINGS OFF}
      Result := SendMessage(PageControl.Handle, TCM_HITTEST, 0, LPARAM(@HitTest));
{$WARNINGS ON}
    end;
end;
{$ENDIF}

//------------------------------------------------------------------------------

procedure TMainForm.SizeCtrlMouseDown(Sender: TObject;
  Target: TControl; TargetPt: TPoint; var handled: boolean);
var
  i: integer;
begin
  //When clicking the PageControl, it's kind of nice to be able to change pages.
  //So, let's see if a new page needs to be displayed ...
  if (Target is TPageControl) then
    with TPageControl(Target) do
    begin

      //We need the PageIndex of the tab being clicked. The following line
      //is fine in Delphi 7 but isn't available with older compilers ...
      //    with TargetPt do i := PageControl1.IndexOfTabAt(X, Y);
      //Therefore, this is my workaround which works back to Delphi 3 ...
      {$IFDEF VER3UP}
      with TargetPt do i := TPageControl(Target).IndexOfTabAt(X, Y);
      {$ELSE}
      with TargetPt do i := My_IndexOfTabAt(TPageControl(Target), X, Y);
      {$ENDIF}

      if (i >= 0) and ( ActivePage.PageIndex <> i) then
      begin
        //since further mouse handling stuff is not required ...
        handled := true;
        //Unregister from SizeCtrl all controls on the current page ...
        UnregComponents(PageControl1, SizeCtrl);
        //select the new page ...
        ActivePage := Pages[i];
        //finally, register controls on the new page...
        RegComponents(PageControl1, SizeCtrl);
      end;
    end;
end;
//------------------------------------------------------------------------------

procedure TMainForm.SizeCtrlSetCursor(Sender: TObject;
  Target: TControl; TargetPt: TPoint; var handled: boolean);
var
  i: integer;
begin
  //when clicking the PageControl, it's kind of nice to show an appropriate
  //cursor if we're clicking a new tab ...
  if (Target is TPageControl) then
    with TPageControl(Target) do
    begin
      {$IFDEF VER3UP}
      with TargetPt do i := TPageControl(Target).IndexOfTabAt(X, Y);
      {$ELSE}
      with TargetPt do i := My_IndexOfTabAt(TPageControl(Target), X, Y);
      {$ENDIF}
      if (i >= 0) and (ActivePage.PageIndex <> i) then
      begin
        //OK, we'll manage things from here ...
        handled := true;
        //assign the cursor directly ...
        {$IFDEF FPC}
        SetCursor(crDefault);
        {$ELSE}
        windows.SetCursor(screen.Cursors[crDefault]);
        {$ENDIF}
      end;
    end;
end;
//------------------------------------------------------------------------------

var
  popupMousePos: TPoint; //A workaround for older versions of Delphi (see below)

procedure TMainForm.Panel3Click(Sender: TObject);
var c: TControlCanvas;
begin
    c := TControlCanvas.Create;
    c.Control := Panel3;
    sets.Visible := not sets.Visible;
    if sets.Visible then
    begin
      Panel3.Caption := pExpanded;
      Label26.Left := (Panel3.Width div 2) + (c.TextWidth(pExpanded) div 2) + 3;
    end
    else
    begin
       Panel3.Caption := pCollapsed;
       Label26.Left := (Panel3.Width div 2) + (c.TextWidth(pCollapsed) div 2) + 3;
    end;
    FreeAndNil(c);
    SizeCtrl.Update;
end;

procedure TMainForm.Panel3Resize(Sender: TObject);
var c: TControlCanvas;
begin
    c := TControlCanvas.Create;
    c.Control := Panel3;
    if sets.Visible then
    begin
      Label26.Left := (Panel3.Width div 2) + (c.TextWidth(pExpanded) div 2) + 3;
    end
    else
    begin
      Label26.Left := (Panel3.Width div 2) + (c.TextWidth(pCollapsed) div 2) + 3;
    end;
    FreeAndNil(c);
end;

procedure TMainForm.sizePopsPopup(Sender: TObject);
begin
  GetCursorPos(popupMousePos);
end;
//------------------------------------------------------------------------------

procedure TMainForm.MenuItem1Click(Sender: TObject);
var
  ctrl: TControl;
begin
  //The following line doesn't compile with older versions of Delphi ...
  //    ctrl := SizeCtrl.TargetCtrlFromPt(PopupMenu2.PopupPoint);
  //because the TPopupMenu.PopupPoint method isn't defined.
  //Therefore, this demo uses a slightly more cumbersome route ...
  ctrl := SizeCtrl.TargetCtrlFromPt(popupMousePos);
  if not assigned(ctrl) then
    ShowMessage('oops!!!') else //should never happen!
    ShowMessage('You just clicked - '+ ctrl.Name);
end;
//------------------------------------------------------------------------------

procedure TMainForm.SizeCtrlKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key > VK_HELP then beep;
end;
//------------------------------------------------------------------------------

end.
