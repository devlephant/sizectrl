unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, SizeControl, ExtCtrls, ComCtrls, StdCtrls, Menus, CommCtrl,
  Vcl.Buttons,
  imageselect,
  Vcl.Samples.Spin;

type
  TForm1 = class(TForm)
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
    ComboBox3: TComboBox;
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
    procedure Label29Click(Sender: TObject);
    procedure BitBtn11Click(Sender: TObject);
    procedure BitBtn13Click(Sender: TObject);
    procedure BitBtn12Click(Sender: TObject);
  private
    SizeCtrl: TSizeCtrl;
    procedure SizeCtrlDuring(Sender: TObject; dx, dy: integer; State: TSCState);
    procedure SizeCtrlEnd(Sender: TObject; State: TSCState);
    procedure SizeCtrlTargetChange(Sender: TObject);
    procedure SizeCtrlMouseDown(Sender: TObject;
      Target: TControl; TargetPt: TPoint; var handled: boolean);
    procedure SizeCtrlSetCursor(Sender: TObject;
      Target: TControl; TargetPt: TPoint; var handled: boolean);
    procedure SizeCtrlKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  protected
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  pv: boolean = true;
  GRIDSIZE: integer = 8; //try changing this too.
  //PanelSizeCaption
const
  pCollapsed: string = 'Show';
  pExpanded: string = 'Hide';
implementation

{$R *.dfm}

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
   Form1.ColorDialog1.Color := aColor;
   if Form1.ColorDialog1.Execute then
    Result := Form1.ColorDialog1.Color
   else Result := aColor;
end;

procedure TForm1.FormCreate(Sender: TObject);
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
//------------------------------------------------------------------------------

procedure TForm1.FormClick(Sender: TObject);
begin
  SizeCtrl.UnRegisterAll;
  RegComponents(TWinControl(Sender), SizeCtrl);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  //it's important to disable SizeCtrl before destroying the form
  //because any SizeCtrl registered controls needs to be 'unhooked' ...
  SizeCtrl.Enabled := false;
end;
//------------------------------------------------------------------------------

procedure TForm1.Exit1Click(Sender: TObject);
begin
  close;
end;
//------------------------------------------------------------------------------

procedure TForm1.EnableSizeControl1Click(Sender: TObject);
begin
  cbSizeMove.Checked := not cbSizeMove.Checked;
end;
//------------------------------------------------------------------------------

procedure TForm1.cbSizeMoveClick(Sender: TObject);
begin
  SizeCtrl.Enabled := cbSizeMove.Checked;
  EnableSizeControl1.Checked := cbSizeMove.Checked;

  //Now, just in case the visible controls have changed (ie a new Tabsheet was
  //made visible) while SizeCtrl was disabled ...

  ActiveControl := nil;
  invalidate; //fixup grid painting on the form
end;
procedure TForm1.CheckBox2Click(Sender: TObject);
begin
  SizeCtrl.ShowGrid := TCheckBox(Sender).Checked;
end;

procedure TForm1.CheckBox3Click(Sender: TObject);
begin
   SizeCtrl.MoveOnly := TCheckBox(Sender).Checked;
end;

procedure TForm1.CheckBox4Click(Sender: TObject);
begin
  SizeCtrl.MultiTargetResize := TCheckBox(Sender).Checked;
end;

procedure TForm1.CheckBox6Click(Sender: TObject);
begin
  SizeCtrl.ShowFrame := TCheckBox(Sender).Checked;
end;

procedure TForm1.CheckBox7Click(Sender: TObject);
begin
  SizeCtrl.ApplySizes := TCheckBox(Sender).Checked;
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  SizeCtrl.BtnCount := TSizeCtrlBtnCount( TCombobox(Sender).ItemIndex );
end;

procedure TForm1.ComboBox2Change(Sender: TObject);
begin
  SizeCtrl.BtnShape := TSizeBtnShapeType( TCombobox(Sender).ItemIndex );
end;

procedure TForm1.ComboBox4Change(Sender: TObject);
begin
  SizeCtrl.ResizeFrameType := TReSizeFrameType( TCombobox(Sender).ItemIndex );
end;

procedure TForm1.ComboBox5Change(Sender: TObject);
begin
  SizeCtrl.movePanelCanvas.Pen.Style :=  TPenStyle( TCombobox(Sender).ItemIndex);
end;

procedure TForm1.ComboBox6Change(Sender: TObject);
begin
  sizeCtrl.movePanelCanvas.Pen.Mode := TPenMode( TCombobox(Sender).ItemIndex );
end;

procedure TForm1.ComboBox7Change(Sender: TObject);
begin
  SizeCtrl.movePanelCanvas.Brush.Style := TBrushStyle( TCombobox(Sender).ItemIndex );
end;

procedure TForm1.ComboBox8Change(Sender: TObject);
begin
  SizeCtrl.ResizeIgnoreMethod := TReSizeHideType( TCombobox(Sender).ItemIndex );
end;

//------------------------------------------------------------------------------

procedure TForm1.FormResize(Sender: TObject);
begin
  //if SizeCtrl has targets selected, and they are moved or resized
  //independently of SizeCtrl, then SizeCtrl must be 'updated' ...
  SizeCtrl.Update;
end;

procedure TForm1.Label26MouseEnter(Sender: TObject);
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

procedure TForm1.Label26MouseLeave(Sender: TObject);
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

procedure TForm1.Label29Click(Sender: TObject);
begin

end;

//------------------------------------------------------------------------------
// Pretty much everything below demonstrates optional features .
//------------------------------------------------------------------------------


//Button1Click() demonstrates that OnClick events of SizeCtrl 'registered'
//controls are disabled when SizeCtrl is enabled.

//(nb: This doesn't work in Delphi 3 so OnClick events would have to be
//blocked manually to prevent Alt+Key shortcuts responding).
procedure TForm1.BitBtn10Click(Sender: TObject);
begin
  SizeCtrl.movePanelCanvas.Brush.Color := selectColor( SizeCtrl.movePanelCanvas.Brush.Color  );
  ComboBox7.ItemIndex := integer( SizeCtrl.movePanelCanvas.Brush.Style );
end;

procedure TForm1.BitBtn11Click(Sender: TObject);
begin
   Form2.Execute( SizeCtrl.HoverBtnImage );
end;

procedure TForm1.BitBtn12Click(Sender: TObject);
begin
  SizeCtrl.HoverBtnFrameColor := selectColor( SizeCtrl.HoverBtnFrameColor );
end;

procedure TForm1.BitBtn13Click(Sender: TObject);
begin
  SizeCtrl.HoverBtnColor := selectColor( SizeCtrl.HoverBtnColor );
end;

procedure TForm1.BitBtn1Click(Sender: TObject);
begin
  SizeCtrl.BtnColor := selectColor( SizeCtrl.BtnColor );
end;

procedure TForm1.BitBtn2Click(Sender: TObject);
begin
  SizeCtrl.BtnFrameColor := selectColor( SizeCtrl.BtnFrameColor );
end;

procedure TForm1.BitBtn3Click(Sender: TObject);
begin
  SizeCtrl.DisabledBtnColor := selectColor(SizeCtrl.DisabledBtnColor);
end;

procedure TForm1.BitBtn4Click(Sender: TObject);
begin
  SizeCtrl.DisabledBtnFrameColor := selectColor( SizeCtrl.DisabledBtnFrameColor   );
end;

procedure TForm1.BitBtn5Click(Sender: TObject);
begin
  Form2.Execute( SizeCtrl.BtnImage );
end;

procedure TForm1.BitBtn6Click(Sender: TObject);
begin
  Form2.Execute( SizeCtrl.DisabledBtnImage );
end;

procedure TForm1.BitBtn7Click(Sender: TObject);
begin
  SizeCtrl.GridColor := selectColor( SizeCtrl.GridColor );
end;

procedure TForm1.BitBtn8Click(Sender: TObject);
begin
  SizeCtrl.GridColorContrast := selectColor( SizeCtrl.GridColorContrast );
end;

procedure TForm1.BitBtn9Click(Sender: TObject);
begin
  SizeCtrl.movePanelCanvas.Pen.Color := selectColor( SizeCtrl.movePanelCanvas.Pen.Color  );
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  ShowMessage('Button1 pressed.');
end;
//------------------------------------------------------------------------------

//Paints a grid on the form (if GRIDSIZE > 1) ...
procedure TForm1.FormPaint(Sender: TObject);
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
procedure TForm1.SizeCtrlTargetChange(Sender: TObject);
begin
  if SizeCtrl.TargetCount = 0 then
    StatusBar1.SimpleText := ''
  else with SizeCtrl.Targets[0] do StatusBar1.SimpleText :=
    format('  %s -  left:%d  top:%d, width:%d  height:%d',
      [Name,left,top,width,height]);
end;
procedure TForm1.SpinEdit1Change(Sender: TObject);
begin
    SizeCtrl.BtnSize := TSpinEdit(Sender).Value;
end;

procedure TForm1.SpinEdit2Change(Sender: TObject);
begin
  SizeCtrl.BtnAlphaBlend := TSpinEdit(Sender).Value;
end;

procedure TForm1.SpinEdit3Change(Sender: TObject);
begin
  SizeCtrl.GridSize := TSpinEdit(Sender).Value;
end;

procedure TForm1.SpinEdit4Change(Sender: TObject);
begin
  SizeCtrl.Constraints.MinHeight := TSpinEdit(Sender).Value;;
end;

procedure TForm1.SpinEdit5Change(Sender: TObject);
begin
  SizeCtrl.Constraints.MinWidth := TSpinEdit(Sender).Value;
end;

procedure TForm1.SpinEdit6Change(Sender: TObject);
begin
  SizeCtrl.Constraints.MaxWidth := TSpinEdit(Sender).Value;
end;

procedure TForm1.SpinEdit7Change(Sender: TObject);
begin
  SizeCtrl.Constraints.MaxHeight := TSpinEdit(Sender).Value;
end;

//------------------------------------------------------------------------------

//2. During target resizing or moving ...
procedure TForm1.SizeCtrlDuring(Sender: TObject; dx,dy: integer; State: TSCState);
begin
  with SizeCtrl.Targets[0] do
    if State = scsMoving then
      StatusBar1.SimpleText := format('  %s -  left:%d  top:%d, width:%d  height:%d',
        [Name, left+dx, top+dy, width, height])
    else {State = scsSizing}
      StatusBar1.SimpleText := format('  %s -  left:%d  top:%d, width:%d  height:%d',
        [Name,left, top, width+dx, height+dy]);
end;
//------------------------------------------------------------------------------

//3. Once target resizing or moving has finished ...
procedure TForm1.SizeCtrlEnd(Sender: TObject; State: TSCState);
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

//------------------------------------------------------------------------------

procedure TForm1.SizeCtrlMouseDown(Sender: TObject;
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
      with TargetPt do i := My_IndexOfTabAt(TPageControl(Target), X, Y);

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

procedure TForm1.SizeCtrlSetCursor(Sender: TObject;
  Target: TControl; TargetPt: TPoint; var handled: boolean);
var
  i: integer;
begin
  //when clicking the PageControl, it's kind of nice to show an appropriate
  //cursor if we're clicking a new tab ...
  if (Target is TPageControl) then
    with TPageControl(Target) do
    begin
      with TargetPt do i := My_IndexOfTabAt(TPageControl(Target), X, Y);
      if (i >= 0) and (ActivePage.PageIndex <> i) then
      begin
        //OK, we'll manage things from here ...
        handled := true;
        //assign the cursor directly ...
        windows.SetCursor(screen.Cursors[crDefault]);
      end;
    end;
end;
//------------------------------------------------------------------------------

var
  popupMousePos: TPoint; //A workaround for older versions of Delphi (see below)

procedure TForm1.Panel3Click(Sender: TObject);
begin
    sets.Visible := not sets.Visible;
    if sets.Visible then
      Panel3.Caption := pExpanded
    else
       Panel3.Caption := pCollapsed;
    SizeCtrl.Update;
end;

procedure TForm1.Panel3Resize(Sender: TObject);
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

procedure TForm1.sizePopsPopup(Sender: TObject);
begin
  GetCursorPos(popupMousePos);
end;
//------------------------------------------------------------------------------

procedure TForm1.MenuItem1Click(Sender: TObject);
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

procedure TForm1.SizeCtrlKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key > VK_HELP then beep;
end;
//------------------------------------------------------------------------------

end.
